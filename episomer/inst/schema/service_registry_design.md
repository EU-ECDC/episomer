# Enregistrement et surveillance des services en architecture multi-instances

Proposition de conception — document compagnon de `multi_instances.md`. Rien n'est implémenté à ce stade.

---

## 0. Petit glossaire (à lire en premier)

Cinq notions suffisent pour comprendre tout le document. Elles viennent du monde des systèmes
distribués, mais aucune n'est compliquée.

| Terme | En clair |
|---|---|
| **Bail** (*lease*) | Un droit **temporaire** et qui **expire tout seul**. Au lieu de dire « je suis le service search » une fois pour toutes, un service **emprunte** ce rôle pour 60 secondes et doit le renouveler. S'il meurt, personne n'a besoin de nettoyer : le bail expire. |
| **TTL** (*time to live*) | La durée de validité du bail. Ici : 60 secondes. |
| **Heartbeat** (signal de vie) | Le petit message « je suis toujours là » qu'un service envoie régulièrement (ici toutes les 15 s) pour renouveler son bail. |
| **Exclusion mutuelle** | La garantie qu'**un seul** processus à la fois fait une chose donnée (par ex. écrire dans les index Lucene). C'est ce que tes fichiers PID assurent aujourd'hui sur une seule machine. |
| **Condition de course** (*race condition*) | Un bug qui n'apparaît que quand deux processus agissent **exactement en même temps**. Typiquement : les deux lisent « la place est libre », donc les deux la prennent. |
| **Partition réseau** | Le réseau se coupe entre deux machines. Chacune est vivante et fonctionne, mais elles ne peuvent plus se parler — et donc chacune peut croire à tort que l'autre est morte. |

Le fil rouge du document : **une machine ne peut pas vérifier directement si un processus est
vivant sur une autre machine.** Toute la conception consiste à remplacer cette question
impossible par une question à laquelle on peut répondre à distance.

---

## 1. Ta question d'abord : search_loop / detect_loop ont-ils un accès en écriture à la machine où tourne fs ?

**Pas à son disque — mais oui à ses données, via l'API de fs.** Cette distinction est en réalité
toute la réponse au problème de conception.

Aujourd'hui, les services distants peuvent déjà **écrire** dans `EPI_HOME` sur la machine fs,
parce que `API.scala` expose une route `POST /file/...` avec `action=upload|touch|createDir|copy`
(côté R : `epifile.upload()`, `epifile.touch()`, `epidir.create()` dans `fs.R`). Ce qu'ils n'ont
**pas**, c'est un disque monté. Dans `docker/compose.yml`, les conteneurs `search` et `detect` ne
montent que `${EPI_HOME}/jobs` — et dans un vrai déploiement multi-machines, ils ne monteraient
rien du tout.

Donc le fichier PID écrit aujourd'hui par `register_runner()` atterrit dans le
`/episomer-data/search.PID` **du conteneur lui-même**, que personne d'autre ne voit.

Mais même si on faisait passer ce fichier par l'API de fs pour que tous les services partagent le
même, **les fichiers PID resteraient le mauvais outil**. Trois raisons :

| Problème | Pourquoi |
|---|---|
| Un PID n'est pas unique globalement | Le PID 42 dans le conteneur search et le PID 42 sur la machine fs sont deux processus qui n'ont rien à voir. `search.PID` contenant `42` ne veut rien dire pour celui qui le lit. |
| On ne peut pas vérifier la vitalité à distance | `get_running_task_pid()` lance `ps -cax \| grep <pid>` **en local**. L'app shiny sur la machine A ne peut pas faire un `ps` sur un processus de la machine B. |
| La prise du rôle n'est pas atomique | `register_runner()` fait « je lis, puis j'écris ». Deux services qui démarrent au même instant lisent tous les deux « c'est libre » et écrivent tous les deux. C'était acceptable avec une seule machine et un seul disque ; c'est une vraie condition de course à travers un réseau. |

La question « le processus 42 est-il vivant ? » doit donc être remplacée par une question à
laquelle un tiers distant **peut** répondre : **« ce service a-t-il donné signe de vie
récemment ? »**

---

## 2. L'idée centrale : un bail renouvelé par des heartbeats

Une seule idée, et tout le reste en découle.

Un service ne déclare pas « je tourne » une fois en laissant un fichier derrière lui. Il
**emprunte le droit d'être `search`** auprès de fs, pour une durée limitée, et doit renouveler cet
emprunt en permanence.

L'analogie : ce n'est pas une plaque vissée sur une porte, c'est un badge qu'il faut retamponner
toutes les 15 secondes. Si le porteur meurt, personne n'a besoin d'aller décrocher quoi que ce
soit — le badge se périme tout seul et la place redevient libre.

```
search démarre  ──"je prends search"──▶  fs : personne ne l'a → accordé, jeton=a1b2, ttl=60s
search tourne   ──heartbeat a1b2─────▶  fs : bail renouvelé, expire dans 60s
search tourne   ──heartbeat a1b2─────▶  fs : bail renouvelé
search tué -9
                (silence)
                                         fs : 60s sans heartbeat → bail expiré
                                              le statut de "search" devient PERDU (lost)
nouveau search  ──"je prends search"──▶  fs : c'est libre → accordé, jeton=c3d4
```

Ce mécanisme te donne **directement** les deux exigences que tu as écrites en gras dans
`multi_instances.md` :

- **« si une tâche s'arrête de façon inattendue, on doit le détecter, même si elle tourne sur une
  autre machine »**
  → plus de heartbeat pendant `ttl` secondes ⇒ statut `lost`. Ça marche pour un `kill -9`, un
  conteneur qui plante, une machine qui tombe, un réseau coupé. Sans `ps`, sans PID, sans disque
  partagé.
- **« on lève une erreur si le service tourne déjà ailleurs »**
  → l'enregistrement renvoie un `409 Conflict` avec les informations du détenteur actuel, et le
  code R appelle `stop()` exactement comme `register_runner()` le fait déjà aujourd'hui.

Un arrêt propre **rend** en plus le bail, ce qui permet un redémarrage immédiat au lieu d'attendre
la fin du TTL. Rendre le bail est un « bonus » qu'on fait de son mieux ; l'expiration est le
filet de sécurité qui, elle, ne peut pas échouer.

### Quatre états, au lieu de deux

Aujourd'hui un service tourne, ou ne tourne pas. Avec les baux on peut dire quelque chose de plus
utile, et l'app shiny a une vraie information à afficher :

| Statut | Signification | Comment fs le décide |
|---|---|---|
| `running` | en bonne santé | dernier heartbeat il y a moins de `ttl`, et la boucle avance |
| `stalled` | le processus est vivant mais la boucle n'avance plus | les heartbeats arrivent, mais `last_progress` est plus vieux que le cycle attendu du service |
| `lost` | mort sans prévenir — **c'est le cas qui doit déclencher une alerte** | aucun heartbeat depuis plus de `ttl` |
| `stopped` | a rendu son bail proprement | appel explicite de `release` |

L'état `stalled` mérite le petit effort supplémentaire : en production, la panne que tu vois
réellement n'est pas un crash, c'est une boucle de collecte **coincée** sur un appel à une API de
réseau social. Or un simple heartbeat ne sait pas distinguer ce cas d'une boucle en bonne santé.
Voir le §6.

---

## 3. Où vit le registre

**Dans le service fs, en mémoire.** `multi_instances.md` établit déjà que fs est le seul service
avec état (*stateful*) et le seul arbitre — le registre lui appartient donc naturellement, à côté
de la `MMap secrets` que `API.scala` garde déjà en mémoire.

En mémoire plutôt que dans un fichier, et c'est volontaire :

- Pas de problème d'écritures concurrentes, pas de verrouillage de fichier, pas d'entorse à la
  règle du projet « un seul service écrit un fichier donné ».
- Perdre le registre quand fs redémarre est le **comportement correct**, pas une perte de données.
  Si fs a disparu, plus rien n'est garanti et tout le monde doit se réannoncer. La reprise est
  automatique : un heartbeat portant un jeton que fs n'a jamais vu reçoit un `404 unknown`, et le
  client R se réenregistre simplement (avec succès, puisque le registre est vide). Au pire, les
  statuts sont vides pendant un intervalle de heartbeat.
- Ce point compte en pratique : `fs_loop()` relance `spark_job()` dans une boucle `while(TRUE)`,
  donc la JVM peut redémarrer sous les pieds de services qui, eux, continuent de tourner. La
  conception doit survivre à ça — celle-ci le fait.

Option utile : recopier le registre dans `EPI_HOME/runners.json` toutes les 30 s, **uniquement
pour pouvoir lire après coup** qui était debout au moment d'un incident. Mais ne jamais relire ce
fichier comme source de vérité.

---

## 4. Les endpoints à ajouter dans fs

Calqués sur le style de la route `pathPrefix("file")` existante.

```
POST /runner/{name}?action=register
     corps : {"host": "...", "pid": 123, "version": "4.0.5", "exclusive": true, "ttl": 60}
     200 → {"status":"granted", "token":"a1b2...", "ttl":60}
     409 → {"status":"conflict",
            "holder":{"host":"epi-2","pid":98,"since":"...","last_seen":"..."}}

POST /runner/{name}?action=heartbeat        en-tête : X-Episomer-Token: a1b2...
     corps : {"last_progress": "2026-08-26T10:03:00Z"}   # facultatif
     200 → {"status":"ok","ttl":60}
     409 → {"status":"lost","holder":{...}}   # quelqu'un d'autre l'a → l'appelant DOIT s'arrêter
     404 → {"status":"unknown"}               # fs a redémarré → l'appelant se réenregistre

POST /runner/{name}?action=release          en-tête : X-Episomer-Token: a1b2...
     200 → {"status":"stopped"}

GET  /runners
     200 → [{"name":"search.bluesky","status":"running","host":"epi-2","pid":98,
              "started_on":"...","last_seen":"...","last_progress":"...","exclusive":true}, ...]
```

Les noms restent **exactement** ceux des fichiers PID actuels — `fs`, `search`, `search.bluesky`,
`search.mastodon`, `detect`, plus `app` — pour que `missing_search_jobs()` continue de fonctionner
sans changer de forme.

Note d'implémentation : `register` et `heartbeat` ont besoin d'un « test-et-pose » indivisible
(lire *et* écrire sans que personne ne se glisse entre les deux), et pekko-http traite les
requêtes en parallèle. Un simple `object RunnerRegistry` contenant une `Map[String, Lease]` avec
des blocs `synchronized` suffit — on parle d'une poignée de services et d'un appel toutes les 15
secondes. Pas besoin d'acteur : c'est plus simple que `LuceneActor`, et ça suit le précédent de la
`MMap secrets`.

`/ping` reste tel quel. `is_fs_running()` est déjà la **seule** fonction de `runners.R` qui
fonctionne à distance, justement parce qu'elle pose sa question en HTTP — on la garde.

---

## 5. Le côté R

Nouvelles fonctions, dans `runners.R` ou dans un nouveau `runners-remote.R` :

```r
# état du processus courant
runner <- new.env()   # $name, $token, $ttl, $last_progress

register_runner(name, exclusive = TRUE, ttl = 60)  # 409 → stop() avec les infos du détenteur
heartbeat_runner(progress = FALSE)                 # progress = TRUE horodate last_progress
release_runner()                                   # appelée depuis on.exit()
get_runners(max_age = 3)                           # GET /runners, avec cache
```

Et les fonctions de lecture existantes deviennent de simples enveloppes autour de `get_runners()` :

```r
is_search_running()   <- un service "search" est présent avec statut "running"
is_detect_running()   <- idem pour "detect"
missing_search_jobs() <- active_social_media() moins les "search.<sm>" en running
```

Deux choses rendent ce chantier moins lourd qu'il n'y paraît :

- **Il n'y a pas deux chemins de code (local / distant) à maintenir.** `setup_config()` met déjà
  `fs_remote` à `http://localhost:8080` par défaut (ou `EPI_HOST`) : donc **tous** les clients R
  parlent déjà à fs en HTTP, y compris dans l'installation classique mono-machine. Le seul
  appelant avec `fs_remote = NULL` est `fs_loop()` lui-même — qui est justement le détenteur du
  registre, et s'enregistre donc en interne. Autrement dit : le même mécanisme sert les deux
  modes de déploiement, sans code spécifique.
- **L'app shiny interroge déjà en bloc.** Dans `shiny-app-utils.R`, `update_env(limit = "tasks")`
  appelle ensemble `is_detect_running()`, `is_fs_running()`, `is_search_running()` et
  `missing_search_jobs()` : les quatre se réduisent à **un seul** `GET /runners`. Il suffit de
  garder la réponse en cache ~3 secondes pour qu'une page avec plusieurs sorties réactives ne
  bombarde pas fs.

**Ordre au démarrage.** Attention à ne pas confondre « fs n'est pas encore démarré » (il faut
réessayer) et « ça tourne déjà ailleurs » (il faut s'arrêter). `search_loop_worker()` a déjà la
bonne structure (`while (!is_fs_running()) Sys.sleep(5)`) : on réessaie jusqu'à ce que fs réponde,
**puis** on s'enregistre, **puis** on échoue franchement sur un 409.

---

## 6. Le point délicat : les heartbeats face aux traitements longs

Où appeler concrètement `heartbeat_runner()` ? C'est la seule décision vraiment épineuse du
document, et elle mérite d'être expliquée parce que les deux solutions évidentes sont fausses.

- **Solution évidente n°1 : appeler le heartbeat au début de chaque tour de la boucle
  `while(TRUE)`.** Problème : une seule étape longue fait dépasser le TTL et le service est
  déclaré `lost` à tort. Les itérations de `detect_loop()` peuvent lancer une construction
  geonames ou une agrégation ; `search_loop_worker()` peut rester bloqué sur une API lente.
- **Solution évidente n°2 : envoyer les heartbeats depuis un processus d'arrière-plan.** Problème
  inverse : une boucle principale **coincée** continue de paraître en bonne santé. Pire, le
  processus d'arrière-plan peut survivre à son parent et envoyer des heartbeats pour un mort.

La solution combine les deux, en utilisant chacune là où elle est valable :

**(a) Un batteur en arrière-plan, qui vérifie son parent en local.** Un `future::future()` par
service, exactement le motif que `fs_loop()` utilise déjà pour `register_fs_monitor()`. Toutes les
15 s, il :

1. vérifie que le PID du parent est toujours vivant **sur sa propre machine** — et c'est ici que
   les vérifications de PID redeviennent parfaitement correctes, puisque le parent et l'enfant
   partagent la machine. Les PID ne sont dénués de sens qu'**entre** machines. On réutilise donc
   le test local existant de `get_running_task_pid()` ;
2. si le parent a disparu → `release_runner()` puis sortie. Plus de heartbeats orphelins ;
3. sinon → `POST heartbeat`. Si la réponse est `409 lost`, il tue le parent : quelqu'un d'autre
   détient le nom maintenant, et le parent ne doit surtout pas continuer à écrire (voir §8).

**(b) Un horodatage de progression émis par la boucle principale.** La boucle principale appelle
`heartbeat_runner(progress = TRUE)` une fois par cycle terminé — c'est peu coûteux, une ligne par
boucle, et ça n'a pas besoin d'être fréquent. fs compare `last_progress` à un seuil propre à
chaque service (search : `2 × collect_span` ; detect : `2 × schedule_span`) et signale `stalled`.

En résumé : **(a) répond à « le processus est-il là ? », (b) répond à « fait-il quelque chose ? »**.
Aucune des deux ne suffit seule, et ensemble elles ne demandent aucune mécanique nouvelle par
rapport à ce que le code fait déjà.

---

## 7. Ce qui reste en fichier PID

**Les processus locaux de fs : `fs`, `fs.java`, `fs_mon`.**

Deux instances de `fs_loop()` sur le même `EPI_HOME` corrompraient les index Lucene. Et aucun tiers
ne peut arbitrer ce cas : fs **est** l'arbitre, il ne peut donc demander à personne. L'exclusion
mutuelle locale doit rester locale, et un fichier PID plus un `ps` local est exactement le bon
outil pour ça : même machine, même disque, aucune ambiguïté.
`ProcessID.writePID(conf.pidPath("fs.java"))` dans `API.scala` et
`register_fs_runner()` / `register_fs_monitor()` continuent de fonctionner tels quels.

D'où une règle simple à retenir :

> **Les fichiers PID protègent contre une deuxième instance sur la même machine. Les baux
> protègent contre une deuxième instance n'importe où. Tout service susceptible d'être distant
> utilise un bail ; seuls les processus locaux de fs utilisent des fichiers PID.**

(À signaler à part : `API.removeLockFiles()` supprime les fichiers `write.lock` de Lucene au
démarrage, ce qui désactive silencieusement la protection propre de Lucene si un second fs était
lancé un jour. C'est préexistant et hors sujet ici, mais c'est la raison pour laquelle la
protection par PID de fs doit rester fiable.)

---

## 8. Mise en garde honnête : un bail n'est pas une exclusion mutuelle

À savoir **avant** de construire là-dessus, parce que c'est le piège classique de ce genre de
mécanisme.

Un bail garantit qu'il y a au plus un détenteur **au su de fs**. Il ne garantit **pas** qu'il y a
au plus un **écrivain**. Scénario de panne : le service `search` A est vivant mais isolé par une
partition réseau (ou bloqué dans une requête de 90 secondes) ; fs fait expirer son bail ; un
service `search` B s'enregistre et commence à écrire des posts ; puis la requête de A se termine
et A écrit aussi. Deux écrivains, brièvement — et l'invariant du projet (« un seul service écrit
un fichier donné ») est rompu.

Trois niveaux de protection, du moins cher au plus solide :

1. **Dimensionner le TTL généreusement.** Heartbeat toutes les 15 s, TTL de 60 s = quatre
   battements manqués avant éviction. Les hoquets passagers et les pauses de la JVM ne font jamais
   expirer un bail. C'est gratuit, et ça couvre les cas réalistes.
2. **Auto-arrêt du client (*self-fencing*).** Un `409 lost` sur un heartbeat signifie que
   l'appelant doit **s'arrêter lui-même** — `stop()`, et surtout ne pas continuer à écrire. Ce sont
   des boucles supervisées (`restart:` de docker, planificateur de tâches Windows), donc s'arrêter
   est sans danger : le service redémarre et, soit il obtient le bail, soit il attend. **C'est le
   point important**, et il ne coûte que quelques lignes.
3. **Contrôle côté serveur (la vraie solution, plus tard).** fs exige le jeton du détenteur sur
   les endpoints d'écriture et rejette les écritures d'un non-détenteur. C'est ce qui rend
   l'invariant étanche, mais ça touche tous les chemins d'écriture (`epifile.upload`,
   `POST /posts`, `POST /aggregate`, …).

Recommandation : construire **1 + 2** maintenant, et rendre le 3 possible plus tard **sans changer
le protocole**, en envoyant les en-têtes `X-Episomer-Runner` et `X-Episomer-Token` sur *toutes* les
requêtes vers fs dès le premier jour (un seul endroit à modifier : une enveloppe autour des appels
httr/httr2 dans `fs.R`). fs peut les ignorer au début et commencer à les contrôler quand tu le
décides. Cette compatibilité anticipée ne coûte presque rien maintenant et évite une migration
plus tard.

---

## 9. L'app shiny : exclusive ou pas ?

`multi_instances.md` dit que l'app suit le même circuit que les services. Elle devrait donc
s'enregistrer — tu veux la voir dans l'écran de supervision — mais je l'enregistrerais en
**non exclusif** (`exclusive = false`, nom `app`, une entrée par instance avec son propre
identifiant d'instance).

Pourquoi : faire tourner deux tableaux de bord est légitime et c'est la façon normale de monter en
charge ; lever une erreur sur le second serait une régression. L'objection est que l'app écrit
`properties.json` et `topics.xlsx`, donc deux administrateurs qui éditent en même temps peuvent
s'écraser mutuellement — mais c'est déjà vrai aujourd'hui avec deux onglets sur une seule app.
C'est donc un problème distinct, et interdire une seconde instance ne le règle pas.

**C'est la décision qui te revient**, et la seule de ce document que je ne veux pas prendre à ta
place. Faire de `exclusive` un paramètre par service ne coûte rien et laisse le choix réversible.

---

## 10. Solution de repli si le chantier Scala bloque

Si tu veux débloquer le déploiement multi-machines avant de toucher à `API.scala`, le **même
modèle de bail** peut tourner **uniquement sur les endpoints fichiers existants, sans une ligne de
Scala** :

- `register` → `epifile.upload()` d'un petit JSON `{instance_id, host, pid, since, last_progress}`
  vers `EPI_HOME/runners/<name>.json`, mais seulement après l'avoir lu et l'avoir trouvé absent ou
  périmé.
- `heartbeat` → réenvoyer le même JSON avec un horodatage frais (le chemin d'upload fait déjà un
  `Files.move` atomique, donc les lecteurs ne voient jamais un fichier à moitié écrit).
- vitalité → `epifile.mtime()` sur ce fichier ; plus vieux que le TTL ⇒ `lost`.
- `release` → écrire `{"status":"stopped"}` (il n'y a pas d'action de suppression sur `/file`).
- `GET /runners` → `epilist.files()` puis lecture de chaque fichier.

C'est pleinement multi-machines et ça se livre en quelques jours. La seule chose que ça ne sait pas
faire, c'est une prise de rôle **atomique** : la condition de course du §1 subsiste, mais réduite
à une fenêtre inférieure à la seconde. On l'atténue en écrivant son `instance_id`, en attendant
~2 s, puis en relisant : si ce n'est pas le sien, on s'arrête. Le « dernier qui écrit gagne »
devient détectable un battement plus tard, au lieu de ne jamais l'être.

La structure des fichiers est volontairement la même que celle des messages du §4 : c'est donc une
vraie étape intermédiaire, pas une impasse. Cela dit : le registre représente ~80 lignes de Scala,
le jar est de toute façon reconstruit à chaque déploiement
(`inst/java/ecdc-episomer-bundle_2.13-1.0.jar`), et l'atomicité est le travail du serveur.
**J'irais directement au §4**, sauf si c'est justement la compilation Scala qui te contraint.

---

## 11. Ordre de travail suggéré

1. **Scala** : `RunnerRegistry` + les quatre routes du §4. Autonome, testable au `curl`, ne casse
   rien.
2. **Lecture côté R** : `get_runners()`, puis réécriture de `is_*_running()` et
   `missing_search_jobs()` par-dessus. À ce stade l'app shiny affiche déjà des statuts corrects
   entre machines, alors que les services écrivent encore leurs fichiers PID — donc on fait
   tourner les deux mécanismes en parallèle pendant cette étape.
3. **Écriture côté R** : `register_runner()` / `heartbeat_runner()` / `release_runner()`, branchés
   dans `search_loop_worker()`, `detect_loop()` et `episomer_app()`, avec le batteur du §6. On
   abandonne les fichiers PID pour ces trois-là ; on les garde pour `fs`, `fs.java`, `fs_mon` (§7).
4. **Alertes** : `health_check()` remonte les services `lost` et `stalled`. Elle tourne déjà toutes
   les 60 s dans le `future` de surveillance de `fs_loop()` — et c'est désormais le même processus
   que celui qui détient le registre, donc elle n'a besoin d'aucun appel réseau pour savoir.
5. **Plus tard** : les en-têtes de jeton sur toutes les écritures vers fs, puis le contrôle côté
   serveur (§8.3).

Nouveaux paramètres de configuration, à côté de `fs_batch_timeout` / `fs_query_timeout` dans
`config.R` : `runner_heartbeat_span` (15 s), `runner_ttl` (60 s), `runner_stall_factor` (2).

---

## 12. Ce qui va coincer pendant la migration

- `stop_runner_task()` / `kill_task()` tuent un PID **local**. Entre machines, c'est impossible :
  arrêter un service distant doit devenir « lui demander de s'arrêter » (un drapeau
  `stop_requested` que fs renvoie dans la réponse au heartbeat, et que le service respecte), ou
  bien être laissé à l'orchestrateur (`docker compose stop`). Les boutons d'arrêt de l'app shiny
  demandent une décision ici — probablement : service local ⇒ on tue comme aujourd'hui ; service
  distant ⇒ on affiche la machine du détenteur et on indique qu'il doit être arrêté là-bas.
- `register_runner_task()` copie des scripts dans `conf$data_dir/jobs` et lance `Rscript`. C'est
  intrinsèquement local : un service distant ne peut être **démarré** que par ce qui gouverne sa
  machine. Le lancement reste hors périmètre ; seuls l'enregistrement et la surveillance
  deviennent distants. Le §13 détaille pourquoi et quelles sont les options.
- Les journaux des services vont aujourd'hui dans `conf$data_dir/jobs/*.log`, ce qui explique le
  montage de `${EPI_HOME}/jobs` dans les conteneurs search et detect de `compose.yml`. Ce montage
  partagé est le dernier morceau de couplage hors-fs ; il vaudra la peine de le supprimer un jour
  (envoyer les journaux via l'endpoint fichier, ou les laisser au runtime des conteneurs) pour que
  les services deviennent réellement sans état.

---

## 13. Mais alors, comment fs lance-t-il un service sur une autre machine ?

**Il ne le fait pas — et il ne doit pas.** C'est la question qui vient naturellement en lisant ce
document, elle mérite donc sa propre section.

Il faut séparer deux problèmes que l'on confond facilement :

| Question | Qui y répond | Statut |
|---|---|---|
| **Qui *tourne* en ce moment ?** (découverte, surveillance) | le registre de baux dans fs | c'est tout l'objet de ce document |
| **Qui *devrait* tourner ?** (orchestration, lancement) | ni fs, ni episomer | volontairement hors périmètre |

Le sens des flèches est le point clé de l'architecture : **les services appellent fs, jamais
l'inverse.** fs ne connaît même pas l'adresse de la machine où tourne search — il apprend son
existence au moment où elle se présente à lui. C'est précisément ce qui permet à un service d'être
sans état et déplaçable d'une machine à l'autre. Si fs devait lancer les services, il lui faudrait
connaître à l'avance la liste des machines, leurs adresses, leurs identifiants et leurs chemins
d'installation : on retomberait sur une configuration centrale à maintenir, c'est-à-dire l'inverse
de l'objectif.

### Ce qui se passe aujourd'hui

`register_runner_task()` est strictement local : `taskscheduleR` sur Windows,
`system2("Rscript", ...)` sur Linux. Les boutons « activer » de l'app shiny
(`shiny-app.R:2330` et `2393`) ne fonctionnent que parce que l'app tourne **sur la même machine**
que le service qu'elle démarre. Et dans `docker/compose.yml`, personne dans episomer ne lance
search : c'est `docker compose` qui démarre le conteneur, et `devscripts/run.R` lit
`EPISOMER_RUN=SEARCH` pour savoir quoi exécuter.

Autrement dit, le lancement à distance est déjà résolu — par l'orchestrateur, pas par episomer.

### Trois options, dont une à écarter

**Option 1 — l'orchestrateur lance. Recommandée, et c'est déjà ce qui est en place.**
Le service est démarré par docker compose, systemd ou k8s sur sa propre machine ; il attend que fs
réponde, puis s'enregistre. Personne ne « lance à la demande » : le service est simplement toujours
debout, et la politique de redémarrage de l'orchestrateur gère les plantages.

Conséquence concrète à assumer : en multi-machines, les boutons activer/arrêter de l'app shiny
n'ont plus de sens et doivent devenir un simple affichage de statut (cf. §12).

> **Point à corriger tout de suite :** `compose.yml` ne contient **aucune directive `restart:`**.
> Il y a bien `depends_on: condition: service_healthy`, mais si le conteneur search plante, docker
> ne le relance pas. Il faut ajouter `restart: unless-stopped` sur les trois services — sans ça,
> l'affirmation du §8 (« s'arrêter est sans danger puisque ce sont des boucles supervisées ») est
> tout simplement fausse, et l'auto-arrêt sur `409 lost` laisserait le service mort.

**Option 2 — un agent local qui *tire* l'état désiré.** À retenir seulement si tu veux conserver le
bouton « activer » de l'app shiny en multi-machines.

Un petit processus episomer tourne en permanence sur chaque machine et demande périodiquement à fs
« est-ce que je dois faire tourner search ? ». Selon la réponse, il démarre ou arrête le processus R
local. fs ne pousse rien : c'est l'agent qui tire l'information, donc le sens des flèches est
préservé.

C'est une extension naturelle et légère du registre : on ajoute simplement un champ `desired`
(`running` / `stopped`) par nom de service, que fs stocke et que l'app shiny modifie. **Le protocole
de bail et de heartbeat des §2 et §4 ne change pas d'une ligne.** C'est conceptuellement ce que fait
Kubernetes : l'état désiré est déclaré au centre, et un agent local se charge de réconcilier la
réalité avec lui.

Bénéfice secondaire : cela résout aussi le problème d'arrêt signalé au §12. Le bouton « arrêter »
écrit `desired = stopped`, et le service obéit en lisant la réponse à son propre heartbeat — plus
besoin de tuer un PID sur une machine distante, ce qui est de toute façon impossible.

**Option 3 — fs fait un `ssh machineB "Rscript ..."`. À écarter.**
Il faudrait que fs détienne les clés SSH de toutes les machines, leurs adresses, leurs chemins R.
Surtout, cela inverse la dépendance sur laquelle repose toute l'architecture et transforme fs en
service d'exécution de code à distance : mauvais choix en sécurité comme en conception.

### À retenir

> **Le lancement d'un service reste la responsabilité de ce qui gouverne sa machine.** Le registre
> dit qui est là et qui est mort ; il ne démarre rien. Si l'on veut piloter le démarrage depuis
> l'app shiny, c'est l'option 2 — et elle se greffe sur le registre sans le modifier.
