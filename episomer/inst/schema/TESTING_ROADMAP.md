# Feuille de route — tests unitaires `episomer`

État des lieux au 2026-08-25 : ~18 400 lignes dans `R/`, ~250 fonctions, 8 fichiers de test
(`tests/testthat/`). Ce document liste les cibles de test par ordre de rapport valeur/effort.

**Comment s'en servir avec Claude Code** : chaque cible a un identifiant stable (`T1-1`, `T3-2`…).
Il suffit de dire « on attaque T1-2 » ou « où on en est sur le Tier 2 ? » pour reprendre le fil
sans re-expliquer le contexte. Cocher les cases au fur et à mesure ; l'identifiant ne change
jamais, même si l'ordre est réarrangé.

---

## Conventions déjà en place (à réutiliser, ne pas réinventer)

| Idiome | Où | Pour quoi |
|---|---|---|
| `local_conf(...)` | `tests/testthat/helper-conf.R` | remplacer le contenu de l'environnement `conf` le temps d'un test, avec restauration automatique. Ne **pas** utiliser `withr::with_environment` : les lecteurs de config résolvent `conf` dans le namespace du package, pas chez l'appelant. |
| `local_mocked_bindings(..., .package = "episomer")` | `test-sm.R`, `test-shiny_app.R` | mocker une fonction interne (I/O, réseau, Java, keyring). |
| `withr::local_envvar()` / `local_timezone()` / `with_tempdir()` | `test-shiny_app.R` | isoler l'environnement (`JAVA_HOME`, TZ, répertoire de travail). |
| testthat 3e | `DESCRIPTION` (`Config/testthat/edition: 3`) | `expect_snapshot()`, `expect_snapshot_value()` disponibles. |

Fixtures JSON à venir : `tests/testthat/fixtures/`, lues via `testthat::test_path()`.

---

## Tier 1 — fonctions pures, aucun setup

Rien à mocker, rien à configurer. C'est là qu'il faut commencer.

- [x] **T1-1 · `ears_t_reweighted()`** — `R/alert.R:112` — **fait**
  (`tests/testthat/test-alert_ears.R`, 68 assertions, 1 snapshot)
  Rappel de contexte utile aux tests : sur les 7 colonnes retournées, la production n'en lit que
  trois (`R/alert.R:439`) — `alarm0_dw` (l'alarme **re-pondérée**), `U0_dw`, et `y0bar` (la
  baseline, elle **non** re-pondérée, affichée sous les courbes). `alarm0`/`U0` sont la référence
  EARS simple et ne sortent jamais du package ; l'écart entre les deux moitiés est donc l'objet
  du test, pas un détail d'implémentation.
  Couvert : forme du résultat (7 colonnes nommées, `time_monitored`/`y0` intacts dans les deux
  branches) ; lignes de chauffe `t < no_historic * step + 1` et déplacement du premier point
  monitoré par `no_historic` ; série plus courte que la baseline → tout `NA` sans erreur ;
  `y0bar` = moyenne simple des `no_historic` valeurs précédentes (le point monitoré exclu) ;
  `U0` recalculé indépendamment en base R (`mean + qt(1-alpha, n-1) * sd * sqrt(1+1/n)`) et non
  copié d'une exécution précédente ; `alarm0` = comparaison **stricte** ; série stable + un pic →
  alarme sur le pic seul et pas le lendemain ; **le cas d'usage du re-weighting** (même valeur
  monitorée, un seul jour d'épidémie ajouté dans la fenêtre : `U0` grimpe et le signal est perdu,
  `U0_dw` le rattrape) vérifié sur une série construite **et** sur la ligne 10 de l'exemple
  roxygen, seule ligne où les deux moitiés divergent ; historique sans outlier → `U0_dw`
  **identique** à `U0` ; `k_decay = 0` → re-weighting désactivé sur toute la série ; monotonies
  (`alpha` ↘ `U0`, `k_decay` ↘ `U0_dw` avec saturation, `alpha_outlier` ↘ `U0_dw`) ;
  `same_weekday_baseline = TRUE` → 49 lignes `NA` et baseline bâtie sur le seul même jour de
  semaine, vérifiée sur une série à forte saisonnalité hebdomadaire (le jour calme alarme contre
  son propre jour de semaine et reste invisible contre les 7 jours précédents) ;
  `expect_snapshot_value()` sur l'exemple roxygen pour verrouiller les valeurs.
  A produit 2 bugs → voir « Bugs trouvés » (B-9, B-10) ; 2 quirks gelés : les alarmes sortent en
  **numeric 0/1** et non en logique (`c()` coerce — inoffensif en aval, mais la branche vide de
  `R/alert.R:446` rend une colonne `logical()`, donc le type de `counts$alert` dépend de la
  présence de données) ; seuls les outliers **hauts** sont re-pondérés (`rstar > seuil`, sans
  `abs()`) — délibéré pour un détecteur d'épidémie, mais une panne de collecte (un jour à zéro au
  milieu d'une semaine normale) gonfle l'écart-type, relève les deux seuils et masque les jours
  suivants, sans rien pour le corriger.

- [x] **T1-2 · `plans.R` en entier** — `R/plans.R` — **fait** (`tests/testthat/test-plans.R`,
  116 assertions)
  Couvert : `parse_plan_attributes()` (types, `NULL` vs `{}`, défauts, routage sur la famille et
  non sur le nom complet du réseau), `format_plan()`, l'aller-retour **réel** de persistance
  (`format_plan` → `write_json(auto_unbox)` → `read_json` → `parse_plan_attributes`, y compris le
  curseur de pagination mastodon, la précision sub-seconde et le piège de l'`auto_unbox` sur un
  `seen_ids` de longueur 1), `merge_plans()`, les 3 branches de `update_plans_schedule()` + le
  plafond, `finish_plans()`, `update_plan_after_request()`.
  A produit 3 bugs → voir la section « Bugs trouvés » (B-1, B-2, B-3).
  Note de méthode : l'aller-retour `parse(format(x))` en mémoire **ne teste rien d'utile** — c'est
  `jsonlite` qui sérialise les dates communes, donc le seul aller-retour qui a du sens passe par
  le JSON.

- [x] **T1-3 · `translate_query()`** — `R/config.R:732` — **fait**
  (`tests/testthat/test-translate_query.R`, 42 assertions)
  Couvert : routage vers le traducteur du réseau (`sm_api_translate_query()` mocké, valeur de
  retour transmise telle quelle), `or`/`and` en minuscules et majuscules, opérateurs natifs
  `&`/`|` avec ou sans espaces, précédence du `and` sur le `or`, espaces multiples, terme unique,
  espaces internes d'un terme préservés, synonymes `/` (2 et 3 synonymes, trimés), négations
  (en tête, au milieu, plusieurs, doublons conservés), tiret intra-mot (`covid-19`) non pris pour
  une négation, portée **globale** des négations (un `-x` écrit dans une branche du `or` exclut
  le terme de toutes les branches), requête tout-négative → sous-requête vide (donc zéro requête
  native produite, topic jamais collecté).
  A produit 2 bugs + 3 quirks → voir la section « Bugs trouvés » (B-4, B-5) ; les 3 quirks gelés
  dans les tests sont : opérateur en fin de requête (`"flu or"`) lu comme partie du terme,
  `" - "` isolé qui négative le terme suivant **espace de tête inclus** (`" covid"`, `substr()`
  sans `trimws()`), et `"|-"` qui avale le `or` (`"a or -b"` ≡ `"a and -b"`).

- [x] **T1-4 · helpers Mastodon isolés** — `R/sm-mastodon-api.R` — **fait**
  (`tests/testthat/test-mastodon_utils.R`, 56 assertions)
  Couvert :
  - `mastodon_id_lt()` :444 — snowflakes 18 chiffres, comparaison **numérique et non
    lexicographique** (`"999999999999" < "1000000000000"` renvoie `FALSE` en character : c'est
    tout l'intérêt du helper), stricte sur l'égalité, vectorisée, `NA` → `NA`,
    `NULL`/`character(0)` → `logical(0)`
  - `mastodon_min_id()` :450 — plus petit id numérique, aplatissement d'une liste imbriquée,
    `NA` ignorés, `NULL` sur toute entrée inutilisable, retour en **character** (et pas en
    `integer64`, qui serait sérialisé en double dans le fichier de plans)
  - `mastodon_is_present()` :501 — le piège `{}` vs `null` testé sur un vrai
    `jsonlite::fromJSON('{"quote": {}, "reblog": null}')`
  - `mastodon_extract_urls()` :480 — HTML sans lien, `href` retiré, `http` et `https`, dédoublonnage
    en conservant l'ordre de première occurrence, `NULL`/`character(0)`/`""`, liste de longueur 1,
    entités HTML **non** décodées (`&amp;` conservé)
  - `mastodon_format_date()` / `mastodon_parse_date()` :429 et :435 — ISO 8601 UTC microsecondes,
    partie horaire conservée à minuit pile (contraste avec B-1), forçage `tz = "UTC"` depuis
    `Europe/Paris` (`withr::local_timezone()`), `tzone` du résultat, forme liste de jsonlite,
    page entière de dates d'un coup, décalage explicite `+02:00` ramené en UTC, aller-retour dans
    les deux sens avec la précision sub-seconde
  3 quirks gelés dans les tests (pas de bug de production) : les ids doivent rester des
  **characters** (passés en numérique, deux snowflakes distincts s'écrasent sur le même double et
  la comparaison répond `FALSE` en silence) ; `mastodon_is_present()` ne teste que la longueur,
  donc `""` et `NA` sont « présents » — ce n'est pas un test de véracité ; `mastodon_extract_urls()`
  est scalaire seulement (contenu de longueur > 1 → erreur `coercion to 'logical(1)'`) et ne
  reconnaît que les `href` en guillemets doubles.

- [x] **T1-5 · helpers Bluesky isolés** — `R/sm-bluesky-api.R` — **fait**
  (`tests/testthat/test-bluesky_utils.R`, 54 assertions)
  Couvert :
  - `bluesky_rate_limited_check()` :235 — 429 avec `RateLimit-Remaining: 0` (+ nom d'en-tête en
    minuscules, httr2 normalise), 429 avec quota restant, 420/500/502/503 avec leur `message()`,
    200/201, erreurs client 400/401/404. Réponses fabriquées avec `httr2::response()`, aucun
    serveur. Rappel de contexte utile aux tests : la fonction est passée en `is_transient` à
    `httr2::req_retry()`, donc `FALSE` = « abandonne et lève sur ce statut ».
  - `bluesky_parse_features()` :514 — post sans `record`/sans `facets`/`facets` vide, `#tag` et son
    alias `#hashtag`, `#link`, accumulation dans l'ordre, plusieurs facets par post, facet sans
    `features`, tag répété conservé deux fois (pas de dédoublonnage ici), types ignorés
    (`#mention`, `#format`, `app.twitter.source#id`, `blue.poll.post.facet#option`) vérifiés
    `expect_silent()`, type inconnu → `warning` **et** `badpost.json` réellement écrit dans
    `conf$data_dir` (`local_conf(data_dir = withr::local_tempdir())`), dump écrasé à chaque post
    inattendu (un seul nom de fichier : aide au débogage, pas un journal).
  - `bluesky_format_date()` / `bluesky_parse_date()` :262 et :270 — même protocole que T1-4 :
    ISO 8601 UTC microsecondes, minuit pile, forçage UTC depuis `Europe/Paris`, `tzone`, forme
    liste de jsonlite, page entière, décalage `+02:00`, précision milliseconde réellement envoyée
    par bluesky (`.123Z` → `.123000Z`), aller-retour dans les deux sens.
  5 quirks gelés dans les tests : un 429 **sans** l'en-tête `RateLimit-Remaining: 0` n'est pas
  retenté (un 429 de passerelle ou de proxy fait donc échouer la collecte, alors que le
  `is_transient` par défaut de httr2 retenterait n'importe quel 429) ; le 504 n'est pas dans la
  liste des transitoires alors que 420/500/502/503 y sont ; la comparaison de l'en-tête se fait
  par `identical(header, "0")`, donc exacte à l'espace près ; `categories` est initialisé et
  retourné mais **jamais** rempli (le format de post standardisé porte toujours une liste vide,
  `R/sm-bluesky-api.R:306`) ; une feature sans `$type` lève *« missing value where TRUE/FALSE
  needed »* au lieu de tomber dans la branche « type inattendu » — et le dump n'a donc pas lieu,
  il ne reste rien à déboguer.

- [x] **T1-6 · aller-retour des attributs de plan par famille** — **fait**
  (`tests/testthat/test-plan_attributes.R`, 146 assertions)
  `sm_plan_parse_attributes_bluesky()` ↔ `sm_plan_format_bluesky()` (`R/sm-bluesky-plan.R`) et
  l'équivalent mastodon (`R/sm-mastodon-plan.R`).
  Contrairement aux dates **communes** (cf. la note de méthode de T1-2), l'aller-retour
  `parse(format(x))` en mémoire **est** ici l'unité pertinente : c'est le formateur de la famille
  qui stringifie, pas `jsonlite`. La branche JSON n'est jouée que là où `jsonlite` remodèle une
  valeur (`{}`, `auto_unbox` d'un tableau de longueur 1).
  Les moitiés « dates » des deux fichiers étant le même code écrit deux fois, la même batterie est
  jouée sur les deux familles (boucle sur `families`) et leur équivalence est gelée par un test.
  Couvert : format des 3 dates de fenêtre (`plan_max_date`, `plan_min_date`, `current_min_date`) en
  `%Y-%m-%d %H:%M:%OS6Z`, date absente laissée absente, reste du plan intact (dates communes
  incluses — c'est la racine de B-1), **idempotence** du formateur sur une date déjà formatée (cas
  réel : `sm_plan_first_attributes_*()` renvoie des chaînes, donc la première sauvegarde formate une
  chaîne) ; parse en `POSIXlt` UTC, `tzone`, forme `{}` et forme liste de jsonlite, forçage UTC
  depuis `Europe/Paris` ; aller-retour à la microseconde (chaîne **et** `as.numeric`), `NULL`
  préservés (`current_min_date` NULL = première requête d'un plan, les deux `sm_api_search_*`
  branchent dessus), minuit pile conservé (le contraste avec B-1 : format explicite ici), date
  donnée en heure locale rendue au même instant, **absence de dérive** sur 10 sauvegardes
  successives (le plan est sauvé après chaque requête), arithmétique sub-seconde encore possible
  au rechargement (`current_min_date - 0.000001`, la borne `until` de bluesky) ; attributs de
  premier plan et de plan suivant relus par le parseur (chaînage exact de `plan_min_date` sur le
  `plan_max_date` précédent) ; côté mastodon le curseur de pagination (types `integer`/`character`,
  `NULL` à l'absence, offset **0** distinct de `NULL` — cf. le « premier appel » de T3-1, passage
  tel quel par le formateur, aller-retour, `seen_ids` de longueur 1 via `auto_unbox`) ; côté
  bluesky, le parseur ne porte **que** les 3 dates (un curseur mastodon écrit sur un plan bluesky
  est perdu au rechargement).
  2 pièges gelés dans les tests (pas de bug de production) : le format du fichier de plans et le
  format de l'API ne diffèrent que d'un caractère (`" "` vs `"T"` — `mastodon_format_date()` /
  `bluesky_format_date()`), et une date à la forme API — ou sans le `Z` final — est lue **`NA`
  silencieusement**, le `NA` ne se manifestant que bien plus tard en *« missing value where
  TRUE/FALSE needed »* dans les comparaisons de fenêtre (même mode de défaillance que B-1) ;
  `as.character()` sur `seen_ids` a l'air de rendre les ids sûrs quel que soit leur type, mais un
  snowflake passé en nombre a déjà perdu ses derniers chiffres dans le double — deux ids distincts
  ressortent en **une seule et même** chaîne, qui n'est l'id de personne.

- [x] **T1-7 · `get_java_version()` + `check_java_version()`** — `R/check.R:60` et `:78` — **fait**
  (`tests/testthat/test-java_version.R`, 54 assertions)
  Du parsing de chaîne pur dès que `get_java_version_vector()` est mocké — c'est la seule fonction
  du trio qui appelle le binaire `java`, donc aucun JVM n'est nécessaire pour les tests.
  Rappel de contexte utile aux tests : `setup_can_run()` (`R/check.R:562`) enchaîne
  `check_64() && check_java_64() && check_java_present() && check_java_version()` dans un
  `tryCatch` qui transforme **n'importe quel warning** en `FALSE`, et `check_all()`
  (`R/check.R:593`) affiche `check_java_version` dans son tableau. La valeur de retour décide donc
  du démarrage d'episomer, et le texte du warning est la seule explication donnée à l'utilisateur.
  Couvert : format moderne (`21.0.1`, `17.0.7`, `11.0.20`, `25.0.1`), version sans update
  (`"9"`, `"10"`), early access (`"22-ea"`), format legacy `1.x` (`1.8.0_301` → 8, `1.7.0_80` → 7)
  et le garde-fou sur le `1` seul (`"1"` → 1, `"1.9.0"` → 9), chiffres du vendeur dans les lignes
  suivantes sans effet (build Zulu), sortie sans ligne « version » → `NULL`, sortie vide →
  `NULL` ; fenêtre `[21, 25]` **épinglée** (bumper un seuil doit casser ce test, c'est le but),
  bornes inclusives et sans warning, version au-dessus → `FALSE` + warning nommant la fenêtre,
  `NULL` → « cannot identify » ; géométrie des branches testée aussi contre des seuils arbitraires
  (`tested > max`) pour être indépendante du cycle de release, et vérification que la fenêtre
  intermédiaire `]max, tested]` est **vide** telle que livrée.
  A produit 3 bugs → voir « Bugs trouvés » (B-11, B-12, B-13).
  Bonus hors périmètre initial : `check_java_64()` (`R/check.R:47`), troisième lecteur du même
  `get_java_version_vector()` et parsing pur pour la même raison — 5 assertions, plus un quirk
  gelé : `grepl("64-Bit", …)` est **sensible à la casse** alors que le warning qu'il émet écrit
  « 64-bit », donc un JVM qui orthographie sa ligne VM autrement est déclaré 32 bits et bloque
  `setup_can_run()` entièrement.

- [ ] **T1-8 · divers rapides**
  - `parse_date()` — `R/search.R:367` (locale forcée à `C`, restauration via `on.exit`)
  - `can_wait_for()` — `R/search.R:376` : tous les plans terminés → attente positive ; un plan
    actif → `0` ; plan seul non listé
  - `conf_languages_as_arg()` / `conf_geonames_as_arg()` — `R/spark-bridge.R:40` et `:56` :
    construction d'arguments shell, quoting, liste de langues vide
  - `java_path()` — `R/spark-bridge.R:2` avec `withr::local_envvar(JAVA_HOME = ...)`
  - `get_location_var()` — `R/geo-tagging.R:3`
  - `get_stop_words()` — `R/text-mining.R:2` : le chemin « cache » (`cached[[stop_id]]` déjà peuplé)
    se teste sans réseau

---

## Tier 2 — avec `local_conf()`, effort faible

- [ ] **T2-1 · `in_pending_status()` / `in_requested_status()`** — `R/scheduler.R:298` et `:324`
  Deux booléens géants : 4 tâches (`dependencies`, `geonames`, `languages`, `alerts`) × timestamps
  de `conf` × `started_on` `NA` ou non. Test paramétré (une table de cas → `expect_equal`).
  Logique de scheduling : une erreur ici bloque le pipeline sans message.

- [ ] **T2-2 · `get_task_day_slots()`** — `R/scheduler.R:138`
  Arithmétique de créneaux avec `%% 24` : le passage de minuit est le cas intéressant.
  Faire varier `conf$schedule_span` (60, 120, 1440) et `conf$schedule_start_hour`.

- [ ] **T2-3 · extensions de `test-config_utils.R`** — `R/config.R`
  `sm_networks_of()`, `sm_networks_where()`, `merge_configs()` (précédence, récursion, liste vide →
  `stop`), `merge_config_lists()`, `get_topics_labels()`, `get_topics_alphas()`,
  `get_topics_k_decays()`, `get_env_props()`, `get_env_secrets()`,
  `add_config_language()` / `remove_config_language()`.
  `coerce_env_props()` et le tour complet des propriétés `runner_*` sont déjà couverts par
  `tests/testthat/test-config_runner_props.R` (issue 3 de `multi_services_plan.md`) : y sont aussi
  figées les deux limites actuelles de `EPISOMER_OPTION_*` (l'environnement n'est lu que si
  `properties.json` existe, et le fichier l'emporte sur l'environnement).

- [ ] **T2-4 · `loop_run_issues()`** — `R/runners.R:572`
  Arbre de décision retournant des messages utilisateur. Mocker `get_tasks()` et
  `check_sm_auth()` ; couvrir `"fs"`, `"search"`, `"detect"`, et le `stop()` sur nom inconnu.

- [ ] **T2-5 · reste de `paths.R`**
  `test-paths.R` couvre `epipath`/`is_remote`/`to_remote_*`. Compléter avec les `get_*_path()`
  qui ont une logique (fallback défaut → utilisateur) : `get_alert_training_path()`,
  `get_geotraining_path()`, `get_topic_keywords_path()`, `get_alert_file()`, `get_plans_paths()`.

- [ ] **T2-6 · `chart_not_empty()` / `can_render()`** — `R/shiny-app-utils.R:239` et `:213`
  Validation Shiny : tester les messages retournés (période inversée, topic vide) en mockant
  `is_fs_running()` et `check_series_present()`.

- [x] **T2-7 · `get_scala_api_endpoint()` / `fs_url_resolves_here()`** — `R/fs.R:3` — **fait**
  L'ordre de résolution du point d'entrée de l'API fs (`conf$fs_remote` → `EPI_HOST` →
  `conf$fs_url:conf$fs_port` si l'hôte résout ici → `http://localhost:fs_port`), le décalage
  temporaire de `fs_port` du réentraînement des langues, et la mémoïsation de la résolution DNS.
  `fs_url_resolves_here()` est mockée dans les tests du point d'entrée : la vraie fait un lookup
  DNS, qui dépendrait du résolveur de la machine et coûterait ~0,5 s par échec.
  Tests : `tests/testthat/test-fs.R`. Contexte : `multi_services_plan.md` §« `conf$fs_url` est une
  adresse de *bind* ».

---

## Tier 3 — parsers sur fixtures JSON (meilleur rendement anti-bug)

Effort moyen, valeur élevée : déposer des réponses d'API réelles dans
`tests/testthat/fixtures/`, les charger avec `test_path()`, et tester les fonctions **pures** qui
les consomment. Aucun réseau.

**Fixtures** : `dev/dump_fixtures.R` capture les corps de réponse bruts de mstdn.party et de
bluesky avec les identifiants du keyring, écrit les `.json` dans `tests/testthat/fixtures/` et
affiche un rapport de couverture des cas attendus (boost, quote accepté/non accepté, `card`,
alt texts, `poll`, `recordWithMedia`…) pour dire lesquels manquent encore.

`dev/anonymize_fixtures.R` en produit ensuite des **copies** `<nom>_anon.json` : ce sont ces
copies-là qui se committent et que les tests chargent, les captures brutes contiennent des posts
réels et ne doivent pas être versionnées. L'anonymisation remplace les identités (comptes,
handles, DID, avatars, mentions), les identifiants de contenu (`cid`, rkey, ids mastodon — remappés
en restant **strictement croissants** et de même longueur, donc `max_id`/`min_id`/`mastodon_min_id()`
et la divergence entre l'ordre des ids et celui des `created_at` sont préservés) et les textes
libres par du texte synthétique de même longueur et de même langue. Est préservé tout ce que
lisent les parseurs : structure HTML et `<a href>` (ce que lit `mastodon_extract_urls()`), facets
bluesky **réindexées** (`byteStart`/`byteEnd` justes sur le nouveau texte), dates, `language`/`langs`,
noms de hashtags, et le vide qui reste vide. Une même valeur réelle donne toujours le même
substitut, donc les `unique()` des parseurs comptent la même chose. `compare_fixtures()` le vérifie :
il fait tourner les parseurs sur la capture brute et sur sa copie et compare le nombre de posts,
de quotes, d'urls, de tags, de langues et d'utilisateurs (seule la longueur totale des textes
diffère). Relire les copies avant commit.

Les charger avec `jsonlite::fromJSON(test_path(...), simplifyVector = FALSE)` — en
`simplifyVector = TRUE` les listes deviennent des data.frames et les parsers ne voient plus les
mêmes données. **T3-1 et T3-5 ne demandent aucune fixture** et sont faits.

- [x] **T3-1 · `sm_api_update_plan_after_request_mastodon()`** — `R/sm-mastodon-api.R:272` — **fait**
  (`tests/testthat/test-mastodon_pagination.R`, 38 assertions, aucune fixture nécessaire :
  fonction pure de `(plan, results$pagination)`)
  Couvert, une règle du roxygen par test : la série `consecutive_empty` (remise à zéro dès un post
  dans la fenêtre, incrément sinon, départ à 1, `posts_after_mindate` absent lu comme vide,
  avancement même sur une page sans statut) ; l'amorçage de l'ancre au premier appel et le fait
  que « premier appel » se reconnaît à un `current_offset` **NULL** et non à une ancre nulle
  (un plan à l'offset 0 après un rebase doit avancer, pas se ré-amorcer) ; l'`offset` avancé de
  `mastodon_page_size` et **pas** du nombre de lignes conservées (positions pré-compact) ; l'ancre
  immobile pendant la marche ; le plafond : le dernier offset émis est 960, jamais 1000 ; le rebase
  exactement quand l'offset suivant atteindrait `mastodon_max_offset` ; la mémoire glissante
  `seen_ids` (empilée du plus récent au plus ancien, dédoublonnée, bornée à
  `mastodon_seen_ids_size`, forme liste de jsonlite aplatie, intacte quand la requête n'a rien
  gardé) ; `current_min_date` qui ne recule que ; et une marche complète de 30 requêtes vérifiant
  que les morceaux se composent.
  1 quirk gelé : sur une **première** requête sans aucun statut, la branche « page vide » passe
  avant la branche « premier appel », donc le plan ressort avec un offset mais sans ancre.
  Inoffensif — `mastodon_parse_response()` renvoie alors `count = 0` et le plan est terminé par
  `update_plan_after_request()` avant tout appel suivant.

- [x] **T3-2 · `mastodon_parse_response()`** — `R/sm-mastodon-api.R:642` — **fait**
  (`tests/testthat/test-mastodon_parse.R`, 114 assertions avec T3-3)
  Fixture : `mastodon_page_covid_anon.json` (5 statuts, 3 langues, 2 `card`, 1 média avec alt text,
  2 avec hashtags) et `mastodon_page_empty_anon.json`, complétées par des statuts minimaux
  construits par `mstdn_status()` pour un comportement à la fois.
  Couvert : le format de post standardisé, ses 12 champs **dans l'ordre** sur toute la page ;
  `id` = l'**uri** (le seul identifiant stable d'une instance à l'autre) et son repli sur l'id
  local, `uri` = l'`url` publique et son repli sur l'uri de fédération ; `created_at` reformaté ;
  `user_name` = l'`acct` webfinger ; `text` nettoyé de son html, de ses urls et de ses retours à la
  ligne ; `lang` ; `tags` (noms sans les urls) ; `urls` = liens du contenu **puis** du matériel
  embarqué, dédoublonnés ; `categories` toujours vide ; le `network` pris **dans le plan** et non
  en dur. Les filtres : date dans le futur (avec warning), `plan_min_date` (**silencieux**, et
  strict — la borne elle-même est gardée), langue absente, dédoublonnage sur `plan$seen_ids`
  (comparé sur les **uris**, pas sur les ids d'instance).
  Le curseur, calculé sur les statuts **bruts** : `returned` (taille de page, pas le nombre gardé),
  `oldest_id` (minimum **numérique**, filtrés inclus), `posts_after_mindate` (qui survit à une page
  vidée par le dédoublonnage — c'est sa raison d'être), `min_created_at` sur les seuls posts gardés,
  `new_ids` limité à ce que la requête a gardé.
  Et le signal de fin de plan `count`, qui n'est **pas** `stored` : nombre de statuts bruts dans la
  fenêtre ; 0 sur une **première** requête sans rien dans la fenêtre (reconnue à `current_offset`
  NULL) ; sentinelle 1 en cours de marche ; 0 quand la requête complète la série de
  `conf$mastodon_max_empty_requests` requêtes vides (seuil lu dans `conf`, défaut 5 quand la
  configuration est muette) ; page vide.

- [x] **T3-3 · `mastodon_parse_quoted()`** — `R/sm-mastodon-api.R:537` — **fait**
  (même fichier que T3-2)
  Tous les cas du roxygen : boost (`reblog`) et quote post accepté → `is_quote = TRUE` ; quote non
  accepté (état sans `quoted_status`) et le `{}` que l'API envoie pour un quote absent → rien ;
  `card` seule → texte récolté mais **pas** `is_quote` ; langue de la `card` préférée à celle du
  statut ; `card` amputée d'un de ses deux textes → l'autre seul (contraste avec `B-14`) ; alt
  texts des médias collectés, alt vide ignoré ; `card` **et** médias du statut cité récoltés aussi,
  avec les urls écrites dans son contenu ; `poll` reconnu sans récolte ; `quoted_lang` = langue
  **majoritaire** des textes récoltés (`mode()`, donc pas celle du statut cité), coupée à 2 lettres.
  L'html du texte cité est rendu **brut** à dessein : c'est `mastodon_parse_response()` qui nettoie.
  Manque encore dans la fixture (donc couvert seulement en synthétique) : un boost et un quote post
  réels — `dump_mastodon_status()` sur une url précise les capturera.

- [x] **T3-4 · `bluesky_parse_response()` / `bluesky_parse_quoted()`** — `R/sm-bluesky-api.R:286`
  et `:352` — **fait** (`tests/testthat/test-bluesky_parse.R`, 87 assertions)
  Fixture : `bluesky_page_covid_anon.json` (100 posts, 56 avec un `embed`, 10 langues, 13 quotes,
  7 posts sans langue exploitable) et `bluesky_page_empty_anon.json`, complétées par des posts
  minimaux (`bsky_post()`).
  Couvert : les 12 champs standardisés dans l'ordre sur toute la page ; `id` = le `cid` ; `uri`
  reconstruite en permalien `bsky.app` depuis le **did** (pas le handle, qui change) et la clé de
  record ; `created_at` reformaté et lu dans le `record` (pas `indexedAt`) ; `user_name` sans le
  suffixe `.bsky.social`, domaine perso gardé entier ; `text` verbatim (bluesky sert du texte brut,
  retours à la ligne inclus) ; `lang` = **première** langue déclarée ; `tags` et `urls` issus des
  facets, fusionnés avec les urls de l'embed et dédoublonnés ; `categories` toujours vide ;
  `is_quote` qui exige un texte cité **non vide** (`bluesky_parse_quoted()` dit `TRUE` dès qu'un
  record est présent, même vide). Les deux filtres de date : futur (warning) et `current_min_date`
  (warning, strict — la borne est gardée), `min_created_at` recalculé sur les seuls posts gardés.
  `bluesky_parse_quoted()` : post sans embed → retour à **un seul champ** (`list(is_quote =
  FALSE)`) ; carte `external` (titre + description, url récoltée, pas un quote) ; post cité
  (`embed$record$value`) avec sa langue et son repli sur celle du post citant ; feed / liste cités
  lus dans leur `description` ; post cité imbriqué (`embed$record$record$value`) ; `card` du post
  cité (`embed$record$embeds`) ; alt texts des images, alt vide ignoré ; images d'un
  `recordWithMedia` (`embed$media$images`) ; quote disparu (`notFound` / `blocked` / `detached`) et
  vidéo (`playlist`) reconnus sans récolte ni dump ; `quoted_lang` majoritaire coupée à 2 lettres ;
  embed inconnu → warning **et** `badpost.json` réellement écrit (`local_conf(data_dir = …)`,
  puisque la fonction **plante** sur un `data_dir` non défini — d'où l'assertion `expect_silent()`
  sur la fixture, qui prouve que la page n'atteint aucun des deux chemins de dump).
  A produit 2 bugs → voir « Bugs trouvés » : `B-14` et `B-15`. 1 quirk gelé : `user_name` retire le
  suffixe avec `gsub(".bsky.social", "", handle)`, une **expression régulière non ancrée aux points
  non échappés** — un domaine perso qui contient par accident ce motif est amputé
  (`abskyisocial.example.org` → `.example.org`). Correctif : `gsub("\\.bsky\\.social$", ...)`.

- [x] **T3-5 · `sm_api_translate_query_mastodon()` / `_bluesky()`** — `R/sm-mastodon-api.R:61` et
  `R/sm-bluesky-api.R:46` — **fait** (`tests/testthat/test-native_queries.R`, 45 assertions, aucune
  fixture nécessaire)
  Les deux implémentations sont identiques au commentaire près : la même batterie est jouée sur
  les deux et leur équivalence est gelée par un test, pour qu'une correction appliquée d'un seul
  côté se voie. Couvert : une requête native par sous-requête (pas d'OR natif — l'OR se paie en
  requêtes), termes joints par une espace (AND implicite), ordre de sortie, produit cartésien des
  synonymes et son **ordre** (`expand.grid` fait varier le premier terme le plus vite), croissance
  multiplicative du nombre de requêtes, négations suffixées `-terme` appliquées à toutes les
  requêtes de toutes les sous-requêtes, sous-requête sans terme → zéro requête (cas de la requête
  tout-négative), terme sans synonyme → sous-requête abandonnée, type de retour (liste plate de
  chaînes), et l'enchaînement réel `translate_query()` → traducteur natif.
  A produit 3 bugs → voir « Bugs trouvés » : B-6 et B-8 **corrigés** (les tests correspondants sont
  devenus des tests de non-régression), B-7 ouvert ; 1 quirk gelé : un littéral
  multi-mots n'est pas mis entre guillemets, donc `"covid vaccine"` part en AND implicite — la
  recherche d'expression exacte est inexprimable dans le langage de requête.

---

## Tier 4 — plus tard, ou jamais

- [ ] **T4-1 · appels HTTP** : `httr2` est déjà dans les `Imports`, donc
  `httr2::with_mocked_responses()` est disponible pour `sm_api_search_mastodon()` /
  `_bluesky()` (retry, rate limit, pagination bout en bout). À faire après le Tier 3.
- [ ] **T4-2 · serveur Shiny** : `shiny::testServer()` sur `episomer_app()`. Coûteux ; les
  helpers extraits (`shiny-app-dataviz-dashboard_funs.R`, `update_config_from_input()`) sont la
  vraie cible, cf. T2-6.
- [ ] **T4-3 · `get_alert_balanced_df()`** — `R/alert.R:1661` : contient un `sample()`, donc
  `withr::local_seed()` obligatoire. Testable (ratio 25 % test / 75 % train, augmentation des
  catégories minoritaires) mais dépend de `get_alert_training_df()`.

### Explicitement hors périmètre

| Quoi | Pourquoi |
|---|---|
| `search_loop()`, `fs_loop()`, `detect_loop()` | `while(TRUE)` + `Sys.sleep`. Ne pas tester la boucle : tester le corps. `search_loop_worker()` et `search_topic()` **sont** les unités d'une itération — c'est là qu'il faut viser (dépendances réseau, donc Tier 4). **Exception** : `tests/testthat/test-runners_guard.R` (issue 6 de `multi_services_plan.md`) fait tourner `search_loop_worker(sandboxed = TRUE, max_requests = 1)` sur un topic à un plan, avec `search_topic()` bouchonné — une seule itération, aucun réseau, aucune écriture — pour vérifier qu'un lease perdu remonte jusqu'au handler de tête au lieu d'être aplati en erreur simple. Même technique dans `tests/testthat/test-search_retry.R` (issue 11) : quelques itérations avec un `search_topic()` qui échoue, pour vérifier qu'une requête ratée est retentée et qu'un échec permanent finit par rendre le réseau. Et dans `tests/testthat/test-search_scope.R` (issue 12) : un `search_topic()` qui enregistre ce qu'on lui donne au lieu de requêter, pour vérifier qu'un worker ne collecte que les topics de **son** réseau — `conf$topics` est plat, tous réseaux confondus. La partie `search_loop()` (le cluster PSOCK) est testée par `report_search_loop_outcomes()`, sortie de la boucle exprès pour cela. **Et dans `tests/testthat/test-fs_monitor.R`** : le corps du moniteur fs est sorti de `fs_loop()` (`fs_monitor_loop()` et ses trois helpers), donc ce qui se teste sans `while(TRUE)` ni worker `future` est ce que le moniteur fait d'une erreur — une ligne dans `fs_mon.log` puis l'itération suivante, au lieu d'un future qui meurt en silence en emportant le heartbeat du lease `fs` et `health_check()`. |
| `R/runners.R` : `register_*_task()`, `stop_*_task()`, `kill_task()` | `processx`, `taskscheduleR`, PID, dépendant de l'OS. À la limite tester les arguments construits. **Exception** : `register_runner()` et `unregister_runner()` sont couverts par `tests/testthat/test-runners_register.R` (issues 4 et 11 de `multi_services_plan.md`) — fichier PID écrit puis retiré, prise et rendu du lease, `conf$runner_name`, et le fait que `stop_all_runner_tasks()` ne dépende pas du lease. Ce qui reste hors périmètre est `get_running_task_pid()` détectant un *autre* process vivant, qui demande un second process OS. **Deuxième exception** : `wait_for_runner_lease()` et `register_runner(wait = TRUE)` sont couverts par `tests/testthat/test-runners_wait.R` (issue 14) — la boucle d'attente est bornée par un `timeout` et par un `claim_runner_lease()` bouchonné qui refuse *n* fois, donc elle se teste sans `while(TRUE)` ; deux tests seulement laissent tourner l'horloge réelle (TTL à 1 s) pour voir une expiration se produire pour de vrai. |
| `spark_job()`, `spark_df()`, `download_dependencies()`, `download_winutils()` | JVM + réseau. |
| `update_geonames()`, `update_languages()`, `download_big_file()` | téléchargements de plusieurs centaines de Mo. |
| `check_*()` liés au système (`is_java_present`, `is_64bit`, `check_winmsvcr100`, `check_pandoc`, `check_tex`) | dépendants de la machine ; seul le parsing (T1-7) est utile. |
| `create_map()`, `create_topwords()`, `create_topchart()`, `plot_trendline()` | rendu graphique ; au mieux des snapshots fragiles. `get_empty_chart()` et `get_font_family()` sont testables si besoin. |

---

## Bugs trouvés en écrivant les tests

Le comportement actuel est **figé par un test** dans chaque cas (avec un commentaire
`KNOWN BUG` / `KNOWN QUIRK` et la ligne d'assertion corrigée en commentaire juste en dessous).
Corriger le code fera donc échouer le test au bon endroit — c'est voulu.

- [ ] **B-1 · une date commune tombant pile sur minuit est perdue au rechargement** (sévérité : haute)
  `format_plan()` ne stringifie que les dates spécifiques au réseau ; les dates communes
  (`expected_end`, `scheduled_for`, `start_on`, `end_on`) arrivent dans le fichier de plans en
  `POSIXlt` et c'est `jsonlite` qui les convertit — or `format.POSIXlt` **supprime la partie
  horaire quand elle vaut exactement 00:00:00**. Au rechargement,
  `strptime("2026-01-01", "%Y-%m-%d %H:%M:%S")` renvoie `NA`.
  Conséquence : `update_plans_schedule()` évalue `plans[[1]]$expected_end < Sys.time()`, donc
  `if (NA)` → *« missing value where TRUE/FALSE needed »* et la boucle de collecte s'arrête pour
  ce topic.
  Correctif recommandé : stringifier les 4 dates communes dans `format_plan()`, par symétrie avec
  les dates spécifiques au réseau (plutôt que de rendre `strptime` tolérant, qui traiterait le
  symptôme).
  Test : `test-plans.R` → *« a common date falling exactly on midnight is LOST on reload »*

- [ ] **B-2 · `merge_plans()` coerce tout en `character`** (sévérité : latente)
  `mapply()` est appelé sans `SIMPLIFY = FALSE` : dès que toutes les valeurs fusionnées sont des
  vecteurs de longueur 1, le résultat est simplifié en vecteur atomique et les types sont coercés
  au plus large — un `requests` numérique revient en `"4"`.
  Inoffensif aujourd'hui parce que `finish_plans()`, seul appelant, fusionne toujours des plans
  contenant au moins un `POSIXlt` ou un `NULL`, ce qui bloque la simplification. Casse dès qu'un
  plan sans date est fusionné.
  Correctif : `SIMPLIFY = FALSE` dans le `mapply()`. Le même motif existe dans `merge_configs()`
  (`R/config.R:708`) — à vérifier en T2-3.
  Test : `test-plans.R` → *« merge_plans coerces the values to a single type… »*

- [ ] **B-3 · le plafond des plans actifs est à 101, pas 100** (sévérité : cosmétique)
  Le commentaire du source annonce « a maximum of 100 plans are kept », mais le plafond ne
  s'applique qu'à la **queue** et le plan courant est ajouté par-dessus. Le `< 100` vs `<= 100`
  ne change rien : à une queue de 100 exactement, les deux branches renvoient les mêmes
  100 éléments.
  Test : `test-plans.R` → *« update_plans_schedule caps the kept plans at 101, not 100 »*

- [ ] **B-4 · une requête vide fait planter l'import de configuration** (sévérité : haute)
  Dans `translate_query()` (`R/config.R:741`), la boucle est écrite
  `for (i in 1:length(ors))` : quand `ors` est vide (requête vide ou uniquement des espaces),
  `1:0` vaut `c(1, 0)` au lieu de ne pas itérer, et `ors[[i]]` lève
  *« subscript out of bounds »*. Une ligne de topic laissée vide dans le fichier Excel fait donc
  échouer tout l'import, avec un message qui ne nomme ni le topic ni la requête.
  Le même motif `1:length()` est présent sur les boucles `ands` et `parts` juste en dessous.
  Correctif recommandé : `seq_along()` partout (retourne alors un `ors` vide, à traiter en amont
  comme « topic sans requête »).
  Test : `test-translate_query.R` → *« an empty query raises a subscript error »*

- [ ] **B-5 · les opérateurs en casse mixte ne sont pas reconnus** (sévérité : moyenne)
  Seules les graphies tout en minuscules et tout en majuscules sont substituées (`" or "`,
  `" OR "`, `" and "`, `" AND "`). Un `"flu Or influenza"` — exactement ce que produit une
  autocorrection de tableur ou une saisie soignée — n'est pas découpé : le topic recherche la
  chaîne littérale `"flu Or influenza"`, qui ne matche rien. Échec silencieux, aucun warning.
  Correctif recommandé : un seul `gsub("\\s+(or|and)\\s+", ..., ignore.case = TRUE)` par
  opérateur.
  Test : `test-translate_query.R` → *« the operators are NOT recognised in mixed case »*

- [x] **B-6 · un topic écrit comme une simple liste de synonymes cherche leur intersection**
  (sévérité : haute) — **corrigé** dans `sm_api_translate_query_mastodon()` et `_bluesky()`
  Dans les deux traducteurs, `do.call(expand.grid, parsed$ors[[i]])` passe à `expand.grid` un
  **unique argument de type liste** quand la sous-requête ne contient qu'un seul terme — or
  `expand.grid` déballe ce cas et prend la liste pour sa liste de facteurs. Les synonymes du terme
  deviennent donc une **colonne chacun**, c'est-à-dire des termes AND, au lieu d'être les valeurs
  alternatives d'une seule colonne.
  Conséquence : un topic dont la requête est `covid/corona` est cherché comme `covid corona` et ne
  collecte que les messages contenant **les deux** mots. Plus il y a de synonymes, plus le résultat
  est vide, et sans le moindre avertissement. Ajouter un second terme (`covid/corona and vaccine`)
  restaure l'expansion attendue — c'est pourquoi le bug est passé inaperçu.
  Correctif appliqué : sortir de `do.call` et garder la liste emballée,
  `comb <- expand.grid(terms, stringsAsFactors = FALSE)`, plus un `next` explicite sur les
  sous-requêtes vides (le déballage supprimé, `expand.grid(list())` ne renvoie plus `nrow == 0`
  et le garde-fou `nrow(comb) > 0` ne suffirait plus).
  Tests de non-régression : `test-native_queries.R` → *« a term alone in its subquery is expanded,
  not ANDed (B-6) »* et *« a topic made of a single synonym list searches each synonym (B-6) »*
  **Conséquence opérationnelle à traiter avant de déployer** : chaque requête native devient une
  entrée de topic distincte dans la configuration (`R/config.R:474`), donc un topic écrit `a/b`
  passe d'**une** entrée à **deux** au prochain import du fichier Excel. Les entrées existantes
  sont réappariées dans l'ordre, si bien qu'une entrée conserve des plans indiquant une collecte
  déjà remontée jusqu'à `plan_min_date` — alors qu'elle n'avait en réalité collecté que
  l'intersection. L'historique des topics concernés est incomplet et ne sera pas rattrapé tout
  seul : il faut réinitialiser leur état de collecte pour forcer une re-collecte.

- [ ] **B-7 · seul le premier mot d'une négation est réellement exclu** (sévérité : moyenne)
  `translate_query()` accepte une négation multi-mots (`"flu -covid vaccine"` → `neg = "covid
  vaccine"`), et le traducteur l'émet telle quelle, préfixée d'un tiret : `flu -covid vaccine`.
  Les deux API lient le tiret au premier mot seulement, donc `vaccine` devient un terme
  **obligatoire** : le topic collecte strictement moins que le `flu` seul, silencieusement, et
  l'inverse de ce qui était écrit.
  Correctif recommandé : négativer chaque mot du terme, ou l'entourer de guillemets
  (`-"covid vaccine"`, supporté par les deux API).
  Test : `test-native_queries.R` → *« KNOWN BUG: only the first word of a negation is negated »*

- [x] **B-8 · `1:length()` dans les traducteurs natifs** (sévérité : latente) — **corrigé**
  Même motif que B-4 : `for (i in 1:length(parsed$ors))` levait *« subscript out of bounds »* sur
  un `ors` vide. Non atteignable via `translate_query()` (B-4 se déclenche d'abord, sur la même
  entrée), mais les deux fonctions sont exportées et documentées comme prenant une requête parsée,
  dont celle-là est une. Corrigé en même temps que B-6, même boucle : `seq_along()` (et
  `seq_len(nrow(comb))` sur la boucle interne, qui portait le même motif).
  **B-4 reste ouvert** dans `translate_query()`.
  Test de non-régression : `test-native_queries.R` → *« an empty ors list produces no query (B-8) »*

- [ ] **B-9 · une baseline parfaitement plate ne produit aucune alerte** (sévérité : haute)
  Dans `ears_t_reweighted()` (`R/alert.R:186`), `sigmai` est l'écart-type de l'historique privé
  d'un point et `rstar` le résidu divisé par lui. Sur un historique constant, les deux valent 0,
  donc `rstar` vaut `0/0` = `NaN`, `ifelse(NaN > seuil, …)` vaut `NA`, et **tous** les poids — donc
  `U0_dw` et `alarm0_dw` — sortent en `NA`.
  La production lit `alarm0_dw` (`R/alert.R:439`) et `R/alert.R:694` filtre sur
  `!is.na(alert) & alert == 1` : le `NA` est écarté en silence, le topic n'alerte sur **rien**.
  Ce n'est pas une entrée exotique : un topic à faible volume resté à zéro pendant une semaine a
  une baseline plate, et le jour où l'on en parle enfin est exactement celui où l'alerte est
  attendue. La moitié non re-pondérée, elle, répond juste (`U0 = 0`, `alarm0 = 1`) — c'est la
  seule moitié réellement utilisée qui part en `NA`. Une seule journée différente dans la fenêtre
  suffit à tout faire refonctionner, ce qui explique que le bug soit passé inaperçu sur les topics
  actifs.
  Correctif recommandé : traiter le cas `sigmai == 0` (poids à 1, il n'y a pas d'outlier à
  détecter dans un historique constant) plutôt que de laisser le `NaN` se propager.
  Test : `test-alert_ears.R` → *« KNOWN BUG: a perfectly flat baseline produces no alert at all »*

- [ ] **B-10 · `1:length()` dans `ears_t_reweighted()`** (sévérité : latente)
  Troisième occurrence du motif de B-4 et B-8 : `sapply(1:length(ts), …)` sur une série vide
  itère sur `c(1, 0)` et lève *« subscript out of bounds »* dès `ts[[1]]`.
  Non atteignable via `get_alerts_topic_counts()`, qui garde sur `nrow(counts) > 0`
  (`R/alert.R:418`), mais la fonction est exportée et documentée comme prenant « a numeric
  vector ». Correctif : `seq_along()`.
  Test : `test-alert_ears.R` → *« KNOWN BUG: an empty series raises a subscript error »*

- [ ] **B-11 · une version de Java sous le minimum s'entend dire qu'elle marche probablement**
  (sévérité : moyenne)
  Dans `check_java_version()` (`R/check.R:87`), la branche intermédiaire est écrite
  `java_version <= java_tested_version()`, **sans borne inférieure**. Le cas « dans la fenêtre » a
  déjà retourné `TRUE` juste avant, donc ce qui reste est tout ce qui est ≤ `tested` — y compris
  **toutes** les versions sous le minimum.
  Un utilisateur sous Java 8 se voit donc répondre « Some users have reported that your version
  also works, but it is recommended to use a previously compatible Java version », à propos d'une
  release quatorze versions sous le minimum. Le verdict reste `FALSE`, donc rien ne casse ; c'est
  le message — celui affiché dans le tableau de `check_all()` — qui envoie l'utilisateur chercher
  dans la mauvaise direction.
  Ce n'est **pas** une conséquence de `tested == max` : le même comportement se produit avec
  `tested > max`, donc relever `java_tested_version()` ne le corrigerait pas.
  Correctif recommandé : `java_version > java_max_version() && java_version <= java_tested_version()`.
  À noter au passage : `java_tested_version()` vaut aujourd'hui `java_max_version()` (25), donc la
  fenêtre `]max, tested]` pour laquelle la branche a été écrite est **vide** — sa seule utilisation
  atteignable aujourd'hui est la mauvaise.
  Test : `test-java_version.R` → *« KNOWN BUG: a version below the minimum is told it probably
  works (B-11) »*

- [ ] **B-12 · une ligne « version » sans chiffre lève une erreur** (sévérité : basse)
  Dans `get_java_version()` (`R/check.R:70`), `gsub("[^0-9]", ".", ligne)` ne laisse que des points,
  le split rend une unique chaîne vide, le filtre sur les chiffres vide le vecteur, et
  `j_version_parts[[1]]` est alors hors bornes. Le mot « version » est cherché n'importe où dans la
  ligne et n'a pas besoin d'être suivi d'un nombre (`"Unrecognized option: -version"` suffit).
  L'erreur sort de `get_java_version()` en **erreur**, pas en warning — or `setup_can_run()` ne
  rattrape que les warnings (`R/check.R:563`), donc elle remonte jusqu'à l'utilisateur.
  Correctif : rendre `NULL`, comme pour une ligne jamais trouvée.
  Test : `test-java_version.R` → *« KNOWN BUG: a matched line with no digits raises a subscript
  error (B-12) »*

- [ ] **B-13 · la première ligne contenant le mot « version » gagne** (sévérité : basse)
  La JVM écrit « Picked up JAVA_TOOL_OPTIONS: … » sur stderr **avant** sa propre sortie de version,
  et `get_java_version_vector()` fusionne stderr dans stdout (`2>&1`, `R/check.R:32` et `:39`).
  Toute ligne de ce genre contenant la sous-chaîne « version » est trouvée en premier et parsée à
  la place de la vraie : un `JAVA_TOOL_OPTIONS=-Dapp.version=3` fait rapporter un Java 21 parfait
  comme la version 3, que `check_java_version()` refuse alors avec un warning nommant une version
  que l'utilisateur n'a pas installée — il est envoyé réinstaller un JDK déjà correct.
  Correctif : ancrer la recherche sur la ligne de version, p. ex. `grep("^\\S* ?version ", jver)`.
  Test : `test-java_version.R` → *« KNOWN QUIRK: the first line holding the word 'version' wins
  (B-13) »*

- [ ] **B-14 · une carte bluesky amputée d'un de ses deux textes injecte la chaîne « NULL »**
  (sévérité : basse)
  Dans `bluesky_parse_quoted()` (`R/sm-bluesky-api.R:391`), le filtre est écrit
  `texts[!is.null(texts)]` : `is.null()` regarde le vecteur **entier**, qui n'est jamais `NULL`,
  donc il répond toujours `FALSE`, le `!` le rend `TRUE` et le sous-ensemble garde **tout** — y
  compris le champ absent, que `paste()` rend comme le littéral `"NULL"`. Une carte `external` qui
  n'a qu'une description (ou qu'un titre) produit donc un `quoted_text` valant
  `"NULL\nla description"`, qui part tel quel dans le matching de mots-clés et en base.
  Correctif : `texts[!sapply(texts, is.null)]`. Le pendant mastodon fait déjà le bon filtre
  (`card_texts[!is.na(card_texts) & nchar(card_texts) > 0]`, `R/sm-mastodon-api.R:563`).
  Latent sur la fixture capturée (les 37 cartes y ont toujours les deux champs), mais rien ne
  l'empêche : `title` et `description` sont tous deux optionnels dans le lexicon.
  Test : `test-bluesky_parse.R` → *« KNOWN BUG: an external card missing one of its texts injects
  the string NULL (B-14) »*

- [ ] **B-15 · le warning « post hors fenêtre » nomme les deux dates à l'envers** (sévérité :
  cosmétique)
  Dans `bluesky_parse_response()` (`R/sm-bluesky-api.R:324`), les arguments du `sprintf()` sont
  inversés : le message annonce la date de création à l'endroit où il imprime la borne, et
  inversement. Purement cosmétique, mais c'est le message qu'on lit précisément quand on cherche à
  comprendre un plan qui n'avance plus.
  Test : `test-bluesky_parse.R` → *« KNOWN BUG: the out-of-window warning names the two dates the
  wrong way round (B-15) »*

## Outillage

- `covr::report()` — carte de couverture réelle, à relancer après chaque tier pour arbitrer la suite.
- `Config/testthat/parallel: true` dans `DESCRIPTION` quand la suite grossira.
- `withr` est déjà dans `Suggests` ; ajouter `mockery` n'est **pas** nécessaire,
  `local_mocked_bindings()` de testthat 3e suffit.

## Ordre de départ recommandé

~~`T1-2`~~ → ~~`T1-1`~~ → ~~`T1-3`~~ → ~~`T1-4`~~/~~`T1-5`~~ → ~~`T3-1`~~/~~`T3-5`~~ → ~~`T1-7`~~ →
~~`T3-2`~~/~~`T3-3`~~/~~`T3-4`~~ → `T2-1`. Reste du Tier 1 : `T1-8`. Le Tier 3 est terminé ; il
reste à compléter la fixture mastodon avec un boost et un quote post réels (cf. T3-3).

`B-6` est corrigé. Trois bugs de production restent à traiter en priorité, par ordre d'urgence :
`B-9` (un topic à baseline plate n'alerte jamais — la détection ne fait rien, sans message),
`B-1` (une date de minuit arrête la boucle de collecte d'un topic), puis `B-4` (une ligne de
topic vide fait échouer tout l'import Excel). Et à traiter la conséquence opérationnelle de
`B-6` : réinitialiser l'état de collecte des topics écrits en liste de synonymes, dont
l'historique est incomplet.

Le motif `1:length()` a maintenant trois occurrences relevées (`B-4`, `B-8` corrigé, `B-10`) :
un `grep -rn "1:length(" R/` avant le prochain tier vaut probablement le détour.
