# -*- coding: utf-8 -*-
"""Génère inst/schema/runners_lease.excalidraw en entier.

Un panneau = une question qu'on se pose en relisant R/runners-lease.R et R/runners.R.
L'ordre va du « pourquoi » au « comment », puis aux cas limites.
"""
import sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
SCHEMA_DIR = os.path.join(HERE, "..", "..", "episomer", "inst", "schema")
from exca import *  # noqa

L = 80            # marge gauche
W = 2560          # largeur utile d'un panneau
Y = 40            # curseur vertical

# =================================================================================================
# En-tête
# =================================================================================================
T(L, Y, "Les leases de runner — comment episomer empêche deux machines de faire le même travail", 28, BLACK)
Y += 44
T(L, Y,
  "Fichiers : R/runners-lease.R (le mécanisme) · R/runners.R (les deux couches, PID + lease) · R/fs.R, R/search.R, R/scheduler.R, R/alert.R (les points d'appel)\n"
  "Conception : inst/schema/multi_services_plan.md — issues 1 à 14. Ce schéma est la version « on reprend tout depuis le début » de ce document.\n"
  "Ajouts après le premier jet : panneau 15 (attendre au lieu de mourir, issue 14) · 16 (le tueur rend pour le tué, issue 13) · 17 (le cas fs et son moniteur).",
  14, GREY)
Y += 50

# =================================================================================================
# 0 · Le problème
# =================================================================================================
Y = panel(L, Y, W, "0 · Le problème qu'on résout",
          "Une seule base de données, plusieurs machines qui la partagent. Rien, dans le code d'origine, n'empêchait deux d'entre elles de collecter les mêmes posts.")

y0 = Y
box(L + 700, y0, 460,
    "machine A  —  la base de données (fs)",
    ["Les index Lucene, sur son disque local.",
     "Elle seule a le droit de les ouvrir.",
     "",
     "Elle héberge aussi :",
     "  <data_dir>/runners/*.json   ← les leases",
     "  properties.json, tasks.json, topics.*.json"],
    ORANGE, ORANGE_BG)

yb = y0 + 190
box(L, yb, 420, "machine B  —  search_loop()",
    ["Collecte les posts.", "Aucune donnée en propre : elle parle à A en HTTP."], BLUE, BLUE_BG)
box(L + 520, yb, 420, "machine C  —  search_loop()",
    ["Le même code, la même configuration,", "la même base."], BLUE, BLUE_BG)
box(L + 1040, yb, 420, "machine D  —  detect_loop()",
    ["Les alertes. Elle écrit tasks.json", "et envoie des courriels."], BLUE, BLUE_BG)

A([(L + 210, yb), (L + 780, y0 + 120)], BLUE)
A([(L + 730, yb), (L + 900, y0 + 120)], BLUE)
A([(L + 1250, yb), (L + 1030, y0 + 120)], BLUE)
T(L + 430, yb - 40, "HTTP  (epipath / epifile.*)", 12, BLUE)

yc = yb + 130
box(L, yc, 700, "Sans exclusion",
    ["B et C demandent les mêmes posts à la même API, et les écrivent tous les deux.",
     "→ comptages doublés → seuils de détection faux → alertes fausses.",
     "→ et, pour detect : deux fois les mêmes courriels envoyés aux abonnés,",
     "   ce qu'aucune déduplication côté base ne peut rattraper."],
    RED, RED_BG)
box(L + 760, yc, 700, "Avec le lease",
    ["Un seul des deux collecte. L'autre lit un petit fichier JSON, comprend que la",
     "place est prise, et attend en réessayant.",
     "Quand le premier s'arrête (ou disparaît), le second prend le relais tout seul.",
     "C'est tout le mécanisme : un fichier, une date de dernière modification, un TTL."],
    GREEN, GREEN_BG)

box(L + 1520, y0, 940, "Ce que le lease n'est PAS",
    ["· Ce n'est pas un verrou distribué. L'API fs n'a pas d'écriture conditionnelle",
     "  (« écris seulement si le fichier n'a pas changé »), donc un upload est toujours",
     "  « le dernier qui écrit gagne ». Deux instances peuvent, en théorie, se croire",
     "  toutes les deux titulaires pendant quelques secondes.",
     "· Ce n'est pas une sécurité. Qui atteint l'API fs peut lire, voler ou libérer",
     "  n'importe quel lease — comme n'importe quel autre fichier qu'elle sert.",
     "· Ce n'est pas non plus un remplacement des fichiers PID : les deux couches",
     "  coexistent et répondent à deux questions différentes (panneau 2).",
     "",
     "Le vrai filet de sécurité n'est pas le claim, c'est stop_if_lease_lost()",
     "appelé avant chaque écriture qui compte (panneau 10)."],
    GREY)

Y = yc + 160

# =================================================================================================
# 1 · Vocabulaire
# =================================================================================================
Y = panel(L, Y, W, "1 · Le vocabulaire, une fois pour toutes",
          "Ces mots se retrouvent tels quels dans les noms de fonctions. Les fixer maintenant évite de relire le code deux fois.")

items = [
    ("service", ORANGE, ["fs, search, detect.", "Le type de travail."]),
    ("runner (le nom)", BLUE, ["L'unité d'exclusivité, plus fine que le service :", "fs · fs_mon · search · search.bluesky ·", "search.mastodon.piaille.fr · detect"]),
    ("instance", VIOLET, ["Un processus episomer qui tourne.", "conf$instance_id  : change à chaque démarrage.", "conf$instance_host : la machine (EPISOMER_INSTANCE)."]),
    ("lease (le bail)", GREEN, ["Le droit d'être un runner donné, accordé pour", "runner_lease_ttl(name) secondes, renouvelable.", "Matériellement : runners/<name>.json"]),
    ("claim", GREEN, ["Prendre le lease : écrire son identité dans le fichier,", "attendre `guard` secondes, puis relire pour confirmer."]),
    ("renew / heartbeat", VIOLET, ["Prouver qu'on est vivant : toucher le fichier, ce qui", "met à jour son mtime. Aucun contenu réécrit."]),
    ("release", GREEN, ["Rendre le lease : déplacer le fichier vers", "runners/old/<name>.<instance_id>.json"]),
    ("TTL", RED, ["Le délai au-delà duquel un lease non renouvelé", "est considéré libre. C'est AUSSI la latence de bascule."]),
    ("guard", RED, ["La pause entre l'écriture optimiste du claim et", "la relecture qui le confirme (5 s par défaut)."]),
    ("stale", RED, ["Un lease que personne n'a renouvelé depuis plus", "que son TTL. Son titulaire est présumé mort."]),
    ("mtime / seen_at", ORANGE, ["La date de dernière modification du fichier,", "produite par la SEULE machine fs.", "C'est la seule chose qui décide de la fraîcheur."]),
    ("holder / titulaire", BLUE, ["L'instance dont l'instance_id est écrit dans le", "fichier en ce moment."]),
]
cw, gap = 620, 20
for i, (name, col, lines) in enumerate(items):
    r, c = divmod(i, 4)
    box(L + c * (cw + gap), Y + r * 108, cw, name, lines, col, h=98, title_size=14, line_size=11)
Y += 3 * 108 + 40

# =================================================================================================
# 2 · Les deux couches
# =================================================================================================
Y = panel(L, Y, W, "2 · Deux couches, deux questions différentes",
          "C'est la confusion la plus coûteuse à faire dans ce code : le fichier PID et le lease ne répondent pas à la même chose, et on ne peut pas remplacer l'un par l'autre.")

y0 = Y
box(L, y0, 1180, "Couche 1 · le fichier PID   (inchangée, elle existait déjà)",
    ["Où       : sur le disque LOCAL de chaque machine, conf$data_dir/<name>.PID",
     "Contenu  : un nombre, le PID du processus qui s'est enregistré ici",
     "Répond à : « y a-t-il, SUR CETTE MACHINE, un processus que je peux tuer ? »",
     "",
     "Lue et écrite par :",
     "   register_runner()            écrit le PID",
     "   read_running_task_pid_file() NA si pas de fichier, le PID sinon",
     "   get_running_task_pid()       -1 si absent OU si le processus est mort",
     "   is_runner_running_locally()  la question locale",
     "   kill_task() / stop_runner_task() / stop_all_runner_tasks()",
     "   remove_running_task_pid()    retire le fichier",
     "",
     "Attention : dans un conteneur, conf$data_dir est LOCAL au conteneur.",
     "Seul jobs/ est monté en partage. Deux conteneurs ne voient donc pas",
     "les fichiers PID l'un de l'autre — ce qui est exactement ce qu'on veut."],
    BLUE, BLUE_BG)

box(L + 1280, y0, 1180, "Couche 2 · le lease   (nouvelle, R/runners-lease.R)",
    ["Où       : sur la machine fs, <data_dir>/runners/<name>.json, via epipath()",
     "Contenu  : l'identité du titulaire (JSON) + le mtime du fichier",
     "Répond à : « qui, DANS TOUT LE CLUSTER, a le droit d'être ce runner ? »",
     "",
     "Lue et écrite par :",
     "   claim_runner_lease()     prendre la place",
     "   renew_runner_lease()     un touch, le battement de cœur",
     "   release_runner_lease()   rendre la place (déplacement vers old/)",
     "   i_hold_runner_lease()    est-ce toujours mon instance_id ?",
     "   stop_if_lease_lost()     le garde, avant chaque écriture",
     "   runner_lease_status()    l'état lisible (app Shiny, is_*_running)",
     "",
     "epipath() fait que le MÊME code marche pour un fs local (accès fichier",
     "direct) et pour un fs distant (requêtes HTTP). Rien dans ce fichier",
     "ne sait lequel des deux est en train de tourner."],
    GREEN, GREEN_BG)

y1 = y0 + 320
box(L, y1, 2380, "Et register_runner() exige que LES DEUX disent oui",
    ["   1. la couche locale d'abord — moins chère, pas de réseau, et un refus ne dérange pas du tout la machine fs ;",
     "   2. la couche cluster ensuite — et si elle refuse, le fichier PID écrit à l'étape 1 est retiré, sinon cette machine aurait l'air d'héberger un runner (panneau 5)."],
    BLACK)
Y = y1 + 100

# =================================================================================================
# 3 · Le fichier
# =================================================================================================
Y = panel(L, Y, W, "3 · Le fichier de lease, champ par champ",
          "runners/search.bluesky.json — écrit une seule fois, au claim. Le renouvellement ne réécrit rien : il ne fait que toucher le fichier.")

y0 = Y
box(L, y0, 700, "runners/search.bluesky.json",
    ['{',
     '  "runner"           : "search.bluesky",',
     '  "service"          : "search",',
     '  "instance_id"      : "7f3a1c2e9b",',
     '  "host"             : "machine-B",',
     '  "pid"              : 41,',
     '  "episomer_version" : "4.0.5",',
     '  "claimed_at"       : "2026-08-28T07:05:11Z",',
     '  "renewed_at"       : "2026-08-28T07:05:11Z"',
     '}',
     '',
     'écrit par write_runner_lease() via write_json_atomic()',
     '(fichier temporaire + rename → un lecteur ne voit jamais',
     'un fichier à moitié écrit)'],
    ORANGE, ORANGE_BG)

box(L + 760, y0, 620, "Ce qui DÉCIDE",
    ["mtime du fichier   → lease$seen_at",
     "     lu par epifile.mtime(), produit par la machine fs.",
     "     C'est la fraîcheur : age = now - seen_at.",
     "",
     "instance_id",
     "     « est-ce toujours moi ? » — i_hold_runner_lease(),",
     "     stop_if_lease_lost(), lease_is_free().",
     "",
     "host + pid  (ensemble seulement)",
     "     le raccourci « ce titulaire a planté ICI »",
     "     (panneau 7)."],
    GREEN, GREEN_BG)

box(L + 1440, y0, 620, "Ce qui INFORME seulement",
    ["claimed_at        depuis quand cette instance est titulaire",
     "renewed_at        l'heure du claim, PAS du dernier battement",
     "pid               diagnostic, sauf le cas host+pid ci-contre",
     "host              affiché partout : « running on host … »",
     "episomer_version  utile quand deux machines n'ont pas",
     "                  la même version installée",
     "runner / service  redondants avec le nom du fichier,",
     "                  mais ils rendent le fichier lisible seul"],
    GREY)

box(L + 2120, y0, 340, "Pourquoi le mtime",
    ["Il est produit par la SEULE",
     "machine qui héberge le fs.",
     "",
     "Aucun décalage d'horloge entre",
     "les machines ne peut donc faire",
     "passer un lease mort pour vivant.",
     "",
     "Les dates dans le JSON, elles,",
     "viennent de l'horloge du titulaire",
     "→ jamais utilisées pour décider."],
    ORANGE, ORANGE_BG)

y1 = y0 + 300
box(L, y1, 1380, "Piège n°1 : ne jamais juger la fraîcheur avec renewed_at",
    ["renewed_at n'est réécrit qu'au claim, parce que renouveler = toucher le fichier, pas le réécrire.",
     "Un runner en parfaite santé depuis six heures a un renewed_at vieux de six heures.",
     "Un long « Since » à côté d'un court « Last seen » est le portrait d'un runner en bonne santé (panneau 13)."],
    RED, RED_BG)

box(L + 1440, y1, 1020, "Piège n°2 : l'ordre de lecture de read_runner_lease()",
    ["Le CONTENU est lu avant le mtime, volontairement. Dans l'autre sens :",
     "   mtime périmé de A lu → A libère → B prend le lease → contenu de B lu",
     "   → on croit « B détient et a l'air expiré » → on vole un lease vivant.",
     "Dans le bon sens le pire cas est « A détient et a l'air frais » : on s'abstient pour rien, ce qui est sans danger."],
    RED, RED_BG)
Y = y1 + 130

# =================================================================================================
# 4 · Les runners
# =================================================================================================
Y = panel(L, Y, W, "4 · Les runners qui existent, et leur TTL",
          "runner_lease_ttl(name) prend le service (la partie avant le premier point) et cherche conf$runner_ttl_<service>, sinon runner_lease_ttl_defaults(), sinon 300 s.")

Y = table(L, Y, [340, 520, 720, 260, 620],
          ["runner", "pris par", "renouvelé par", "TTL", "remarque"],
          [["fs", "fs_loop()", "renew_fs_runner_lease(), depuis le moniteur fs", "runner_ttl_fs — 120 s",
            "spark_job() ne rend jamais la main à R"],
           ["fs_mon", "register_fs_monitor(), à chaque tour de 2 s", "le même appel (chemin rapide du claim)", "défaut 120 s — pas de propriété",
            "le seul ré-enregistré en boucle — panneau 17"],
           ["search  (parent)", "search_loop()", "PERSONNE → devient stale sur une machine saine", "runner_ttl_search — 300 s",
            "assumé §9.5 — dernier point ouvert de l'issue 14"],
           ["search.<réseau>", "search_loop_worker(), un par média actif", "chaque tour + sleep_with_heartbeat + retry 429", "runner_ttl_search — 300 s",
            "c'est LÀ qu'est la vraie exclusivité"],
           ["detect", "detect_loop()", "chaque tour + sleep_with_heartbeat", "runner_ttl_detect — 1800 s",
            "une seule tâche peut durer des heures"]],
          color=BLUE)
Y += 20

box(L, Y, 1180, "Choisir un TTL",
    ["Trop COURT  → fausses expirations : un runner occupé mais vivant se fait voler sa place → deux collecteurs → doublons.",
     "Trop LONG   → seule la bascule est plus lente. Aucun risque de corruption.",
     "→ Se tromper « trop long » est le sens sûr. Le TTL n'est pas un bouton de performance.",
     "Le battement (runner_heartbeat_span, 30 s) doit rester ~10× plus court que le plus court TTL :",
     "quelques renouvellements perdus d'affilée (fs lent, réseau qui hoquette) ne doivent pas suffire à expirer un lease vivant."],
    RED, RED_BG)
box(L + 1240, Y, 1220, "Où se règlent ces valeurs",
    ["conf$runner_heartbeat_span · runner_ttl_fs · runner_ttl_search · runner_ttl_detect · runner_claim_guard",
     "Stockées dans properties.json — que TOUTES les machines lisent : ces réglages appartiennent au cluster, pas à une machine.",
     "Volontairement ABSENTES de la page de configuration Shiny : un TTL mal tapé y casserait l'exclusivité de tout le cluster d'un coup,",
     "et la page est ouverte à des utilisateurs non administrateurs. Ce sont des réglages de déploiement, pas d'analyse.",
     "Sur un premier déploiement : EPISOMER_OPTION_RUNNER_TTL_SEARCH=600 … (coerce_env_props() leur donne le type du défaut)."],
    ORANGE, ORANGE_BG)
Y += 160

# =================================================================================================
# 5 · register_runner
# =================================================================================================
Y = panel(L, Y, W, "5 · Prendre son poste — register_runner(name)",
          "Le point d'entrée de tout : appelé par register_fs_runner(), register_search_runner(network), register_detect_runner(), register_fs_monitor().")

x = L
y0 = Y
step_w = 430
def step(x, y, title, lines, color, bg="transparent"):
    return box(x, y, step_w, title, lines, color, bg)

y = y0
y = step(x, y, "1 · stop_if_no_config()", ["Pas de configuration → on ne peut rien lire", "ni écrire. On s'arrête tout de suite."], GREY)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
y += 30
y = step(x, y, "2 · get_running_task_pid(name)", ["Lit <name>.PID sur CE disque.", "-1 si absent ou si le processus est mort."], BLUE, BLUE_BG)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
y += 30
y = step(x, y, "3 · un AUTRE PID vivant ici ?", ["last_pid >= 0 && last_pid != pid"], BLUE, BLUE_BG)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
A([(x + step_w, y - 30), (x + step_w + 120, y - 30)], RED)
box(x + step_w + 130, y - 62, 480, "oui → stop()",
    ['"Runner <name> PID: 4123 is already running"',
     "Rien n'a été écrit, aucune requête réseau."], RED, RED_BG)
y += 30
y_claim = y
y = step(x, y, "4 · claim, wait = FALSE ou TRUE",
         ["claim_runner_lease(name)          → panneau 6",
          "wait_for_runner_lease(name)      → panneau 15",
          "Le seul moment où l'on parle au fs."], GREEN, GREEN_BG)
A([(x + step_w, y_claim + 40), (x + step_w + 120, y_claim + 40)], RED)
box(x + step_w + 130, y_claim - 6, 700, "FALSE → refus, et on ne nettoie RIEN",
    ["stop(runner_lease_holder_message(name))",
     "",
     "Le disque local est laissé EXACTEMENT comme on l'a trouvé — pas de",
     "remove_running_task_pid(). C'est un renversement de l'issue 13 : le",
     "<name>.PID du titulaire précédent est le second des deux signaux dont",
     "lease_holder_is_dead_locally() a besoin (panneaux 7 et 16). Le supprimer",
     "détruisait la preuve pour TOUS les redémarrages suivants.",
     "",
     '   "Runner \'search.bluesky\' is already held by another instance (7f3a1c2e)',
     "    running on host 'machine-B' since 2026-08-28T07:05:11Z, last seen 12",
     '    seconds ago (lease TTL: 300 seconds). This instance will not start it."',
     "",
     "Ce message répond aux deux seules questions utiles à cet instant :",
     "OÙ ça tourne déjà, et DEPUIS QUAND (+ l'âge du dernier battement, qui",
     "distingue un titulaire vivant d'un titulaire sur le point d'expirer)."], RED, RED_BG)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
y += 30
y = step(x, y, "5 · écrire <name>.PID",
         ["write(pid, file = pid_path)",
          "APRÈS le claim, et c'est tout l'objet de l'issue 13."], ORANGE, ORANGE_BG)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
y += 30
y = step(x, y, "6 · conf$runner_name <- name", ["Posé EN DERNIER : il ne nomme jamais", "qu'un runner réellement détenu."], GREEN, GREEN_BG)
A([(x + step_w / 2, y), (x + step_w / 2, y + 30)])
y += 30
y = step(x, y, "7 · le runner travaille", ["Le nom sert de défaut partout :", "renew_runner_lease(), stop_if_lease_lost(),", "release_runner_lease() s'appellent sans argument."], GREEN, GREEN_BG)

box(L + 1400, y0, 1060, "Quatre décisions qui ne se devinent pas à la lecture",
    ["· L'ordre des LECTURES : local, puis réseau. Le contrôle local est gratuit et ne dérange personne ; le claim, lui,",
     "  écrit sur la machine fs. Échouer le moins cher d'abord.",
     "",
     "· L'ordre des ÉCRITURES : le claim, PUIS le fichier PID — l'inverse de la première version. Le PID file laissé par",
     "  le titulaire précédent est une PREUVE, pas un encombrement : écrire le nôtre par-dessus rendait impossible la",
     "  reprise rapide d'un runner mort ici, depuis la seule fonction qui démarre un runner (panneau 16).",
     "  Le prix, assumé : deux processus de cette machine démarrant le même runner dans la fenêtre du guard sont",
     "  désormais séparés par le lease et non par le fichier PID. C'est le travail du lease.",
     "",
     "· conf$runner_name est posé par register_runner(), pas par setup_config(). Seul un processus qui a réellement pris un",
     "  lease peut en porter un, et setup_config() est rappelé à chaque rafraîchissement de configuration. C'est de l'état de",
     "  session, comme instance_id : jamais écrit dans properties.json.",
     "  Dans l'app Shiny il reste NULL — ce qui rend release_runner_lease() et stop_if_lease_lost() inoffensifs là-bas.",
     "",
     "· Un claim refusé est une situation NORMALE, pas une panne. C'est la machine de secours qui découvre que l'autre",
     "  collecte déjà. Pour un SERVICE, la réponse n'est plus de sortir en espérant un redémarrage mais d'attendre sur",
     "  place : wait = TRUE, panneau 15. `restart: on-failure` reste, mais dégradé au rang de filet.",
     "",
     "Cas particulier de register_fs_monitor() : appelé à CHAQUE tour de la boucle de 2 s du moniteur fs. Sans le chemin",
     "rapide de claim_runner_lease() (panneau 6), il paierait une écriture JSON + 5 s de guard toutes les deux secondes."],
    GREY)
Y = y + 60

# =================================================================================================
# 6 · claim
# =================================================================================================
Y = panel(L, Y, W, "6 · Prendre le lease — claim_runner_lease(name, guard)",
          "« écrire, attendre, relire ». Cette danse remplace un compare-and-swap que l'API fs n'a pas.")

y0 = Y
x = L
y = box(x, y0, 520, "current <- read_runner_lease(name)",
        ["NULL si le fichier n'existe pas,", "sinon son contenu + seen_at (= le mtime)."], BLUE, BLUE_BG)
A([(x + 260, y), (x + 260, y + 30)])
y += 30
y_free = y
y = box(x, y, 520, "lease_is_free(current, name) ?", ["→ panneau 7"], BLUE, BLUE_BG)

A([(x + 520, y_free + 30), (x + 660, y_free + 30)], RED)
box(x + 670, y_free - 4, 700, "NON — quelqu'un de vivant tient la place",
    ["return FALSE.  AUCUNE écriture : le titulaire n'est",
     "pas dérangé du tout, pas même son mtime."], RED, RED_BG)

A([(x + 260, y), (x + 260, y + 30)])
y += 30
y_me = y
y = box(x, y, 520, "current$instance_id == conf$instance_id ?",
        ["« le lease vivant porte déjà mon nom »"], VIOLET, VIOLET_BG)
A([(x + 520, y_me + 30), (x + 660, y_me + 50)], VIOLET)
y_fast = box(x + 670, y_me + 20, 940, "OUI — chemin rapide : ré-enregistrement, pas acquisition",
    ["renew_runner_lease(name) ; return TRUE",
     "",
     "Ni réécriture, ni attente de guard : il n'y a rien contre quoi courir, puisqu'un",
     "concurrent aurait déjà remplacé mon nom dans la lecture ci-dessus.",
     "Sans ce raccourci, register_fs_monitor() (toutes les 2 s) paierait une écriture",
     "JSON + 5 s de sommeil… toutes les 2 s."], VIOLET, VIOLET_BG)
A([(x + 260, y), (x + 260, y + 30)])
y += 30
y = box(x, y, 520, "write_runner_lease(name)",
        ["Écriture OPTIMISTE : j'écris mon identité", "par-dessus ce qu'il y avait.", "write_json_atomic() → jamais de demi-fichier."], ORANGE, ORANGE_BG)
A([(x + 260, y), (x + 260, y + 30)])
y += 30
y = box(x, y, 520, "runner_claim_wait(guard)",
        ["Sys.sleep(guard) — 5 s par défaut.", "Fonction séparée pour qu'un test puisse", "y glisser une écriture concurrente."], VIOLET, VIOLET_BG)
A([(x + 260, y), (x + 260, y + 30)])
y += 30
y = box(x, y, 520, "i_hold_runner_lease(name)",
        ["Lecture de CONFIRMATION : le fichier", "porte-t-il encore mon instance_id ?", "→ c'est la valeur de retour du claim."], GREEN, GREEN_BG)

y_race = box(x + 670, y0 + 470, 1000, "La course, pas à pas",
    ["    B                                  C",
     "    read → absent                      read → absent",
     "    write  holder = B                  write  holder = C     ← C atterrit en dernier",
     "                     … guard secondes …",
     "    relit → holder = C                 relit → holder = C",
     "    FALSE, s'efface                    TRUE, collecte",
     "",
     "`guard` doit être plus long que la pire latence d'écriture, sinon le perdant relirait",
     "AVANT que l'écriture du gagnant n'atterrisse, et tous les deux se croiraient titulaires.",
     "",
     "La fenêtre résiduelle n'est pas éliminée : elle est couverte par stop_if_lease_lost()",
     "avant chaque écriture qui compte (panneau 10)."], GREY)

y_why = box(L + 1720, y0, 740, "Pourquoi pas plus simple ?",
    ["L'API fs ne propose que `upload`, qui écrase.",
     "Il n'existe pas de « écris seulement si le fichier",
     "n'a pas changé depuis la version X ».",
     "",
     "L'issue 10 (optionnelle, côté Scala) ajouterait",
     "   POST /file/…?action=upload&ifMd5=<md5>",
     "renvoyant 409 en cas de désaccord.",
     "claim_runner_lease() l'utiliserait quand elle",
     "existe, en gardant le chemin actuel en repli.",
     "",
     "Tant qu'elle n'existe pas, on assume : le lease est",
     "COOPÉRATIF. Il empêche l'erreur, pas l'adversaire."], ORANGE, ORANGE_BG)
Y = max(y, y_race, y_why, y_fast) + 60

# =================================================================================================
# 7 · lease_is_free
# =================================================================================================
Y = panel(L, Y, W, "7 · « La place est-elle libre ? » — lease_is_free(lease, name)",
          "Cinq raisons de dire OUI, testées dans cet ordre ; la première qui répond gagne. Tout le reste est OCCUPÉ.")

y0 = Y
rows = [
    ("1", "lease est NULL", "aucun fichier : jamais pris, ou rendu (déplacé vers runners/old/)", GREEN),
    ("2", "lease$instance_id == conf$instance_id", "c'est déjà moi. Après un redémarrage l'instance_id a changé, donc « moi » ne survit pas à un restart", GREEN),
    ("3", "seen_at est NA", "le fichier a disparu pendant qu'on le lisait — c'est le cas libre aussi", GREEN),
    ("4", "lease_holder_is_dead_locally(lease, name)", "le titulaire est un processus de CETTE machine et il est mort → reprise immédiate, sans attendre le TTL", GREEN),
    ("5", "age = now - seen_at  >  runner_lease_ttl(name)", "expiré : personne ne l'a renouvelé depuis plus d'un TTL", GREEN),
    ("—", "sinon", "OCCUPÉ : quelqu'un de vivant tient la place", RED),
]
y = y0
for n, cond, why, col in rows:
    T(L, y + 8, n, 16, col)
    box(L + 40, y, 700, cond, [], col, GREEN_BG if col == GREEN else RED_BG, h=44, title_size=13)
    T(L + 760, y + 12, why, 12, GREY)
    y += 54

box(L + 1440, y0, 1020, "lease_holder_is_dead_locally() — DEUX signaux, tous les deux obligatoires",
    ["signal 1   lease$host == conf$instance_host              « le lease prétend être tenu ici »",
     "signal 2   read_running_task_pid_file(name) == lease$pid « le fichier PID de CETTE machine est d'accord",
     "                                                          sur QUEL processus c'est »",
     "puis       !is_r_pid_running(lease$pid)                  « et ce processus n'existe plus »",
     "",
     "Pourquoi deux et pas un : deux conteneurs du MÊME service partagent EPISOMER_INSTANCE",
     "(docker/compose.yml le pose par service) tout en ayant des espaces de PID séparés. Avec le seul",
     "signal 1, « le PID 42 n'existe pas ici » ne voudrait pas dire « le titulaire est mort » — et on",
     "volerait un lease VIVANT, la seule chose que tout ce mécanisme existe pour empêcher.",
     "Le signal 2 ferme la porte : le fichier PID est écrit par le titulaire sur le disque qu'on est en",
     "train de lire (conf$data_dir est local au conteneur, seul jobs/ est partagé). S'il est d'accord,",
     "ce PID est vraiment à nous de tester.",
     "",
     "Tout échoue en « OCCUPÉ » : manquant, illisible ou simplement différent → on fait confiance au mtime."],
    ORANGE, ORANGE_BG)

box(L + 1440, y0 + 300, 1020, "Ce que ce raccourci achète",
    ["Sans lui, un runner qui plante a l'air vivant pendant tout un TTL — 30 minutes pour detect.",
     "Ce serait une RÉGRESSION par rapport au fichier PID que le lease reprend : dans le déploiement",
     "mono-machine (le cas de presque tout le monde), l'app Shiny afficherait « running » longtemps après",
     "la mort de la boucle, et redémarrer le service demanderait d'attendre le TTL.",
     "",
     "Quand le titulaire est un processus à nous, on peut faire beaucoup mieux qu'attendre : demander au",
     "système d'exploitation s'il existe encore. C'est le seul raccourci autorisé, et il exige une PREUVE",
     "locale, pas une supposition."], GREEN, GREEN_BG)
Y = max(y, y0 + 430) + 40

# =================================================================================================
# 8 · le heartbeat
# =================================================================================================
Y = panel(L, Y, W, "8 · Le battement de cœur — ne jamais se taire plus longtemps que son TTL",
          "Renouveler est facile. Le difficile est de trouver OÙ le faire : une boucle passe l'essentiel de son temps à ne pas boucler.")

y0 = Y
box(L + 860, y0, 700, "renew_runner_lease(name)",
    ["assign(name, Sys.time(), envir = lease_last_renewal)   ← comptabilité locale",
     "if (!epifile.exists(path)) return(FALSE)               ← test NON redondant",
     "epifile.touch(path)                                    ← un tout petit POST",
     "",
     "Le test d'existence est indispensable : epifile.touch() CRÉE un fichier manquant.",
     "Toucher un lease rendu, ou déplacé par quelqu'un d'autre, laisserait un fichier",
     "vide et non-JSON sous runners/ et polluerait le listing que lit l'app Shiny.",
     "Rien n'est renouvelé quand le lease est parti ; l'appelant l'apprend par",
     "stop_if_lease_lost()."], VIOLET, VIOLET_BG)

yb = y0 + 220
callers = [
    ("renew_runner_lease_if_due(name, span)", VIOLET,
     ["« je suis occupé mais je repasse souvent ici »",
      "heartbeat_is_due() lit lease_last_renewal (un",
      "environnement de paquet) : au plus UN touch par",
      "runner_heartbeat_span, quel que soit le nombre",
      "d'appels. Coût habituel : une lecture en mémoire.",
      "Appelé en tête de chaque tour de boucle."]),
    ("sleep_with_heartbeat(seconds, name, span)", VIOLET,
     ["« je ne vais rien faire pendant longtemps »",
      "deadline calculée UNE fois (sinon les",
      "renouvellements feraient dériver l'attente), puis",
      "   Sys.sleep(min(reste, span))",
      "   renew_runner_lease_if_due()",
      "   stop_if_lease_lost()      ← aussi important",
      "Remplace Sys.sleep() dans search / scheduler / alert."]),
    ("heartbeat_before_retry(seconds)", RED,
     ["Le rappel `after =` de httr2::req_retry().",
      "La SEULE attente que sleep_with_heartbeat() ne peut",
      "pas atteindre : sur un 429, httr2 dort À L'INTÉRIEUR",
      "de req_perform(). Une fenêtre Mastodon = 5 min =",
      "exactement runner_ttl_search.",
      "attente > span → on dort nous-mêmes, découpé, et on",
      "renvoie 0 pour que httr2 réessaie tout de suite ;",
      "sinon → on renouvelle et on laisse son backoff."]),
    ("renew_fs_runner_lease()", ORANGE,
     ["fs est le seul service incapable de battre pour",
      "lui-même : fs_loop() prend le lease puis passe la",
      "main à spark_job(), qui ne revient jamais dans R.",
      "Le moniteur fs (une future, boucle de 2 s) bat à sa",
      "place — mais SEULEMENT si /ping répond, et seulement",
      "quand un battement est dû. Un fs bloqué laisse donc",
      "bien son lease expirer."]),
]
cw = 600
for i, (name, col, lines) in enumerate(callers):
    box(L + i * (cw + 20), yb, cw, name, lines, col, h=180, title_size=13, line_size=11)
    A([(L + i * (cw + 20) + cw / 2, yb), (L + 1210, y0 + 190)], col, dashed=True)

y2 = yb + 210
box(L, y2, 1180, "Ce qu'on NE fait PAS : un batteur en tâche de fond",
    ["Un processus séparé qui toucherait le fichier toutes les 30 secondes serait beaucoup plus simple à écrire.",
     "Il crierait aussi « je suis vivant » pendant que la boucle principale est bloquée — c'est-à-dire exactement",
     "la panne que le lease est censé révéler.",
     "Ici, un renouvellement n'arrive JAMAIS que parce que la boucle a elle-même atteint la ligne qui renouvelle."],
    RED, RED_BG)

box(L + 1240, y2, 1220, "lease_last_renewal — de la comptabilité, pas une vérité",
    ["Un environnement de paquet : une entrée par nom de runner, la date de la dernière TENTATIVE.",
     "· LOCAL uniquement. La fraîcheur reste décidée par le mtime sur la machine fs, jamais par ce qu'un runner",
     "  croit de lui-même.",
     "· Il note la tentative, PAS le succès : un lease disparu ne reviendra pas tout seul, inutile de retester",
     "  son existence à chaque tour. Le perdre est le travail de stop_if_lease_lost(), aux points de garde."],
    GREY)

y3 = y2 + 130
T(L, y3, "Une minute de la vie d'un runner search (TTL 300 s, span 30 s)", 13, BLACK)
ty = y3 + 30
LINE([(L, ty), (L + 1800, ty)], color=GREY, width=2)
for i in range(0, 13):
    tx = L + i * 150
    LINE([(tx, ty - 8), (tx, ty + 8)], color=GREY, width=2)
    T(tx - 12, ty + 14, "%d s" % (i * 30), 11, GREY)
    if i:
        T(tx - 22, ty - 30, "touch", 11, VIOLET)
T(L + 1830, ty - 8, "… le TTL (300 s) n'est atteint qu'après DIX battements manqués d'affilée.", 12, RED)
Y = y3 + 80

# =================================================================================================
# 9 · les silences
# =================================================================================================
Y = panel(L, Y, W, "9 · Tous les silences possibles, et qui les couvre",
          "La question à se poser en relisant : « existe-t-il un chemin de code où ce processus peut se taire plus longtemps que son TTL ? »")

Y = table(L, Y, [640, 400, 700, 720],
          ["le silence", "durée", "couvert par", "note"],
          [["attente du prochain créneau de collecte", "jusqu'à collect_span, 60 min", "sleep_with_heartbeat()  (R/search.R:177)", "TTL search = 300 s : un Sys.sleep() nu tuait le lease"],
           ["attente d'une limite de débit (429)", "Mastodon : 5 min", "heartbeat_before_retry(), dans `after =`", "httr2 dort dans req_perform() : hors d'atteinte autrement"],
           ["une longue tâche de detect", "jusqu'à des heures", "TTL de 30 min + renouvellement en tête de boucle", "generate_alerts() rend compte via update_alerts_task()"],
           ["sommeils du scheduler / des alertes", "variables", "sleep_with_heartbeat()  (scheduler.R:293, alert.R:601)", "mêmes remplacements que dans search"],
           ["fs : spark_job() ne revient jamais dans R", "infinie", "renew_fs_runner_lease(), depuis le moniteur", "et seulement tant que /ping répond"],
           ["le parent `search` bloqué dans parLapply()", "infinie", "RIEN — le lease devient stale sur une machine saine", "assumé (§9.5) : l'exclusivité utile est sur search.<réseau>"]],
          color=VIOLET)
Y += 20
box(L, Y, 1180, "Deux Sys.sleep() laissés tels quels, volontairement",
    ["R/search.R : 1 seconde quand un tour n'a fait aucune requête.",
     "R/scheduler.R:44 : 5 secondes avant de relire un tasks.json qu'il n'a pas su analyser.",
     "Les deux sont plus courts que n'importe quel span : les découper n'ajouterait que du bruit,",
     "et le renouvellement en tête de boucle les couvre déjà."], GREEN, GREEN_BG)
box(L + 1240, Y, 1220, "Et pourtant detect_loop() passe quand même par sleep_with_heartbeat() pour 5 secondes",
    ["Pas pour le renouvellement — 5 s est très en dessous du span — mais pour le stop_if_lease_lost() qu'il apporte.",
     "Une boucle detect au repos remarque ainsi qu'on lui a pris sa place PENDANT qu'elle est au repos,",
     "au lieu de l'apprendre à sa prochaine écriture."], GREEN, GREEN_BG)
Y += 130

# =================================================================================================
# 10 · les gardes
# =================================================================================================
Y = panel(L, Y, W, "10 · Garder les écritures — stop_if_lease_lost() avant, run_until_lease_lost() autour",
          "C'est ici qu'est la vraie sûreté du mécanisme. Le claim empêche l'erreur au démarrage ; le garde empêche la corruption pendant le travail.")

y0 = Y
box(L, y0, 1180, "stop_if_lease_lost(name)  —  avant chaque écriture qui compte",
    ["if (!i_hold_runner_lease(name))  rlang::abort(class = \"episomer_lease_lost\", runner = name)",
     "",
     "Une condition TYPÉE, pas un stop() nu : les boucles doivent pouvoir distinguer « j'ai perdu ma place »",
     "(normal, attendu, sortie propre) d'une vraie erreur. Même idiome que stop_if_no_config() / stop_if_no_fs(),",
     "ce qui rend les points d'appel auto-documentés.",
     "",
     "Les points de garde :",
     "   search_loop_worker()   avant commit_posts(), avant save_config(), avant register_series()",
     "   search_topic()         APRÈS la requête réseau, avant l'envoi des posts (geolocate=true)",
     "   detect_loop()          avant CHAQUE appel de save_tasks(), avant ET après generate_alerts()",
     "",
     "name vaut NULL dans l'app Shiny et dans une boucle sandboxed → la fonction ne fait rien. C'est voulu."],
    RED, RED_BG)

box(L + 1240, y0, 1220, "run_until_lease_lost(expr)  —  en tête de chaque boucle",
    ["tryCatch(expr, episomer_lease_lost = function(cnd) { msg(...) ; \"lease-lost\" })",
     "",
     "Le miroir de stop_if_lease_lost(). En faire une fonction plutôt qu'un tryCatch() recopié trois fois",
     "garde les trois règles au même endroit :",
     "   1. écrire une ligne de log explicite ;",
     "   2. renvoyer le sentinelle \"lease-lost\" (et pas propager une erreur, qu'un superviseur lirait",
     "      comme un plantage à redémarrer immédiatement contre une instance parfaitement saine) ;",
     "   3. NE RIEN RENDRE. Le fichier sous runners/ appartient maintenant au nouveau titulaire ;",
     "      le rendre déplacerait SON fichier.",
     "",
     "L'on.exit(release_runner_lease()) déjà présent dans les boucles reste inoffensif pour la même raison :",
     "release_runner_lease() vérifie i_hold_runner_lease() d'abord et refuse."], GREEN, GREEN_BG)

y1 = y0 + 290
box(L, y1, 1180, "Deux subtilités qui reviennent à chaque relecture",
    ["· Dans search_topic(), le garde est APRÈS la requête réseau, pas avant. La requête n'écrit rien : elle ne",
     "  dépense qu'un appel du quota de quelqu'un d'autre. Tout ce qui suit cette ligne écrit — et la prise de",
     "  pouvoir peut très bien arriver PENDANT la requête, qui attend des minutes sur un 429.",
     "  Vérifier le plus tard possible est donc à la fois moins cher et plus strict.",
     "",
     "· save_tasks() n'est PAS gardé de l'intérieur, et un test le dit explicitement. La tentation de garder",
     "  l'écrivain plutôt que ses points d'appel revient à chaque lecture ; elle est fausse parce que l'app",
     "  Shiny appelle save_tasks() aussi, et ne détient aucun lease. Le garde y avorterait l'application dès",
     "  qu'un service prendrait un lease."], ORANGE, ORANGE_BG)

box(L + 1240, y1, 1220, "Le piège attrapé pendant l'issue 5",
    ["Le tryCatch() qui entoure search_topic() dans search_loop_worker() ré-émettait tout ce qui n'était pas",
     "\"too-old\" avec stop(paste(e$message, e)) — ce qui APLATIT la condition en erreur ordinaire et perd la",
     "classe episomer_lease_lost.",
     "Ce chemin est devenu atteignable : heartbeat_before_retry() peut lever la condition depuis l'intérieur",
     "de req_perform(), pendant une attente de limite de débit.",
     "Le worker s'arrêtait quand même (rien n'était corrompu) mais sur une erreur inexpliquée au lieu d'une",
     "sortie propre. La branche episomer_lease_lost est désormais testée EN PREMIER, avant les branches",
     "\"too-old\" qui, elles, filtrent sur le MESSAGE — ce qui la rend indépendante du texte."], RED, RED_BG)

y2 = y1 + 220
T(L, y2, "Les trois façons de finir", 16, BLACK)
y2 += 30
ends = [
    ("A · arrêt propre", GREEN, GREEN_BG,
     ["on.exit(unregister_runner())",
      "   → release_runner_lease() : le fichier est DÉPLACÉ vers runners/old/",
      "   → puis le <name>.PID est retiré (s'il nomme encore ce processus)",
      "Reprise ailleurs : quelques secondes."]),
    ("B · lease perdu (on m'a pris ma place)", VIOLET, VIOLET_BG,
     ["stop_if_lease_lost() lève → run_until_lease_lost() attrape",
      "   → une ligne de log, retour \"lease-lost\", RIEN n'est rendu",
      "   → l'on.exit refuse de toute façon (i_hold_runner_lease() est faux)",
      "Le nouveau titulaire collecte déjà : il n'y a rien à faire."]),
    ("C · mort brutale (kill, machine coupée)", RED, RED_BG,
     ["Le TUÉ ne rend rien : R n'a pas de gestionnaire de SIGTERM, aucun on.exit().",
      "   → SI c'est nous qui avons tué : le TUEUR rend pour lui (panneau 16)",
      "   → sinon, même machine : lease_holder_is_dead_locally() → immédiat (panneau 7)",
      "   → sinon (machine coupée, `compose down`) : le TTL, par construction."]),
]
for i, (t, col, bg, lines) in enumerate(ends):
    box(L + i * 820, y2, 780, t, lines, col, bg, h=130, title_size=14, line_size=12)
Y = y2 + 160

# =================================================================================================
# 11 · release
# =================================================================================================
Y = panel(L, Y, W, "11 · Rendre sa place — release_runner_lease() et unregister_runner()",
          "Rendre n'est pas obligatoire pour la correction : c'est ce qui fait la différence entre une bascule en quelques secondes et une bascule en un TTL.")

y0 = Y
x = L
y = box(x, y0, 620, "release_runner_lease(name = conf$runner_name)", [], GREEN, GREEN_BG, h=44, title_size=13)
A([(x + 310, y), (x + 310, y + 26)])
y += 26
y = box(x, y, 620, "name est NULL ?", ["l'app Shiny, une boucle sandboxed, la console"], GREY, h=56, title_size=13, line_size=11)
A([(x + 620, y - 28), (x + 740, y - 28)], GREY)
T(x + 750, y - 40, "→ FALSE, silencieusement. Ce n'est pas une erreur.", 12, GREY)
A([(x + 310, y), (x + 310, y + 26)])
y += 26
y = box(x, y, 620, "i_hold_runner_lease(name) ?", ["LE point important de toute la fonction"], RED, RED_BG, h=56, title_size=13, line_size=11)
A([(x + 620, y - 28), (x + 740, y - 28)], RED)
box(x + 750, y - 66, 900, "NON → message + FALSE, et surtout : on ne touche à rien",
    ["Une instance qui a silencieusement perdu son lease et qui « rendrait » quand même",
     "emporterait le fichier du NOUVEAU titulaire.",
     "C'est aussi pour ça que le gestionnaire de episomer_lease_lost ne rend rien."], RED, RED_BG)
A([(x + 310, y), (x + 310, y + 26)])
y += 26
y = box(x, y, 620, "epifile.move(runners/<name>.json → runners/old/<name>.<instance_id>.json)",
        ["runner_lease_old_dir(create = TRUE) d'abord"], GREEN, GREEN_BG, h=60, title_size=12, line_size=11)

box(L + 1720, y0, 740, "Pourquoi DÉPLACER et pas supprimer",
    ["L'API fs n'a pas d'action `delete`.",
     "",
     "Et déplacer est mieux de toute façon : cela garde",
     "« le fichier est absent » comme sens UNIQUE de",
     "« ce runner est libre ».",
     "",
     "Bonus : runners/ ne liste que les vivants (ce que",
     "lit le tableau Shiny), et runners/old/ reste une",
     "trace lisible après coup — le mtime d'un fichier",
     "rendu est l'heure de la libération.",
     "",
     "L'instance_id dans le nom évite que deux",
     "libérations successives du même runner s'écrasent."], ORANGE, ORANGE_BG)

y1 = y + 40
box(L, y1, 1600, "unregister_runner(name) = release_runner_lease(name) + retrait du <name>.PID",
    ["Les deux couches doivent partir, pour deux raisons différentes :",
     "   · le lease, pour qu'une autre machine puisse reprendre tout de suite au lieu d'attendre le TTL ;",
     "   · le fichier PID, parce qu'il décrit CE processus, qui se termine.",
     "",
     "Ce dernier point vient d'un vrai bug observé : après la mort d'un worker search, le lease disait « libre » pendant que le",
     "fichier PID nommait encore un processus vivant (un nœud PSOCK au repos dans son cluster parent). Les deux couches",
     "répondaient différemment sur le même runner, et le redémarrage local de ce seul runner était refusé… par un runner que",
     "personne ne faisait tourner.",
     "",
     "Le PID n'est retiré que s'il nomme encore CE processus : si un autre processus local a repris l'enregistrement, le fichier",
     "est le sien, pas le nôtre."], BLUE, BLUE_BG)
Y = max(y1 + 190, y0 + 320) + 40

# =================================================================================================
# 12 · qui tourne
# =================================================================================================
Y = panel(L, Y, W, "12 · « Est-ce que ça tourne ? » — deux questions, ne jamais les confondre",
          "Depuis l'issue 7, is_*_running() lit le lease. Tout ce qui TUE un processus doit, lui, continuer à lire le PID local.")

y0 = Y
box(L, y0, 1180, "Question CLUSTER — « quelqu'un, quelque part, fait-il ce travail ? »",
    ["is_runner_running(name)  ==  runner_lease_status(name)$status == \"running\"",
     "",
     "is_search_running()    lit le parent `search` ET chaque search.<réseau> actif, et répond oui si",
     "                       L'UN d'eux est frais. Lire `search` seul aurait été la traduction évidente,",
     "                       et fausse : search_loop() le prend puis se bloque dans parLapply(), sans plus",
     "                       aucun tour pour le renouveler. L'app aurait affiché « Stopped » cinq minutes",
     "                       après chaque démarrage. Les workers, eux, battent vraiment ; et le parent tout",
     "                       juste pris couvre la fenêtre de démarrage avant leur enregistrement.",
     "is_detect_running()    is_runner_running(\"detect\") — un seul nom, renouvelé à chaque tour",
     "missing_search_jobs()  les réseaux actifs dont le lease n'est PAS frais (un réseau collecté ailleurs",
     "                       n'est pas « manquant »)",
     "is_fs_running()        EXCEPTION : un GET /ping, pas le lease. Instantané, sans TTL, et il dit si le",
     "                       service RÉPOND plutôt que si quelqu'un tient son lease. fs prend quand même un",
     "                       lease, mais pour une autre raison : empêcher une 2ᵉ base d'ouvrir le même stockage.",
     "",
     "Posée par : l'app Shiny (pure lectrice), check_all(), les avertissements — dont le texte a changé :",
     "« the pipeline is not running » veut maintenant dire « sur AUCUNE machine partageant cette base »."],
    BLUE, BLUE_BG)

box(L + 1240, y0, 1220, "Question LOCALE — « y a-t-il ici quelque chose à tuer ? »",
    ["is_runner_running_locally(name)  ==  get_running_task_pid(name) >= 0",
     "",
     "Posée par :",
     "   stop_all_runner_tasks()      arrêter, c'est tuer un processus ICI",
     "   stop_search_runner_task() / stop_detect_runner_task() / stop_fs_runner_task()",
     "   le menu « services » de setup()",
     "",
     "Le menu de setup() est passé au statut LOCAL pour cette raison : ses deux actions",
     "(register_*_runner_task / stop_*_runner_task) démarrent et tuent des processus sur",
     "cette machine seulement. Afficher « Stop search loop (currently running) » parce",
     "qu'une AUTRE machine tient le lease proposerait d'arrêter quelque chose qui n'est",
     "pas là. Un démarrage local que le lease refusera le dira, dans le log du processus",
     "qu'il lance.",
     "",
     "fs garde son /ping ici aussi.",
     "",
     "Trois extractions faites pour que le code du lease teste un PID EXACTEMENT comme le",
     "fait la couche PID : get_running_task_pid_path(), read_running_task_pid_file()",
     "(qui distingue « pas de fichier » de « processus mort », contrairement au -1 unique",
     "de get_running_task_pid()) et is_r_pid_running()."], GREEN, GREEN_BG)
Y = y0 + 380

# =================================================================================================
# 13 · le tableau shiny
# =================================================================================================
Y = panel(L, Y, W, "13 · Le tableau de l'app Shiny, et le garde de l'interface",
          "Sans lui, un lease perdu est invisible et l'opérateur doit lire les logs de trois machines. C'est ce qui rend tout le reste débogable.")

y0 = Y
x = L
y = box(x, y0, 620, "runner_lease_names()",
        ["epilist.files(runners/, pattern = \"\\\\.json$\")",
         "Le listing n'est pas récursif, donc runners/old/ y",
         "apparaît comme une entrée de répertoire : c'est le",
         "motif .json qui l'exclut. Un dossier absent renvoie",
         "une liste vide → aucun cas particulier."], BLUE, BLUE_BG)
A([(x + 310, y), (x + 310, y + 26)]); y += 26
y = box(x, y, 620, "sort_runner_names()",
        ["fs, fs_mon, search, detect, puis alphabétique.",
         "L'alphabétique place `search` juste avant ses",
         "enfants search.<réseau> — l'ordre de lecture qui",
         "rend évidente la répartition de la collecte."], BLUE, BLUE_BG)
A([(x + 310, y), (x + 310, y + 26)]); y += 26
y = box(x, y, 620, "runner_leases()",
        ["un runner_lease_status() par nom.",
         "Le tableau est construit sur les STATUTS, pas sur le",
         "JSON brut : le contre-contrôle local de l'issue 7 y",
         "est déjà, donc « crashed here » sort gratuitement."], BLUE, BLUE_BG)
A([(x + 310, y), (x + 310, y + 26)]); y += 26
y = box(x, y, 620, "runner_leases_df()",
        ["Écarte les lignes redevenues \"free\" : un lease peut",
         "disparaître entre le listing et sa lecture (rendu, ou",
         "repris par une autre instance qui a déplacé le fichier).",
         "Renvoie un data.frame vide MAIS avec ses colonnes."], BLUE, BLUE_BG)

box(L + 700, y0, 900, "Les cinq colonnes",
    ["Runner      fs, search, search.bluesky, detect…",
     "Status      running · stale (no heartbeat for 5 min) · crashed here",
     "Host        machine-B, ou « machine-A (this machine) »",
     "Last seen   l'âge du mtime — la seule quantité qui décide",
     "Since       claimed_at, lu dans le JSON, en UTC",
     "",
     "Pas de ligne = libre. Un runner rendu n'a plus de fichier (panneau 11),",
     "un runner jamais pris n'en a jamais eu : les deux se lisent pareil, et",
     "c'est voulu. Le tableau dit qui TIENT quelque chose, pas ce qu'on",
     "pourrait démarrer — ça, ce sont les trois libellés Running/Stopped",
     "au-dessus de lui.",
     "",
     "Quand rien ne tient rien, le message `emptyTable` de DataTables le dit",
     "en toutes lettres plutôt que d'afficher une grille vide.",
     "",
     "C'est un bloc de STATUT, pas une table de données : dom = \"t\", pas de",
     "pagination, pas de tri — un clic qui réordonne ne ferait que casser",
     "l'ordre de lecture ci-contre. Rafraîchi avec le reste du bloc `tasks`,",
     "toutes les 10 secondes."], BLUE, BLUE_BG)

box(L + 1680, y0, 780, "Lire les statuts",
    ["running        renouvelé il y a moins d'un TTL",
     "",
     "stale (…)      personne n'a renouvelé depuis plus d'un",
     "               TTL. Titulaire PRÉSUMÉ mort. Le libellé",
     "               nomme le TTL dépassé, pour qu'on sache",
     "               ce qu'on attend.",
     "",
     "crashed here   holder_dead_locally : ce n'est plus une",
     "               présomption mais une CERTITUDE, sur un",
     "               processus à nous. Récupérable tout de",
     "               suite. Cacher la différence derrière un",
     "               seul mot gâcherait le drapeau que",
     "               l'issue 7 a ajouté exprès.",
     "",
     "Un long « Since » à côté d'un court « Last seen » =",
     "un runner en bonne santé. Les deux colonnes ne se",
     "lisent qu'ensemble.",
     "",
     "Faux problème connu : sur une machine saine, `search`",
     "passe en stale quelques minutes après le démarrage",
     "pendant que ses enfants restent running. Ce sont les",
     "enfants qu'il faut croire (panneau 9)."], GREY)

y1 = max(y, y0 + 480) + 30
box(L, y1, 2460, "loop_run_issues(loop_name) — le garde de l'interface : la modale BLOQUE l'activation, elle n'informe pas seulement",
    ["Le contrôle est tout en HAUT de la fonction, avant get_tasks() : rien dans l'état des tâches locales ne pourrait faire réussir ce démarrage, donc les interroger",
     "d'abord ne ferait qu'apporter une réponse plus précise à la mauvaise question. Le nom de runner de chacune des trois boucles est le nom de la boucle elle-même,",
     "d'où une seule ligne pour fs, search et detect.",
     "",
     "runner_running_elsewhere_message(name) nomme l'hôte, l'heure de prise, l'âge du dernier battement et le TTL — c'est-à-dire quoi faire ensuite.",
     "Elle est volontairement SILENCIEUSE dans deux cas, et les deux ressemblent à des oublis quand on lit vite (ils sont testés pour ça) :",
     "   · lease STALE sur une autre machine → c'est exactement ce pour quoi le standby existe, le claim va réussir ;",
     "   · lease tenu par NOTRE hôte → le démarrage est une affaire locale, et le contrôle PID de register_runner() donne un message strictement meilleur : il nomme le processus à arrêter.",
     "",
     "Trouvé en chemin (issue 8, câblé en issue 9) : le bouton « activate » du service d'alertes n'appelait pas loop_run_issues() du tout. C'était le dernier bouton capable de lancer une boucle detect",
     "condamnée. Le contrôle y est maintenant placé AVANT le reste du gestionnaire, parce que ce reste ÉCRIT : il date les tâches ponctuelles et sauvegarde la configuration, que la boucle déjà en",
     "cours sur l'autre machine ramasserait aussitôt."], ORANGE, ORANGE_BG)
Y = y1 + 250

# =================================================================================================
# 14 · scénario
# =================================================================================================
Y = panel(L, Y, W, "14 · Le scénario complet — deux collecteurs, une base",
          "docker compose --profile standby up -d   ·   le service `search` et le service `search-standby` de docker/compose.yml")

y0 = Y
steps = [
    ("t = 0", "machine B démarre", GREEN, GREEN_BG,
     ["search_loop() → register_search_runner() → claim `search` : libre → pris",
      "puis chaque worker → claim `search.bluesky` / `search.mastodon.<hôte>` → pris",
      "→ la collecte commence."]),
    ("t = 0+", "machine C démarre", RED, RED_BG,
     ["read `search` → vivant → lease_is_free() = FALSE → claim = FALSE",
      "→ remove_running_task_pid() puis stop(runner_lease_holder_message())",
      "→ R sort avec un code non nul, Docker relance le conteneur (restart: on-failure)",
      "→ C réessaie. CETTE BOUCLE DE RÉESSAI EST LE STANDBY : un read d'un petit",
      "   fichier par tentative, rien d'autre."]),
    ("toutes les 30 s", "B bat", VIOLET, VIOLET_BG,
     ["renew_runner_lease() → epifile.touch() → le mtime avance.",
      "C relit à chaque tentative : toujours frais, toujours FALSE."]),
    ("arrêt PROPRE", "docker compose stop search", GREEN, GREEN_BG,
     ["B : on.exit → unregister_runner() → le fichier part vers runners/old/, le PID est retiré.",
      "C : à sa tentative suivante, read → absent → LIBRE → write + 5 s de guard + relecture",
      "    → c'est bien mon instance_id → TRUE → C collecte.",
      "Bascule : quelques secondes."]),
    ("arrêt BRUTAL", "docker compose kill search  (ou machine coupée, réseau perdu)", RED, RED_BG,
     ["Rien n'est rendu. Le fichier reste, son mtime se fige.",
      "C : tant que age <= 300 s → vivant → FALSE.",
      "    dès que age > 300 s   → LIBRE → write + guard + relecture → C collecte.",
      "Bascule : le TTL. C'est la latence de bascule, par construction — ne PAS baisser le TTL pour l'améliorer."]),
    ("cas particulier", "B et C sur la MÊME machine", ORANGE, ORANGE_BG,
     ["lease_holder_is_dead_locally() : host identique + le <name>.PID local nomme le même PID + ce PID est mort",
      "→ le lease est libre IMMÉDIATEMENT, sans attendre le TTL (panneau 7).",
      "C'est ce qui garde le déploiement mono-machine aussi réactif qu'avant les leases."]),
]
y = y0
for tag, title, col, bg, lines in steps:
    T(L, y + 12, tag, 13, col)
    y = box(L + 200, y, 2260, title, lines, col, bg, title_size=14, line_size=12) + 14
Y = y + 30


# =================================================================================================
# 15 · attendre au lieu de mourir (issue 14)
# =================================================================================================
Y = panel(L, Y, W, "15 · Un service ATTEND sa place, il n'en meurt pas — register_runner(name, wait = TRUE)",
          "Trouvé en lançant le déploiement que l'issue 13 venait de déclarer hors périmètre : make compose-up · make compose-down · make compose-up. Le second démarrage ne collectait plus jamais rien.")

y0 = Y
y_a = box(L, y0, 820, "Le symptôme, et pourquoi RIEN ne réessayait",
    ["jobs/search.bluesky.log, au second démarrage :",
     "   [ERROR]: the search worker for bluesky stopped: Runner",
     "   'search.bluesky' is already held by another instance",
     "   (hzklet9244e724fe) … last seen 165 seconds ago (TTL: 300).",
     "",
     "Le refus lui-même est le comportement documenté de l'issue 13.",
     "Ce qui n'était PAS voulu : après les 300 secondes, rien. Jamais.",
     "",
     "1 · register_runner() refuse par stop() — il demande UNE fois.",
     "    Juste pour un appelant qui doit répondre maintenant (l'app",
     "    Shiny, le moniteur fs) ; faux pour un service, dont le métier",
     "    est justement d'être là.",
     "2 · le raise remonte au tryCatch() du parLapply de search_loop(),",
     "    le worker rend \"error: …\", et une fois tous les workers dans",
     "    cet état report_search_loop_outcomes() lève à son tour :",
     "    Rscript sort non-zéro, le conteneur s'arrête.",
     "3 · docker/compose.yml ne portait AUCUNE politique restart:.",
     "    La boucle de réessai que le plan décrit comme LE standby",
     "    n'existait que sur search-standby, dans l'autre fichier",
     "    Compose. Le déploiement que tout le monde utilise n'avait",
     "    donc ni l'une ni l'autre des deux façons de réessayer."], RED, RED_BG)

y_b = box(L + 850, y0, 790, "wait_for_runner_lease(name, timeout, interval)",
    ["Claim ; sur un refus, re-claim toutes les `interval` secondes",
     "jusqu'au succès ou à l'épuisement du `timeout`.",
     "",
     "Un read d'un petit JSON par tentative, un Sys.sleep() nu entre",
     "deux : il n'y a AUCUN lease à renouveler pendant qu'on attend.",
     "",
     "runner_wait_timeout    Inf par défaut   (properties.json)",
     "runner_wait_interval   15 s             (properties.json)",
     "runner_wait_log_span   60 s             (constante du paquet)",
     "",
     "Messages partagés avec le refus :",
     "   runner_lease_holder_sentence()   qui tient, depuis quand",
     "   runner_lease_wait_message()      + l'intervalle, et la date",
     "                                    limite quand l'attente est",
     "                                    bornée — ce qu'un service",
     "                                    qui attend doit à son",
     "                                    opérateur.",
     "",
     "Tests : tests/testthat/test-runners_wait.R"], GREEN, GREEN_BG)

y_c = box(L + 1660, y0, 880, "Qui attend, qui ne doit pas attendre",
    ["wait = TRUE  — les trois boucles de service, et elles seules :",
     "   search_loop() et search_loop_worker()   R/search.R",
     "   fs_loop()                               R/fs.R",
     "   detect_loop()                           R/scheduler.R",
     "",
     "wait = FALSE (le défaut) — tout le reste :",
     "   l'app Shiny décidant si elle lance une tâche : bloquer sa",
     "   session serait pire que refuser ;",
     "   register_fs_monitor(), qui repasse de toute façon dans",
     "   2 secondes (mais voir le panneau 17 : sur une machine de",
     "   secours, cette non-attente lui coûte la vie).",
     "",
     "La couche PID locale n'est JAMAIS attendue, wait = TRUE ou pas.",
     "Un processus vivant de CETTE machine tenant déjà le runner,",
     "c'est le même service démarré deux fois : une erreur d'opérateur",
     "dont la réponse est un PID à tuer, pas un délai à subir."], BLUE, BLUE_BG)

y1 = max(y_a, y_b, y_c) + 30
box(L, y1, 2460, "Décidé en implémentant",
    ["· Inf est le timeout par DÉFAUT. « Le lease est pris » n'est pas un incident à signaler, c'est un rôle à tenir. La situation couverte est toujours l'une des deux : un prédécesseur tué sans",
     "  rien rendre — la bonne réponse est de démarrer dès la fin du TTL — ou une autre machine vivante — la bonne réponse est d'être son standby. Les deux se répondent « attends ».",
     "  Une valeur finie ramène l'attente au refus historique, pour un déploiement dont le superviseur préfère voir le service mourir ; et runner_wait_timeout = 0 EST exactement l'ancien comportement.",
     "",
     "· Les deux runner_wait_* sont de la politique LOCALE, pas un accord de cluster. Contrairement aux TTL du panneau 4, deux machines aux valeurs différentes ne cassent rien : combien de temps",
     "  *je* veux bien attendre est mon affaire. Elles ne vivent dans properties.json que pour se régler comme tout le reste. Inf fait l'aller-retour JSON sous la forme de la chaîne \"Inf\" — jsonlite ne",
     "  connaît pas l'infini — et revient nombre par le même as.numeric() que toutes les autres propriétés.",
     "",
     "· Attendre n'affaiblit l'exclusivité de RIEN. Une attente ne se termine que sur un claim_runner_lease() réussi, guard et relecture de confirmation comprises (panneau 6). Ce qui disparaît est un",
     "  refus, pas un contrôle.",
     "",
     "· `restart: on-failure` reste, DÉGRADÉ. Ce n'est plus ce qui fait d'un standby un standby — l'attente l'est — mais c'est encore le filet sous tout ce qu'une attente ne rattrape pas : un fs qui",
     "  plante, un worker search qui abandonne après search_max_request_failures() échecs consécutifs, un OOM kill. `on-failure` et pas `always`, pour qu'un conteneur sorti en 0 reste arrêté.",
     "  Il est maintenant posé sur fs, search et detect dans les DEUX fichiers Compose, et le bloc standby de compose_with_search_standby.yml a été réécrit : il décrivait un `docker compose stop`",
     "  comme libérant les leases via on.exit(), ce que l'issue 13 a établi qu'il ne fait pas."], VIOLET, VIOLET_BG)
Y = y1 + 290

# =================================================================================================
# 16 · le tueur rend pour le tué (issue 13)
# =================================================================================================
Y = panel(L, Y, W, "16 · Le TUÉ ne rend rien — c'est le TUEUR qui rend pour lui",
          "R n'installe aucun gestionnaire de SIGTERM : un Rscript qui en reçoit un meurt sans exécuter ni .Last ni un seul on.exit(). Or tout le §3.4 — toute la libération du panneau 11 — vit dans des on.exit().")

y0 = Y
y_a = box(L, y0, 1220, "Les deux défauts, trouvés en arrêtant puis relançant depuis le wizard",
    ["Le second démarrage de fs n'avait jamais lieu, et jobs/fs.log disait pourquoi : les trois leases étaient encore là, aucun",
     "déplacé vers runners/old/, nommant trois processus qui n'existaient plus. §3.4 et §3.6 avaient chacun été implémentés",
     "exactement comme écrits — et c'est ce qui rend les deux défauts intéressants.",
     "",
     "1 · on.exit() ne tourne pas quand un runner est TUÉ.",
     "    stop_all_runner_tasks() arrête un service par kill_task() → kill <pid>. La libération gracieuse couvrait donc les cas",
     "    dont personne ne se plaint (une boucle qui rend la main, un worker qui abandonne son réseau) et AUCUN de celui",
     "    pour lequel elle avait été écrite. Seul le TTL rendait ces leases : 2 min pour fs, 5 pour search, 30 POUR DETECT.",
     "",
     "2 · register_runner() détruisait la preuve dont la reprise rapide a besoin.",
     "    Il écrivait le fichier PID local AVANT de claim. Or ce PID file, laissé par le titulaire précédent, est exactement le",
     "    second des deux signaux que lease_holder_is_dead_locally() exige (panneau 7) : le lease nomme un PID, et seul",
     "    notre propre PID file prouve que ce PID était le nôtre. L'écraser d'abord transformait la comparaison en échec",
     "    garanti — le raccourci « le titulaire a planté ici, il est mort » ne pouvait JAMAIS partir depuis la seule fonction qui",
     "    démarre un runner. Il marchait partout ailleurs (le tableau Shiny lisait « crashed here » correctement), ce qui",
     "    explique qu'il ait eu l'air implémenté. Pire : le refus supprimait ensuite ce PID file, donc le premier redémarrage",
     "    raté détruisait la preuve pour tous les suivants."], RED, RED_BG)

y_b = box(L + 1240, y0, 1220, "Le correctif : déplacer la libération vers le seul processus encore vivant",
    ["release_dead_local_runner_lease(name)",
     "    Rend le lease d'un runner dont le titulaire est un processus de CETTE machine qui s'est arrêté. release_runner_lease()",
     "    (panneau 11) ne peut pas faire ce travail : elle rend le lease de l'appelant, et ici tout l'enjeu est que le titulaire",
     "    soit quelqu'un d'autre. Mêmes deux signaux, même lecture fail-closed : un lease d'une autre machine, d'un conteneur",
     "    qui partage notre nom d'hôte mais pas notre espace de PID, ou d'un titulaire encore vivant, est laissé STRICTEMENT",
     "    tranquille. C'est ce qui permet à l'appelant de balayer tout runners/ sans savoir ce qu'il va y trouver.",
     "",
     "release_dead_local_runner_leases()",
     "    Le même, sur un LISTING de runners/ — pas sur une liste de noms. stop_runner_task(\"search\") sait qu'il a tué",
     "    `search`, mais les leases qui comptent sont search.bluesky et search.mastodon.<hôte>, tenus par des nœuds PSOCK",
     "    dont les noms dépendent de la configuration. Le listing ramasse aussi fs_mon, et les leases d'un plantage PLUS",
     "    ANCIEN — ce que rien ne nettoyait jusque-là. Coût : un listing. Rien ici ne peut lever : cela tourne pendant un arrêt.",
     "",
     "release_stopped_runner_leases(names, timeout = 10)",
     "    Attend d'abord que les PID tués soient réellement partis. kill(1) rend la main quand le signal est DÉLIVRÉ, pas",
     "    quand le processus est mort : balayer trop tôt ne libérerait rien et remettrait tout le monde à attendre le TTL.",
     "    L'attente est bornée, et un runner qui lui survit garde son lease — le bon côté du doute, il écrit peut-être encore.",
     "",
     "register_runner() claim AVANT d'écrire le PID, et un refus laisse le disque local exactement comme il l'a trouvé.",
     "Tests : test-runners_lease.R (« Releasing the lease of a stopped local process ») et test-runners_register.R."], GREEN, GREEN_BG)

y1 = max(y_a, y_b) + 30
y2 = box(L, y1, 2460, "Et alors pourquoi `make compose-down` ne déplace-t-il RIEN dans runners/old/ ? Parce qu'aucune ligne de R ne tourne sur le chemin de la descente — et c'est assumé",
    ["1 · `docker compose down` envoie SIGTERM au PID 1 du conteneur, qui est `bash /episomer/devscripts/run.sh`.",
     "2 · run.sh lance `Rscript /episomer/devscripts/run.R` en ENFANT — pas en `exec` — et n'installe aucun `trap`. Un bash qui attend un enfant au premier plan ne relaie pas le signal : R ne le voit jamais.",
     "3 · Après le délai de grâce (10 s par défaut), Docker envoie SIGKILL à tout le conteneur. R meurt sur le coup.",
     "4 · Donc pas d'on.exit(unregister_runner()), donc pas de release_runner_lease(), donc aucun déplacement : les fichiers RESTENT dans runners/, leur mtime figé.",
     "",
     "Et corriger seulement l'étape 2 ne suffirait pas : même en recevant le SIGTERM, R n'a pas de gestionnaire pour lui. C'est le premier des deux défauts ci-contre, et il n'a pas de correctif « propre » côté paquet.",
     "",
     "Qui s'en remet comment, au `up` suivant :",
     "   fs                son EPI_HOME est monté, donc fs.PID SURVIT au conteneur → lease_holder_is_dead_locally() reconnaît le prédécesseur → reprise immédiate, sans attendre le TTL.",
     "   search · detect   ils ne montent que jobs/ : leur PID file meurt avec le conteneur, le second signal échoue fail-closed, et ils attendent leur TTL entier (300 s et 1800 s).",
     "                     C'est correct et voulu — deux conteneurs du même service ne peuvent toujours pas se voler leur lease — et c'est le wait = TRUE du panneau 15 qui rend l'attente inoffensive.",
     "",
     "Le correctif possible, non fait : un `trap` dans run.sh qui, après avoir tué son enfant et attendu sa mort, lance un R NEUF sur release_dead_local_runner_leases(). C'est le principe même de ce",
     "panneau — ce n'est pas le tué qui rend son lease, c'est un processus encore vivant. Deux réserves à lever avant : l'ordre d'arrêt de Compose (search et detect avant fs, donc leur runners/ derrière",
     "l'API HTTP de fs qui répond encore — vrai aujourd'hui, mais par accident de dépendances plutôt que par contrat), et un stop_grace_period explicite plutôt que les 10 s par défaut."], ORANGE, ORANGE_BG)

y3 = y2 + 30
y_d = box(L, y3, 1220, "L'ordre dans stop_runner_task(\"fs\") : fs_mon, PUIS les leases, PUIS le service Scala",
    ["Là où conf$fs_remote est posé — le défaut de setup_config(), donc toutes les boucles et l'app",
     "Shiny — runners/ vit DERRIÈRE le processus java qu'on est en train de tuer. Rendre le lease de",
     "fs après l'avoir tué est impossible : on le rend tant qu'il répond encore.",
     "",
     "Tuer le moniteur AVANT le fs qu'il surveille épargne en plus une alerte health_check() parasite,",
     "envoyée sur un fs qu'on est justement en train d'arrêter.",
     "",
     "Même raison pour le balayage de stop_all_runner_tasks(), placé avant l'arrêt du fs et non à la fin."], BLUE, BLUE_BG)

y_e = box(L + 1240, y3, 1220, "La trace sous runners/old/ garde le nom de l'instance qu'elle DÉCRIT",
    ["runner_lease_released_path() prend l'instance_id en paramètre au lieu de lire conf$instance_id.",
     "",
     "Sans cela, un lease balayé par une autre instance serait classé sous l'identité de celle qui l'a",
     "balayé, et non de celle qui le tenait : la seule chose pour laquelle ce dossier existe deviendrait",
     "illisible.",
     "",
     "Rappel du panneau 11 : le mtime d'un fichier de runners/old/ est l'heure de la libération, et",
     "l'instance_id dans le nom évite que deux libérations successives du même runner s'écrasent."], VIOLET, VIOLET_BG)
Y = max(y_d, y_e) + 60

# =================================================================================================
# 17 · le cas fs et son moniteur
# =================================================================================================
Y = panel(L, Y, W, "17 · Le cas fs — pourquoi UN service a DEUX runners, et ce que ça a coûté",
          "fs est le seul service dont le processus R disparaît de R. `fs_mon` n'est pas un doublon de `fs` : c'est l'autre processus.")

y0 = Y
y_a = box(L, y0, 1200, "fs — le service, qui cesse d'être du R",
    ["fs_loop() (R/fs.R) :",
     "   1 · lance le moniteur en future::future(multisession)",
     "   2 · register_fs_runner(wait = TRUE)   → claim le lease `fs`",
     "   3 · entre dans spark_job(), un system() qui lance le JVM",
     "       Scala/Lucene et NE REND JAMAIS la main à R tant que",
     "       le JVM vit.",
     "",
     "À partir de la ligne 3, le R principal du conteneur fs n'exécute",
     "plus une seule instruction. Il ne peut donc ni renouveler son",
     "propre lease, ni vérifier quoi que ce soit, ni rien rendre."], ORANGE, ORANGE_BG)

y_b = box(L + 1230, y0, 1230, "fs_mon — le seul R encore vivant du côté fs",
    ["Le future lancé à l'étape 1. Sa boucle de 2 secondes fait trois",
     "choses, et une seule concerne son propre lease :",
     "",
     "   health_check()             toutes les 60 s (R/check.R) — les",
     "                              alertes : posts non collectés depuis",
     "                              plus d'une heure, etc.",
     "   renew_fs_runner_lease()    il bat POUR le lease de `fs`",
     "                              (panneaux 4 et 8)",
     "   register_fs_monitor()      il claim SON PROPRE lease `fs_mon`",
     "",
     "Le claim repasse ici toutes les 2 s : c'est lui, et lui seul, qui",
     "justifie le chemin rapide de claim_runner_lease() (panneau 6)."], GREEN, GREEN_BG)

y1 = max(y_a, y_b) + 30
y_c = box(L, y1, 1220, "Pourquoi fs_mon a besoin de sa PROPRE identité (PID + lease)",
    ["Trois raisons, dont une non négociable.",
     "",
     "· Une CIBLE À TUER — la non négociable. stop_runner_task(\"fs\") fait kill_task(\"fs\", \"R\")",
     "  PUIS kill_task(\"fs_mon\", \"R\") (panneau 16). Le moniteur est un processus PSOCK séparé :",
     "  tuer le R principal ne le tue pas. Sans son propre fs_mon.PID il survivrait en ORPHELIN — et il",
     "  continuerait à battre pour le lease d'un fs mort, c'est-à-dire exactement le batteur en tâche de",
     "  fond que le panneau 8 rejette.",
     "",
     "· Exclusion mutuelle : deux moniteurs, c'est deux health_check() et deux sources de vérité sur",
     "  la vitalité de fs.",
     "",
     "· Visibilité : `fs` et `fs_mon` sont deux LIGNES distinctes du tableau (sort_runner_names(),",
     "  panneau 13). Cela sépare « le JVM répond » de « le batteur est vivant » — et voir ci-dessous",
     "  à quel point cette distinction n'est pas de la décoration."], BLUE, BLUE_BG)

y_d = box(L + 1240, y1, 1220, "Ce qui rend ce battement HONNÊTE malgré le panneau 8",
    ["Le panneau 8 rejette le batteur en tâche de fond, et fs en est un. Deux précautions, et deux",
     "seulement, en font une dérogation bornée plutôt qu'un mensonge :",
     "",
     "· il ne bat QUE si is_fs_running() répond 200 sur /ping — un fs mort ou bloqué laisse donc bien",
     "  son lease expirer, ce qui est toute la raison d'être d'un lease ;",
     "· l'appel /ping n'a lieu que quand un battement est DÛ, pas à chacun des tours de 2 secondes.",
     "",
     "Conséquence à savoir en lisant le tableau : le heartbeat de `fs` ne dit pas « mon processus R",
     "tourne », il dit « le JVM répond ». C'est aussi pourquoi is_fs_running() est l'exception du",
     "panneau 12 : un GET /ping, pas une lecture de lease.",
     "",
     "fs prend quand même un lease, mais pour une autre raison que la vitalité : empêcher une",
     "deuxième base d'ouvrir le même stockage Lucene."], VIOLET, VIOLET_BG)

y2 = max(y_c, y_d) + 30
y3 = box(L, y2, 2460, "Le bug que cette distinction a permis de trouver — 2026-09-01, en lisant runners/ pendant un make compose-up",
    ["Symptôme    pas de runners/fs_mon.json, pas de fs_mon.PID, et runners/fs.json avec renewed_at == claimed_at depuis 41 minutes — sur un fs parfaitement sain, /ping répondant, healthcheck vert.",
     "",
     "Cause       dans le future, setup_config(data_dir = data_dir) était appelé SANS fs_remote. Le défaut de setup_config() est EPI_HOST s'il est posé, sinon \"http://localhost:8080\" — et le conteneur fs",
     "            n'a pas de EPI_HOST (seuls search et detect en ont, docker/compose.yml). Le worker lisait donc properties.json en HTTP sur localhost:8080, une adresse où l'API n'écoute PAS : elle bind",
     "            conf$fs_url, soit \"http://fs\" via EPISOMER_OPTION_FS_URL, c'est-à-dire l'interface réseau du conteneur et pas la loopback. Le future mourait sur sa PREMIÈRE ligne.",
     "",
     "Invisible   fs_loop() ne collecte JAMAIS la valeur du future. Avec multisession, messages et erreurs sont capturés dans le worker et ne sont relayés qu'au value() — qui n'arrive jamais.",
     "            Zéro ligne de log pendant 41 minutes, pendant que les DEUX rôles du moniteur étaient morts : aucun health_check(), et le lease `fs` non renouvelé sur un service vivant.",
     "",
     "Correctif   setup_config(data_dir = data_dir, fs_remote = NULL) — exactement ce que fait déjà la branche `data_dir` de fs_loop() juste au-dessus. Le worker tourne SUR la machine fs :",
     "            /episomer-data est son propre disque, et l'API n'est de toute façon pas encore levée quand il démarre.",
     "",
     "À retenir   `fs` frais et `fs_mon` absent du tableau du panneau 13 était le SEUL endroit où ce bug se voyait. C'est ce que la troisième raison ci-dessus achète.",
     "            Et la leçon plus générale : un future dont personne ne collecte la valeur est un processus qui peut mourir en silence. Le rendre observable (callr avec stdout redirigé, ou un",
     "            resolved() non bloquant dans la boucle principale de fs_loop() pour le relancer) est un changement de conception, pas encore fait."], RED, RED_BG)

y4 = y3 + 30
box(L, y4, 2460, "Reste ouvert : sur une VRAIE machine de secours, le moniteur meurt pour de bon",
    ["Dans fs_loop(), le future est créé AVANT register_fs_runner(wait = TRUE). Et register_fs_monitor() claim avec wait = FALSE, le défaut (panneau 15).",
     "",
     "Sur une machine B en attente pendant que A tient fs : le moniteur de B meurt immédiatement — le lease fs_mon est tenu, vivant, par A — et comme le while (TRUE) est DANS le future, il ne réessaie",
     "jamais. B obtient le lease `fs` quand A meurt, lance le JVM, et tourne SANS moniteur : plus de health_check(), et plus personne pour renouveler le lease `fs`, qui expire au bout de 120 secondes",
     "pendant que le service est en parfaite santé.",
     "",
     "Le cas `compose down` / `up` sur la même machine, lui, fonctionne : lease_holder_is_dead_locally() voit l'ancien fs_mon.PID mort (le disque est monté) et considère le lease libre.",
     "",
     "Deux correctifs possibles : wait = TRUE sur le claim de fs_mon, ou déplacer la création du future APRÈS register_fs_runner(). Le second est plus juste — le moniteur n'a rien à surveiller avant",
     "qu'on soit devenu `fs` — mais il coûte le health_check() pendant toute l'attente du standby, qui peut être longue."], ORANGE, ORANGE_BG)
Y = y4 + 240

# =================================================================================================
# 18 · limites
# =================================================================================================
Y = panel(L, Y, W, "18 · Les limites, assumées explicitement",
          "Aucune n'est un oubli. Les connaître évite de « corriger » un comportement voulu.")

lims = [
    ("1 · Coopératif, pas infaillible", RED,
     ["Deux runners peuvent se chevaucher quelques secondes autour d'une course de claim ou d'une",
      "partition réseau. ATTÉNUÉ par le garde avant chaque écriture, pas éliminé.",
      "Le vrai correctif demande une écriture conditionnelle côté Scala (issue 10)."]),
    ("2 · Latence de bascule = TTL", RED,
     ["Par construction. Ne pas descendre le TTL en dessous de quelques multiples du battement pour",
      "« l'améliorer » : cela ne fait que provoquer de fausses expirations sous charge — c'est-à-dire",
      "exactement les doublons que tout ceci évite."]),
    ("3 · Écritures concurrentes sur properties.json / le xlsx", GREY,
     ["Plusieurs apps Shiny, ou une app et un service, peuvent encore s'écraser mutuellement.",
      "Ce n'était pas traité avant non plus : c'est un autre sujet."]),
    ("4 · Aucune authentification", GREY,
     ["Qui atteint l'API fs peut lire, voler ou libérer n'importe quel lease. C'est déjà vrai de tous",
      "les autres fichiers qu'elle sert : cela relève du réseau et du déploiement, pas de ce mécanisme."]),
    ("5 · Le lease parent `search` n'est jamais renouvelé", ORANGE,
     ["search_loop() le prend puis se bloque dans parLapply() pour la vie du service : aucun tour sur",
      "lequel battre, et rien de propre à prouver vivant — sa vitalité EST celle de ses workers.",
      "Le vrai correctif est de ne plus lui donner de lease du tout (§9.5) ; c'est le dernier point",
      "ouvert de l'issue 14. Cause du `search` stale des panneaux 9 et 13."]),
    ("6 · `make compose-down` ne rend aucun lease", ORANGE,
     ["Aucune ligne de R ne tourne sur le chemin de la descente : les fichiers restent dans runners/",
      "et le TTL est le seul mécanisme — c'est à cela qu'il sert. Détail et remèdes : panneau 16.",
      "Atténué par le wait = TRUE du panneau 15, qui a supprimé la conséquence réelle (un service",
      "qui refusait de démarrer et que plus rien ne relançait)."]),
    ("7 · Le moniteur fs d'une machine de secours", RED,
     ["Il claim `fs_mon` sans attendre, AVANT que sa machine ait pris `fs` : sur un vrai standby il",
      "meurt tout de suite et définitivement, et la machine qui reprend fs tourne sans health_check",
      "ni renouvellement du lease `fs` (panneau 17).",
      "Le seul point de cette liste qui soit un défaut plutôt qu'un arbitrage."]),
]
y = Y
for i, (t, col, lines) in enumerate(lims):
    r, c = divmod(i, 2)
    box(L + c * 1240, y + r * 130, 1220, t, lines, col, h=120, title_size=14, line_size=12)
Y = y + ((len(lims) + 1) // 2) * 130 + 40

T(L, Y, "Fin. Pour la conception et les décisions prises en chemin : inst/schema/multi_services_plan.md, issues 1 à 14.", 14, GREY)

dump(os.path.join(SCHEMA_DIR, "runners_lease.excalidraw"))
print("elements:", len(ELEMENTS))
print("height:", Y)
