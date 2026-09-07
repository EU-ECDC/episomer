# -*- coding: utf-8 -*-
"""Ajoute la couche « lease » au bas de inst/schema/services.excalidraw.

Ce fichier est le brouillon de réflexion qui a précédé la conception : il pose des questions.
On n'y touche pas — on lui ajoute, en dessous, la section qui y répond avec ce qui a
réellement été implémenté (issues 1 à 9 de multi_services_plan.md).
"""
import sys, os, json
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import exca
from exca import *  # noqa

SRC = os.path.join(HERE, "..", "..", "episomer", "inst", "schema", "services.excalidraw")

doc = json.load(open(SRC, encoding="utf-8"))
# Idempotent : les éléments ajoutés par ce script portent un index commençant par "c",
# on les retire avant de les régénérer, sinon un second passage dupliquerait la section.
existing = [e for e in doc["elements"] if not e["index"].startswith("c")]
maxy = max(e["y"] + e["height"] for e in existing)

# Les nouveaux éléments prennent des index qui trient après le dernier existant ("b0E").
_c = [0]
def _idx():
    n = _c[0]; _c[0] += 1
    return "c%02d" % n
exca._next_index = _idx

L = 138
W = 2200
Y = maxy + 140

LINE([(L, Y - 60), (L + W, Y - 60)], color=BLACK, width=2)
T(L, Y, "▼  Ce qui a finalement été implémenté : la couche « lease »", 28, BLACK)
Y += 46
T(L, Y,
  "Tout ce qui est au-dessus de ce trait est la réflexion qui a précédé. Cette section y répond.\n"
  "Conception complète : inst/schema/multi_services_plan.md (issues 1 à 9) · Le mécanisme en détail : inst/schema/runners_lease.excalidraw · Le code : R/runners-lease.R et R/runners.R",
  14, GREY)
Y += 60

# --- Les réponses aux questions posées plus haut --------------------------------------------------
Y = panel(L, Y, W, "Les questions posées plus haut, et leurs réponses")

qa = [
    ("« le fs_loop a-t-il le droit de faire tourner et de surveiller un service sur une autre machine ? »",
     RED, GREEN_BG,
     ["NON, et ce n'est pas nécessaire. Il n'y a pas d'arbitre. Chaque machine s'enregistre elle-même,",
      "en écrivant un fichier sur la machine fs ; la machine fs, elle, ne fait qu'HÉBERGER ces fichiers.",
      "Elle ne lance rien, ne surveille rien et ne tue rien à distance.",
      "Corollaire : tout ce qui TUE un processus (stop_*_runner_task, stop_all_runner_tasks, le menu de",
      "setup()) reste strictement LOCAL et continue de lire le fichier PID."]),
    ("« un service écrirait régulièrement un fichier… » — l'idée du bas de page",
     GREEN, GREEN_BG,
     ["C'est exactement ce qui a été fait, avec deux précisions qui font toute la différence :",
      "  · la fraîcheur se lit sur le MTIME du fichier, jamais sur une date écrite dedans — le mtime est",
      "    produit par la seule machine fs, donc aucun décalage d'horloge n'est possible ;",
      "  · renouveler ne réécrit rien : c'est un simple epifile.touch(), un tout petit POST."]),
    ("« quand vérifie-t-on qu'on a le droit d'écrire ? juste avant d'écrire topics.json ? »",
     ORANGE, GREEN_BG,
     ["Oui, et pas seulement là. stop_if_lease_lost() est appelé avant CHAQUE écriture qui compte :",
      "  search_loop_worker()  avant commit_posts(), save_config(), register_series()",
      "  search_topic()        après la requête réseau, avant l'envoi des posts",
      "  detect_loop()         avant chaque save_tasks(), avant ET après generate_alerts()",
      "C'est là qu'est la vraie sûreté du mécanisme, pas dans la prise du lease."]),
    ("« B et C écrivent quand même leur fichier PID de leur côté »",
     BLUE, GREEN_BG,
     ["Oui, et la couche PID est conservée TELLE QUELLE. Les deux couches répondent à deux questions",
      "différentes et aucune ne remplace l'autre :",
      "  <name>.PID          disque local — « y a-t-il ICI un processus que je peux tuer ? »",
      "  runners/<name>.json machine fs   — « qui, dans TOUT le cluster, a le droit d'être ce runner ? »",
      "register_runner() exige que les deux disent oui."]),
]
y = Y
for q, col, bg, lines in qa:
    y = box(L, y, 1500, q, lines, col, bg, title_size=14, line_size=12) + 16
Y = y + 20

# --- Le dessin A / B / C, version implémentée -----------------------------------------------------
Y = panel(L, Y, W, "Le dessin machine A / B / C, en vrai")

y0 = Y
box(L + 480, y0, 640, "machine A — fs_loop()",
    ["Les index Lucene (elle seule les ouvre).",
     "Et, servis par l'API fs :",
     "   runners/fs.json",
     "   runners/fs_mon.json",
     "   runners/search.json",
     "   runners/search.bluesky.json",
     "   runners/detect.json",
     "   runners/old/…            ← les leases rendus",
     "",
     "Rôle passif : elle stocke, elle horodate. Rien de plus."],
    ORANGE, ORANGE_BG)

yb = y0 + 250
box(L, yb, 460, "machine B — search_loop()",
    ["claim `search`, puis `search.bluesky`, …",
     "touch toutes les 30 s (heartbeat)",
     "release à l'arrêt propre",
     "écrit aussi search.PID en LOCAL"], GREEN, GREEN_BG)
box(L + 520, yb, 460, "machine C — search_loop() (standby)",
    ["read `search` → vivant → refuse de démarrer",
     "log : « already held by … on host 'machine-B' »",
     "sort, est relancée, réessaie",
     "reprend quand B rend, ou après le TTL"], RED, RED_BG)
box(L + 1040, yb, 460, "machine D — detect_loop()",
    ["claim `detect` (TTL 1800 s : une tâche",
     "peut durer des heures sans rien écrire)",
     "garde avant chaque save_tasks() et",
     "avant generate_alerts() (courriels !)"], BLUE, BLUE_BG)

A([(L + 230, yb), (L + 700, y0 + 170)], GREEN)
A([(L + 750, yb), (L + 800, y0 + 170)], RED)
A([(L + 1270, yb), (L + 900, y0 + 170)], BLUE)
T(L + 300, yb - 46, "epipath() / epifile.*  —  HTTP", 12, GREY)

box(L + 1560, y0, 640, "Ce que la machine fs NE fait PAS",
    ["Elle ne décide pas qui tourne : elle stocke des fichiers",
     "et leur donne une date de modification.",
     "",
     "Tout le raisonnement (est-ce libre ? est-ce toujours moi ?)",
     "se fait dans le processus qui pose la question, en R.",
     "",
     "C'est pour cela que le mécanisme est COOPÉRATIF : deux",
     "instances peuvent, en théorie, se croire toutes les deux",
     "titulaires pendant quelques secondes autour d'une course.",
     "",
     "L'API fs n'a pas d'écriture conditionnelle ; d'où la lecture",
     "de confirmation après un délai de garde, et surtout le",
     "contrôle avant chaque écriture."], GREY)
Y = yb + 190

# --- Les fonctions, mises à jour ------------------------------------------------------------------
Y = panel(L, Y, W, "La liste « Services-related functions » du haut, mise à jour")

y0 = Y
box(L, y0, 1080, "Ce qui a CHANGÉ",
    ["is_search_running()   lisait get_running_task_pid() → lit maintenant les LEASES",
     "                      (le parent `search` OU un search.<réseau> actif, cf. runners_lease panneau 12)",
     "is_detect_running()   idem : is_runner_running(\"detect\")",
     "missing_search_jobs() les réseaux actifs dont le lease n'est pas frais",
     "loop_run_issues()     bloque désormais l'activation quand le runner tourne sur une AUTRE machine,",
     "                      dans une modale qui nomme l'hôte, l'heure de prise, l'âge du battement et le TTL",
     "register_*_runner()   = fichier PID (inchangé) + claim_runner_lease()",
     "les trois boucles     on.exit(unregister_runner()) : release du lease + retrait du PID",
     "",
     "Et un nouveau bloc dans la page de configuration : le tableau des leases",
     "(runner, statut, hôte, dernier battement, depuis quand) — runner_leases_df()."],
    GREEN, GREEN_BG)

box(L + 1120, y0, 1080, "Ce qui n'a PAS changé",
    ["is_fs_running()             toujours un GET /ping : instantané, sans TTL, et il dit si le",
     "                            service RÉPOND plutôt que si quelqu'un tient son lease",
     "register_*_runner_task()    lance un processus ICI",
     "stop_*_runner_task()        tue un processus ICI (kill_task → fichier PID)",
     "stop_all_runner_tasks()     idem, et il ne doit surtout PAS dépendre du lease",
     "get_running_task_pid()      la couche PID, intacte",
     "le menu « services » de setup()  affiche le statut LOCAL, parce que ses deux actions",
     "                            (démarrer / arrêter) sont locales",
     "",
     "Règle : tout ce qui TUE lit le PID. Tout ce qui AFFICHE « est-ce que ça tourne quelque",
     "part ? » lit le lease."],
    BLUE, BLUE_BG)
Y = y0 + 260

box(L, Y, 2200, "Le seul point resté ouvert",
    ["Le lease parent `search` est pris par search_loop() avant même qu'il ne lance ses workers. Une machine ne peut donc pas collecter SEULEMENT certains réseaux",
     "pendant qu'une autre fait le reste — bien que search.bluesky et search.mastodon.<hôte> soient deux leases indépendants.",
     "Et comme search_loop() se bloque ensuite dans parLapply(), plus rien ne renouvelle ce lease parent : il passe en « stale » sur une machine parfaitement saine.",
     "À trancher (§9.5 du plan) : rendre l'entrée parente « de simple présence », ou la supprimer. Tant que ce n'est pas fait, is_search_running() contourne le",
     "problème en interrogeant aussi les enfants."],
    ORANGE, ORANGE_BG)

doc["elements"] = existing + ELEMENTS
# On respecte à la lettre le format que produit l'extension Excalidraw de VS Code
# (indentation de 2, UTF-8 brut, pas de saut de ligne final) : sans cela le fichier
# entier apparaîtrait comme modifié dans le diff, et on ne verrait plus ce qu'on a ajouté.
with open(SRC, "w", encoding="utf-8") as f:
    json.dump(doc, f, ensure_ascii=False, indent=2)
print("added", len(ELEMENTS), "elements; total", len(doc["elements"]))
