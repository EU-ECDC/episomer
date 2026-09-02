# Génération des schémas `.excalidraw`

Les deux schémas de `episomer/inst/schema/` qui décrivent le mécanisme des *runner leases* sont
**générés** plutôt qu'écrits à la main : ils sont trop denses pour être maintenus au clic, et un
schéma qui n'est plus à jour vaut moins que pas de schéma du tout.

| script | produit |
|---|---|
| `gen_runners_lease.py` | `episomer/inst/schema/runners_lease.excalidraw` — réécrit **entièrement** le fichier |
| `gen_services_append.py` | `episomer/inst/schema/services.excalidraw` — **ajoute** la section « couche lease » au bas du fichier existant, sans toucher au reste (dont les deux images incrustées) |

`exca.py` est la petite bibliothèque commune : `box()`, `table()`, `panel()`, `A()` (flèche),
`LINE()`, `T()` (texte), et la palette reprise des schémas existants.

## Utilisation

```sh
python3 devscripts/schema/gen_runners_lease.py     # Python 3, aucune dépendance
python3 devscripts/schema/gen_services_append.py   # idempotent : relancer ne duplique rien
```

Les deux fichiers produits restent parfaitement modifiables à la main dans Excalidraw ; simplement,
une retouche manuelle de `runners_lease.excalidraw` sera perdue à la prochaine génération. Pour un
changement durable, modifier le script.

`gen_services_append.py` reconnaît ses propres éléments à leur `index`, qui commence par `c` (les
éléments d'origine du fichier vont de `a0` à `b0E`). C'est ce qui le rend rejouable.
