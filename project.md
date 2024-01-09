# Projet Programmation fonctionnelle en Haskell

## Rendu

14 février avant minuit par mail : thomas.bagrel@tweag.io

## Objectif

Écrire un programme "réaliste" en Haskell, en utilisant les concepts vus en cours. La clarté et lisibilité du code, la production d'un code correct, et l'organisation du projet seront mise en avant dans l'évaluation par rapport à un code débordant de features mais avec des bugs/edge case non gérés.

## Sujet

Le but est de réaliser un convertisseur de données structurées (S-expressions vers JSON), sous forme d'une appli web (API HTTP). Une interface graphique web très légère (HTML/CSS/JS) peut être ajoutée, mais n'est pas obligatoire.

## S-expressions

Les [S-expressions](https://en.wikipedia.org/wiki/S-expression) sont un format de données structurées, utilisé notamment par le langage Lisp. Elles sont constituées de listes et d'atomes. Les listes sont délimitées par des parenthèses, et les éléments sont séparés seulement par des espaces. Les atomes peuvent être de 4 types: entiers, flottants (`Double`), chaînes de caractères (délimitées par des _double quotes_ `"`), et symboles (chaînes de caractères sans _double quotes_ autour).

Voici quelques exemples de S-expressions :

```
(1 2   3)
(1 (my-symbol 3.485) "hello World")
( 1 (2 (3 4) 5) 6 )
```

La suite de cette partie détaille le format attendu des S-expressions pour le cadre du projet.

### Entier

Au format décimal standard, tel que lu par la fonction [`read`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:read) ou [`Data.BytesString.Read.int`](https://hackage.haskell.org/package/bytestring-read-0.3.1/docs/Data-ByteString-Read.html#v:int) de Haskell.

### Flottant

Au format décimal ou en notation scientifique, tel que lu par la fonction [`read`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:read) ou [`Data.BytesString.Read.double`](https://hackage.haskell.org/package/bytestring-read-0.3.1/docs/Data-ByteString-Read.html#v:int) de Haskell.

### Chaîne de caractères

- Commence par une _double quote_ `"` (non incluse dans le contenu réel de la chaîne).
- Suivie de 0 à _n_ caractères.
- Un _backslash_ simple (`'\\' :: Char`) permet d'entrer en mode échappement :
  - si le caractère suivant est un `'n'`, alors un caractère de saut de ligne `'\n'` est ajouté à la chaîne ;
  - si le caractère suivant est une _double quote_ (`'"'`), alors elle est ajoutée à la chaîne sans terminer la chaîne pour autant ;
  - si le caractère suivant est un backslash (`'\\'`) à nouveau, alors il est ajouté à la chaîne ;
  - pour tout autre caractère _c_ différent, _c_ est ajouté à la chaîne (mais pas le _backslash_ le précédant).
- Se termine par une _double quote_ `"` (non incluse dans le contenu réel de la chaîne).

### Symbole

Composés de la plus longue séquence d'octets à partir du point de lecture qui n'inclue aucun des caractères suivants : `'('` (parenthèse ouvrante), `')'` (parenthèse fermante), `'"'` (_double quote_), `'\\'` (_backslash_ simple) ; et qui n'est pas un entier ou flottant valide. Tous les autres caractères sont autorisés. Un symbole ne peut pas être de longueur 0.

### Liste

- Commence par une parenthèse ouvrante `'('` (non incluse dans le contenu réel de la liste)
- Suivie de 0 à _n_ éléments, séparés par au moins un espace.
- Un ou plusieurs espaces peuvent être placés après la parenthèse ouvrante et avant le premier élément, ou après le dernier élément et avant la parenthèse fermante, sans impact sur le contenu réel de la liste.
- Se termine par une parenthèse fermante `')'` (non incluse dans le contenu réel de la liste).

### Nota Bene

Une S-expression ne peut pas être de longueur 0 ni n'être composée que d'espaces. La S-expression à la racine peut admettre des espaces avant ou après son contenu réel, sans que cela ne produise d'erreur lors du _parsing_ (par exemple, ` ( 1 )    `). En revanche, le contenu réel de la S-expression ne doit admettre qu'un seul élément à la racine (soit un atome, soit une liste) ; `1 (a b)` n'est pas une S-expression valide.

Les S-expressions avant parsing pourront être représentées par des `String`s ou `ByteString`s, au choix. Les S-expressions après parsing devront être représentées par un type de données Haskell adéquat (type somme riche). Il est recommandé de définir un type somme riche pour la gestion des erreurs également. De manière générale, l'attention portée aux types sera évaluée.

Le parsing des S-expressions pourra être réalisée au besoin via une librairie externe, telle que [`megaparsec`](https://hackage.haskell.org/package/megaparsec) ; cela n'aura pas d'impact sur la notation. En revanche, **il est interdit** d'utiliser [`megalisp`](`https://hackage.haskell.org/package/megalisp`) directement. Mais n'ayez pas peur, même "à la main", le parsing peut se faire en une cinquantaine de lignes de code fonctionnel standard.

## Conversion en JSON

- Les listes `( <élément> ... )` seront mappées vers des listes `[ <élément> , ... ]` en JSON.
- Les chaînes de caractères seront directement mappées vers des chaînes en JSON.
- Les entiers et flottants seront mappés vers des _numbers_ en JSON.
- Concernant les symboles :
  - Les symboles `true` et `false` seront mappées vers les booléens `true` et `false` en JSON.
  - Le symbole `null` sera mappé vers la valeur `null` en JSON.
  - Les autres symboles `<symbole>` seront mappés vers un dictionnaire `{ "symbol": "<symbole>" }` en JSON.

Des librairies telles que [`json`](https://hackage.haskell.org/package/json-0.9/candidate/docs/Text-JSON.html) ou [`aeson`](https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html) pourront être utilisés au besoin, sans impact sur la notation.

## API HTTP

L'API HTTP peut être implémentée en utilisant le framework [Scotty](https://hackage.haskell.org/package/scotty-0.12.0/docs/Web-Scotty.html) (ou un autre framework similaire, sans impact sur la notation). Elle doit être composée d'une route `POST /convert` qui prend en corps une S-expression (`Content-Type: text/plain;charset=UTF-8`) et renvoie un document JSON:

- avec un code `200 OK` et le document JSON correspondant au résultat de la conversion si celle-ci s'est bien passée ;
- avec un code `400 Bad Request` et un document JSON (potentiellement, juste une chaîne) correspondant à l'erreur si la S-expression n'est pas valide.

L'exécutable devra prendre en argument un numéro de port sur lequel écouter (et potentiellement une adresse de `bind`, typiquement `127.0.0.1` pour n'écouter qu'en local ou `0.0.0.0` pour écouter sur toutes les interfaces). Ces arguments pourront être passés en option de la ligne de commande (avec valeurs par défaut), en paramètre obligatoire, via des variables d'environnement, ou via un fichier de configuration, au choix (il faudra seulement indiquer l'option retenue et l'usage attendu dans le `README.md` du projet).

## Bonus : interface UI web

Pour **3 points bonus**, une interface utilisateur, avec un champ d'entrée, un bouton de conversion, et un champ de sortie, pourra être ajouté via la route `GET /ui`. L'interface pourra être écrite en HTML et Javascript. Il n'est pas nécessaire d'ajouter de CSS ni de faire une interface très élaborée. L'interface devra seulement être fonctionnelle et permettre de tester la conversion de S-expressions en JSON, en gérant le cas où la route `POST /convert` renvoie une erreur 400.

Si vous réalisez le bonus, merci de l'indiquer dans le fichier `README.md` du projet.

## Rendu

Un dossier compressé au format `<nom>-<prenom>-projet-haskell.zip` contenant :

- les sources Haskell du projet (code dans un dossier `src`, fichiers de projets/tooling à la racine de l'archive);
- un fichier `README.md` à la racine également, décrivant le projet, comment le compiler, comment l'exécuter, et précisant tout choix particulier ou choix d'architecture (notamment pour les options de ligne de commande) réalisé. Un document long n'est pas attendu, le but est simplement de lever toute incertitude qui ne pourrait pas être facilement expliquée dans les commentaires ou déduite du code source. N'hésitez pas à préciser les difficultés rencontrées, les bugs connus, les features non implémentées, etc. ;
- un ou plusieurs fichiers `.sexpr` dans un dossier `examples` contenant les exemples que vous aurez testé de convertir via votre API ;
- tous les autres fichiers potentiels qui pourraient être utiles ou bénéfiques à la correction.

L'usage de librairies externes est autorisé, mais leur téléchargement et installation doit se faire _seamlessly_ via le processus de build choisi, et leur usage devra être justifié (en 1 ligne) dans le fichier `README.md`.

L'usage uniforme de l'anglais est fortement indiqué pour le code et les commentaires. En revanche, le fichier `README.md` pourra être écrit en français ou en anglais.

## Notation

La notation sera basée sur les critères suivants :

- organisation du code (fonctions pures le plus possible, utilisation des types de données adéquats, _separation of concerns_, etc.) sur **4 points** ;
- clarté et lisibilité du code (noms de variables et fonctions, commentaires, formatage, etc.) sur **3 points** ;
- parsing des S-expressions sur **5 points** ;
- conversion en JSON sur **3 points** ;
- implémentation de l'API qui respecte les consignes sur **3 points** ;
- gestion des erreurs sur **2 points** ;
- (bonus) interface web sur **3 points**.

## Contact

Pour toute question,

Thomas BAGREL  
<thomas.bagrel@tweag.io>
