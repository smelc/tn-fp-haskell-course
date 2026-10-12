<!-- LTeX: language=fr -->

# Projet Programmation fonctionnelle en Haskell

## Rendu

À envoyer par mail au plus tard le **1er décembre 2026** à l'adresse <thomas.bagrel@tweag.io>, avec <clement.hurlin@gmail.com> en copie.

Le projet peut être réalisé seul ou en binôme (recommandé), au choix.

## Objectif

Écrire un programme réaliste en Haskell, en utilisant les concepts vus en cours.
La clarté et la lisibilité du code, la production d'un code correct et l'organisation du projet seront mises en avant dans l'évaluation.
Il vaut mieux un projet bien organisé et fonctionnel plutôt qu'un projet débordant de fonctionnalités bâclées/buggées.

> [!IMPORTANT]
> Lisez bien le sujet *entièrement* avant de vous lancer dans le code !

Le but de ce projet est d'implémenter, en Haskell, un outil permettant de parser, de filtrer et d'afficher des URL.

## Partie 1 : Parsing et affichage des URLs

### Schéma d'une URL

Une URL est composée de plusieurs parties :

- Le schéma (ou protocole) : `http`, `https`, `ftp`, etc.
- Le nom de domaine : `www.example.com`
- Le chemin : `/chemin/vers/la/ressource`
- La requête (optionnelle) : `?param1=valeur1&param2=valeur2`
- Le fragment (optionnel) : `#section`

Par exemple, dans l'URL `https://www.example.com/chemin/vers/la/ressource?param1=valeur1#section` :
- Le schéma est `https`
- Le nom de domaine est `www.example.com`
- Le chemin est `/chemin/vers/la/ressource`
- La requête est `param1=valeur1`
- Le fragment est `section`

Le format complet des URL valides est plus complexe en réalité, mais on se limitera ici à ce qui est présenté ci-dessus pour le cadre de ce projet.

#### Nom de domaine

Un nom de domaine est composé de plusieurs segments séparés par des points. Les segments sont en réalité affichés dans l'ordre inverse de leur importance : le segment le plus à droite est appelé le domaine de premier niveau (TLD), par exemple `com` dans `www.example.com`. Vient ensuite le domaine de second niveau (SLD), par exemple `example` dans `www.example.com`, puis éventuellement des sous-domaines, par exemple `www` dans `www.example.com`.

On considère qu'un nom de domaine valide doit avoir au moins deux segments (on veillera à garantir cette règle via l'utilisation d'un type approprié en Haskell) : un domaine de premier niveau (TLD) et un domaine de second niveau (SLD). Les sous-domaines sont optionnels et leur nombre n'est pas limité.

Dans le cadre du projet, il est demandé de stocker en Haskell les segments du nom de domaine dans l'ordre réel d'importance, c'est-à-dire du domaine de premier niveau (TLD) vers les sous-domaines. Cet ordre sera important pour le filtrage (voir la [partie sur le filtrage](#partie-2--filtrage)).

#### Chemin

Le chemin d'une URL correspond à l'emplacement de la ressource sur le serveur. Il commence généralement par un `/` et peut contenir plusieurs segments séparés par des `/`. Par exemple, dans l'URL `https://www.example.com/chemin/vers/la/ressource`, le chemin est `/chemin/vers/la/ressource`.

Dans ce projet, les slashs `/` redondants dans le chemin sont ignorés. Ainsi, `https://www.example.com`, `https://www.example.com/` et `https://www.example.com//` sont des URL valides et équivalentes : dans les trois cas, le chemin ne contient aucun segment. De même, `https://www.example.com/a//b/` est équivalente à `https://www.example.com/a/b`. La représentation canonique n'inclut aucun slash après le domaine lorsque le chemin est vide ; sinon, elle sépare les segments du chemin par un seul slash et n'ajoute pas de slash final.

#### Paramètres de requête

Les paramètres de requête sont optionnels et apparaissent après le `?` (si présent) dans une URL. Ils sont composés d'une ou plusieurs paires clé-valeur séparées par des `&`. Chaque clé et chaque valeur doit être non vide ; les séparateurs répétés ou les paires incomplètes (par exemple `?`, `?a=` ou `?=b`) sont invalides. Pour simplifier le projet, les clés peuvent apparaître plusieurs fois. On veillera à conserver l'ordre d'apparition des paires clé-valeur.

#### Fragment

Le fragment est optionnel et apparaît après `#`. S'il est présent, il doit être non vide. Lorsqu'une requête et un fragment sont tous deux présents, le fragment vient après la requête.

#### Hypothèses

Afin de simplifier le projet, nous faisons l'hypothèse que tous les schémas, et tous les segments de nom de domaine, chemins, clés/valeurs de requêtes, et le fragment (si présent) ne contiennent pas les caractères réservés pour leur séparation (`://`, `?`, `#`, etc.).
En particulier, les URLs utilisées pour les tests pourront se contenter d'utiliser des caractères alphanumériques pour les éléments susmentionnés.

### Tâches Partie 1

- Créer les types Haskell appropriés pour représenter une URL et ses différents sous-composants. La représentation tiendra compte du soin apporté à rendre les états invalides ou illégaux impossibles à représenter (c'est-à-dire que l'espace de valeurs des types choisis devra être aussi proche que possible de l'ensemble des valeurs considérées comme valides).
- Implémenter une fonction de parsing d'URL qui prend une chaîne de caractères et retourne éventuellement une valeur du type URL si la chaîne fournie représente une URL valide, en respectant les hypothèses et les contraintes définies ci-dessus. Pour cette question, le parsing devra se faire à la main, à l'aide de fonctions de manipulation de chaînes de caractères et de récursion si nécessaire. **L'usage des expressions régulières et des bibliothèques externes de parsing est interdit.**
- Implémenter les instances de `Show` pour tous les types précédents
- Vérifier qu'une chaîne représentant une URL valide et canonique[^1] est bien parsée puis retransformée par `show` en une chaîne identique à la chaîne de départ. Pour ce faire, on écrira un test QuickCheck et on pourra s'aider des générateurs suivants :
```haskell
listWithLength :: (Int, Int) -> Gen a -> Gen [a]
listWithLength (minLen, maxLen) gen = do
  len <- choose (minLen, maxLen)
  vectorOf len gen

genAlphaWithLength :: (Int, Int) -> Gen String
genAlphaWithLength lenRange =
  listWithLength lenRange (elements ['a'..'z'])

genSchemeStr :: Gen String
genSchemeStr = elements ["http", "https", "ftp", "ssh"]

genDomainNameStr :: Gen String
genDomainNameStr = do
  tld <- genAlphaWithLength (2, 4)
  sld <- genAlphaWithLength (2, 10)
  subdomains <- frequency [
      (3, return []),
      (1, listWithLength (1, 5) $ genAlphaWithLength (2, 10))
    ]
  let subdomainsString = foldr (\sub acc -> acc ++ sub ++ ".") "" subdomains
  return $ subdomainsString ++ sld ++ "." ++ tld

genPathStr :: Gen String
genPathStr = frequency [
    (3, return ""),
    (1, do
      segments <- listWithLength (1, 5) $ genAlphaWithLength (2, 10)
      return $ foldr (\seg acc -> "/" ++ seg ++ acc) "" segments
    )
  ]

genRequestStr :: Gen String
genRequestStr = frequency [
    (3, return ""),
    (1, do
      k <- genAlphaWithLength (2, 10)
      v <- genAlphaWithLength (2, 10)
      rest <- listWithLength (0, 4) $ do
        key <- genAlphaWithLength (2, 10)
        value <- genAlphaWithLength (2, 10)
        return (key, value)
      return $ "?" ++ k ++ "=" ++ v ++ foldr (\(key, value) acc -> "&" ++ key ++ "=" ++ value ++ acc) "" rest
    )
  ]

genFragmentStr :: Gen String
genFragmentStr =
  frequency [
    (3, return ""),
    (1, ("#" ++) <$> genAlphaWithLength (2, 10))
  ]

genUrlStr :: Gen String
genUrlStr = do
  schemeStr <- genSchemeStr
  domainStr <- genDomainNameStr
  pathStr <- genPathStr
  requestStr <- genRequestStr
  fragmentStr <- genFragmentStr
  return $ schemeStr ++ "://" ++ domainStr ++ pathStr ++ requestStr ++ fragmentStr
```

[^1]: Une URL canonique ne contient pas de slash redondant dans son chemin : le chemin vide est omis et les segments non vides sont séparés par un seul slash, sans slash final.

## Partie 2 : Filtrage

#### Format d'un filtre

- Un filtre complet a la forme `filtre-de-schéma://filtre-de-domaine`. Un filtre ne contient ni chemin, ni requête, ni fragment.
- Le filtre pour le schéma est soit `*`, soit un schéma spécifique comme `http` ou `https`. Les schémas spécifiques suivent les mêmes règles de caractères et de casse que dans une URL.
- Le filtre pour le nom de domaine est écrit avec ses segments (séparés par `.`) dans l'ordre habituel (par exemple `www.example.com`) et est interprété **de droite à gauche, comme le nom de domaine lui-même** :
  - le segment de filtre `**` autorise un nombre quelconque de segments de nom de domaine, y compris 0 (ce segment de filtre ne peut être placé qu'à la toute gauche du filtre de nom de domaine, et ne peut apparaître qu'une fois)
  - le segment de filtre `*` autorise exactement un segment de nom de domaine
  - un segment de filtre spécifique, par exemple `example`, exige que le segment correspondant soit identique.
  - exemples :
    - le filtre de domaine `**` autorise tout nom de domaine valide. `**.*` et `**.*.*` sont également équivalents, puisqu'un domaine valide comporte au moins deux segments.
    - le filtre de domaine `*.example.com` autorise les domaines ayant exactement trois segments avec `example.com` comme suffixe ; il n'accepte pas `example.com` lui-même.
    - le filtre de domaine `**.example.com` autorise les domaines ayant un nombre quelconque de segments du moment qu'ils ont `example.com` comme suffixe ; il accepte en particulier `example.com` lui-même.
    - le filtre de domaine `www.*.com` autorise n'importe quel SLD. En revanche, `www.**.com` n'est pas un filtre de domaine valide, car `**` n'est pas placé à la toute gauche du filtre.

### Tâches Partie 2

- Créer les types appropriés pour représenter les filtres de schéma et de nom de domaine, puis un type global pour représenter un filtre combinant les deux.
- Implémenter une fonction de parsing du filtre complet (schéma et nom de domaine).
- Implémenter une fonction de matching pour chaque filtre et type correspondant (e.g. type schéma avec filtre de schéma, type nom de domaine avec filtre de nom de domaine); qui indique si ledit composant de l'URL est autorisé par le filtre correspondant.
- Implémenter une fonction de matching pour le filtre complet en combinant les résultats des sous-filtres. Cette fonction prendra une URL complète en entrée et indique si elle est acceptée par le filtre. Les autres composants de l'URL (chemin, requête et fragment) seront ignorés et donc acceptés automatiquement par le filtre.
- Implémenter les instances de `Show` pour tous les types utilisés pour représenter les filtres
- Vérifier qu'une chaîne représentant un filtre complet valide est bien parsée puis retransformée en chaîne identique via `show`. Pour ce faire, on écrira un test QuickCheck (on pourra notamment copier et adapter les générateurs de la partie 1).
- Ajouter quelques tests unitaires pour vérifier que des URLs sont bien acceptées ou rejetées par la fonction de filtrage, comme attendu. On veillera à couvrir un nombre raisonnable de cas pour s'assurer que les différents comportements de l'énoncé sont correctement testés.

## Contenu de l'archive

Une archive **.zip** ou **.tar.gz** dont le nom respecte le format `<nom1>-[<nom2>-]projet-haskell.(zip|tar.gz)`, contenant :
- le code Haskell du projet, et les fichiers `cabal` nécessaires à la compilation (pas besoin d'inclure le projet compilé, sinon l'archive risquerait d'être trop lourde pour un envoi par mail) ;
- un fichier `README.md` à la racine de l'archive, décrivant les aspects suivants de votre projet (en français ou en anglais au choix) :
  - la commande de compilation utilisée, qui doit permettre de compiler le projet depuis un terminal situé à la racine ;
  - la commande pour lancer les tests, qui doit également être utilisable depuis un terminal situé à la racine ;
  - les éventuelles bibliothèques externes utilisées, et pourquoi ;
  - une description de tout aspect du projet que vous jugez pertinent (architecture, choix de conception, limitations, etc.) qui n'est pas couvert par les commentaires dans le code, et que vous jugeriez pertinent de mentionner ;
  - les éventuelles fonctionnalités non implémentées, les bugs connus ou les difficultés rencontrées ;
- tout autre fichier que vous jugez pertinent pour la correction (par exemple, des fichiers de données ou des exemples de test). Pensez à mentionner leur existence et leur utilité dans le `README.md`, le cas échéant.

## Notation

Le barème est donné à titre indicatif

- Parsing et affichage d'une URL : 14 points
- Filtrage : 8 points

Le but du projet est d'opérer en tant qu'ingénieur. La clarté du code, celle des commentaires/explications, et la prise d'initiatives seront partie intégrante de l'évaluation. Veillez à suivre les bonnes pratiques vues en cours (par exemple pour la représentation des erreurs / fonctions au comportement partiel).

N'hésitez pas à prendre les arbitrages ou décisions nécessaires pour rendre le projet dans l'état que vous jugez optimal. Le projet remis doit compiler et ses tests doivent pouvoir être exécutés. Si certaines fonctionnalités ne sont pas terminées, décrivez clairement leur état dans le `README.md`, et commentez-les dans le code, pour éviter qu'ils bloquent la compilation ou l'exécution des tests pour le reste du projet. Pour vous mettre dans le rôle, considérez qu'une personne d'une autre équipe va devoir reprendre le projet là où vous l'avez laissé, avec seulement ce sujet pour contexte.

## Des questions ?

Si vous avez des questions relatives au projet, n'hésitez pas à m'envoyer un courriel à l'adresse <thomas.bagrel@tweag.io>.
