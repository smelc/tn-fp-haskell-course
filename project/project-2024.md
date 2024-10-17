# Projet Programmation fonctionnelle en Haskell

## Rendu

-- Date à fixer

## Objectif

<!-- Merci Thomas :) -->
Écrire un programme "réaliste" en Haskell, en utilisant les concepts vus en cours. La clarté et lisibilité du code, la production d'un code correct, et l'organisation du projet seront mises en avant dans l'évaluation par rapport à un code débordant de features mais avec des bugs/edge cases non gérés.

## Sujet

Le but de ce projet est de créer un parser et un interpréteur pour un petit langage de programmation inspiré de C, que nous appellerons MiniC dans la suite.
Des idées pour aller plus loin sont proposées, mais non obligatoires.

## MiniC

MiniC est un petit langage de programmation impérative dérivé de C, dont voici la syntaxe :
```
Expression ⩴ Number | Identifier | Boolean | String

Statement ⩴ 'skip' | Identifier ':=' Expression | 'if' Expression 'then' '{' Block '}' ('else' '{' Block '}')? | 'print' Expression

Block ⩴ ε | Statement ';' Block
```

Dans la grammaire ci-dessus :
- `ε` dénote le mot vide.
- `Number` dénote la plus longue séquence non vide de caractères numériques (`0` jusqu'à `9`), potentiellement précédée immédiatement (sans espaces) par un `-`.
  Il n'est pas question, dans un premier temps, de gérer des nombres entiers sous forme hexadécimale ou binaire.
- `Boolean` dénote les identifiants spéciaux `true` et `false`.
- `String` dénote une chaîne de caractères, c'est-à-dire une séquence de caractères telle que :
  - Le premier et le dernier caractères sont des guillemets `"`.
  - Tous les caractères au milieu, s'il y en a, sont soit des caractères (ASCII) simples (p.e. `a`, `0`).
- `Identifier` dénote la plus longue séquence non vide de caractères, telle que le premier caractère est alphabétique et les caractères suivants sont soit alphanumériques soit `_`.
  Il n'est pas demandé de gérer une unique casse (`snake_case`, `camelCase`, …).
  Attention, les mots-clés de la grammaire (`if`, `switch`, …) ainsi que les booléens ne doivent pas être reconnus comme des identifiants.
  Ainsi, il ne doit pas être possible d'écrire `true := 0`.
  De même, `ifx` doit être reconnu comme un identifiant, et non comme le mot-clé `if` suivant de l'identifiant `x`.

### Sémantique semi-formelle des instructions

Tout comme les expressions, les instructions suivent la sémantique usuelle de C.
- `skip` ne fait rien du tout.
- `x := e` assigne à `x` la valeur de `e`, si évaluer `e` ne lève pas d'erreur.
- `if e then B₁ else B₂` exécute `B₁` si la valeur de `e` est `true`, sinon `B₂` si la valeur de `e` est `false`.
  Si `e` ne s'évalue pas à un booléen, une erreur doit être levée.
- `print e` affiche la valeur de `e` sur la sortie standard.

### Parsing de MiniC

Vous avez plusieurs choix pour l'écriture du parser de MiniC, sans impact sur la notation.
- Si vous vous sentez aventureux, vous pouvez écrire le parser entièrement à la main.
  Attention cependant, cela n'est pas facile et il est plus intéressant de se concentrer sur l'interpréteur.
- Vous pouvez sinon utiliser des bibliothèques telles [parsec](https://hackage.haskell.org/package/parsec) ou [megaparsec](https://hackage.haskell.org/package/megaparsec) pour vous faciliter l'écriture du parser.
  Ces deux bibliothèques possèdent des tutoriels afin de vous accompagner dans l'écriture de votre parser, aussi bien au niveau des instructions que des expressions (par exemple pour la gestion des priorités opératoires).

Dans tous les cas, il vous faudra définir des types permettant de décrire l'arbre de syntaxe, afin de pouvoir ensuite écrire l'interpréteur.

### Interpréteur de MiniC

L'interpréteur de MiniC devra se composer de deux interpréteurs différents : un pour les expressions du langage, et un autre pour les instructions/block.

- L'interpréteur pour les expressions réduit une expression en sa forme normale, définie par le type `data Value = Number Integer | Boolean Bool | String String`.
- L'interpréteur des blocks réduit chaque instruction du block une à une, à la suite, jusqu'à ce qu'il n'y en ait plus.

Dans les deux cas, l'interpréteur dépendra d'un contexte, qui assigne des valeurs (de type `Value`) à des noms de variables (on peut le voir comme un *mapping partiel* des noms de variables vers des valeurs).

Il est possible, au choix :
1. De gérer les erreurs d'interprétation avec la fonction [`error`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:error).
   Dans ce cas, vous pourrez utiliser la monade suivante pour écrire l'interpréteur :
   ```haskell
   type MonadInterpreter m = (MonadState Context m, MonadIO m)

   run :: Block -> IO ()
   run B = evalStateT (executeBlock B) emptyContext
   ```
2. De gérer les erreurs d'interprétation avec la monade [`MonadError`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Error-Class.html#t:MonadError).
   Vous pourrez utiliser cette monade pour écrire l'interpréteur :
   ```haskell
   type MonadInterpreter m = (MonadState Context m, MonadError String m, MonadIO m)

   run :: Block -> IO (Either String ())
   run B = do
     res <- evalStateT (runExceptT $ executeBlock B) emptyContext
     case res of
       Left err -> putStrLn $ "error: " ++ err
       Right () -> pure ()
   ```

> [!WARNING]
> Attention à bien remplacer `executeBlock` par le nom de votre fonction d'exécution pour les blocks (de type `MonadInterpreter m => m ()`),
> et `emptyContext` par la représentation d'un contexte vide (sans association de variables).

> [!IMPORTANT]
> Vous aurez besoin d'ajouter `{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}` au début de votre fichier (avant la déclaration du module).
> Cela vous permettra d'écrire le type `MonadInterpreter`, sans quoi le compilateur Haskell vous renverra des erreurs.

La sortie standard devra inclure les valeurs imprimées par l'instruction `print`, suivie de l'erreur s'il y en a une.
Aucun format particulier n'est attendu pour l'affichage de l'erreur.
Ne vous embêtez pas à essayer d'afficher des erreurs avec le code source originel, cela n'en vaut pas la peine ici.

## Bonus: pour aller plus loin

Voici plusieurs axes d'améliorations/extensions possibles pour MiniC :
- Dans le langage proposé, il n'y a pas de boucles.
  Vous pouvez ajouter une boucle `'while' Expression '{' Block '}'` qui exécutera le block tant que la valeur de l'expression est `true`.
  Ne vous préoccupez pas de la terminaison, si vous écrivez `while true { print 0; };`, la sortie standard doit inclure une infinité de `0` (jusqu'à <kbd>Ctrl</kbd>+<kbd>C</kbd> en tout cas).
- MiniC n'inclut pas d'instruction permettant d'arrêter le programme dans des circonstances extrêmes.
  Vous pourrez ajouter une instruction `'panic' String` qui fait quitter le programme immédiatement avec la chaîne associée comme erreur (potentiellement préfixée par `panic:`).

> [!IMPORTANT]
> N'essayez pas de faire tous les bonus!
> Cela ne vous apportera pas forcément plus de points que si vous en aviez fait moins.

## Rendu

Une archive Zip dont le nom respecte le format `<nom1>-[<nom2>-]projet-haskell.zip`, qui contient :
- le code Haskell du projet, dans un dossier `src/` à la racine, et les fichiers générés par `stack`/`cabal` à la racine de l'archive;
- un fichier `README.md` à la racine de l'archive, décrivant les aspects suivants de votre projet (en français ou en anglais au choix) :
  - comment le compiler;
  - comment l'exécuter (en particulier, est-ce que votre exécutable prend un fichier en argument ou du code sur l'entrée standard);
  - les bibliothèques utilisées, et pourquoi;
  - les potentielles fonctionnalités non implémentées, bug connus ou difficultés rencontrées;
- plusieurs fichiers contenant des exemples de programmes MiniC que vous avez testés, dans un dossier `examples/` à la racine;
- tout autre fichier potentiel que vous pensez pertinents quant à la correction.

> [!NOTE]
> Veillez à bien documenter votre code, idéalement en anglais!

## Notation



## Des questions ?

Si vous avez des questions relatives au projet, n'hésitez pas à m'envoyer un mail à <ghilain.bergeron@inria.fr>.
