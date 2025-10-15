<!-- LTeX: language=fr -->

# Projet Programmation fonctionnelle en Haskell

## Rendu

Rendu par mail avant le ***[[DATE A FIXER]]***, par mail à l'adresse <ghilain.bergeron@inria.fr> avec <clement.hurlin@gmail.com> en CC.

Le projet peut être réalisé seul ou en binôme (recommandé), au choix.

## Objectif

Écrire un programme "réaliste" en Haskell, en utilisant les concepts vus en cours.
La clarté et la lisibilité du code, la production d'un code correct et l'organisation du projet seront mises en avant dans l'évaluation.
Il vaut mieux un projet bien organisé et fonctionnel plutôt qu'un projet débordant de fonctionnalités bâclées/buggées.

> [!IMPORTANT]
> Lisez bien le sujet *entièrement* avant de vous lancer dans le code !

## Le projet

Le but de ce projet est d'implémenter, en Haskell, des fonctions de compression de données (utilisées dans le monde réel, notamment dans des outils tels que [bzip2](https://fr.wikipedia.org/wiki/Bzip2)).
Votre outil devra être capable de compresser et décompresser des données obtenues soit depuis un fichier (codé en dur ou indiqué sur la ligne de commande) soit depuis l'entrée standard à l'aide des fonctions implémentées.

### Le codage de Huffman

Généralement, une chaîne de caractères (supposée non compressée, ici) est représentée par la suite des encodages de chacun de ses caractères.
En fonction de l'encodage, cette représentation peut prendre plus ou moins de place : en ASCII, une chaîne de caractères $\mathrm{S}$ peut occuper plus que $8 \times |\mathrm{S}|$ bits en mémoire !

Le [codage de Huffman](https://fr.wikipedia.org/wiki/Codage_de_Huffman) est une technique de compression basée sur du parcours de graphe, fonctionnant sur des séquences de symboles (encodables) arbitraires. 
Il a été conçu dans le but de réduire considérablement la taille des données encodées, en représentant les symboles apparaissant fréquemment par des séquences de bits plus petites que les symboles n'apparaissant que rarement.
Il est à noter que le codage de Huffman d'une séquence de symboles n'est pas nécessairement unique !

> [!NOTE]
> Prenons par exemple le chaîne de caractères `"ABEACADABEA"`.
> Représentée en ASCII, celle-ci occupe $8 \times 11 = 88$ bits.
> En appliquant le codage de Huffman, nous pouvons réduire cette taille jusqu'à $23$ bits !

Dans la suite, nous verrons comment obtenir ce résultat.
Notez que nous écrirons les bits verbatim sur la console ou dans un fichier.
Dans un cas d'application réel, les bits seraient écrits directement dans un fichier ouvert en mode binaire, mais ce n'est pas demandé.

#### L'algorithme

Pour un texte donné, composé de symboles dans un alphabet donné, chaque symbole est représenté par une suite de bits (que l'on appellera "code") satisfaisant les propriétés suivantes :
- Aucun code n'est préfixe d'un autre (connue sous le nom de [propriété du préfixe](https://en.wikipedia.org/wiki/Prefix_code)).
  Cela nous permet de pouvoir inverser la compression.

  > [!IMPORTANT]
  > Cette propriété est importante ! 
  > Supposons une chaîne de caractères `"AB"` telle que `A` serait encodé par `10` et `B` par `1010`.
  > La chaîne de caractères encodée serait `101010`.
  > Il est alors impossible de déterminer, sans cette propriété, s'il s'agissait de `AB` ou `BA`.
- Les symboles apparaissant le plus souvent seront les symboles avec le code le plus court. 
  Cela nous permet de réduire considérablement la taille de la séquence encodée.

Les codes sont déterminés en construisant un arbre binaire dont les feuilles représentent les symboles à encoder.
Cet arbre (que nous nommerons "arbre de codes" dans la suite) est construit de la manière suivante :
1. L'algorithme s'exécute sur une liste d'arbres, que nous fusionnerons petit à petit.
   
   Pour chaque symbole de la séquence en entrée, créer une feuille contenant ce symbole ainsi que la fréquence d'apparition du symbole dans la séquence (son nombre d'occurrences).
2. Tant qu'il reste strictement plus qu'un arbre à traiter :
    1. Récupérer deux arbres de poids minimum (faites un choix, s'il y en a plus que deux).
    2. Fusionner les deux arbres en créant un nouveau nœud de branchement par dessus.
       Ce nouveau nœud contient l'ensemble des symboles des deux arbres fusionnés, ainsi que la somme des fréquences d'apparition des arbres fusionnés.
    3. Réitérer.

<details>
<summary>Exemple de construction d'un arbre de codes sur la chaîne <code>"ABEACADABEA"</code></summary>

* Etape 1 : 
  
    ```
    [A: 5]      [B: 2]      [C: 1]      [D: 1]      [E: 2]
    ```
* Etape 2 : 

    Nous allons fusionner `[C: 1]` et `[D: 1]` :
    ```
    [A: 5]      [B: 2]      [CD: 2]      [E: 2]
                            /     \
                       [C: 1]     [D: 1]
    ```

    Puis nous allons fusionner `[B: 2]` et `[CD: 2]` :
    ```
    [A: 5]      [BCD: 4]      [E: 2]
                /      \
           [B: 2]      [CD: 2]
                       /     \
                  [C: 1]     [D: 1]
    ```
    > [!NOTE]
    > Nous aurions très bien pu fusionner `[B: 2]` et `[E: 2]`, ou `[CD: 2]` et `[E: 2]` à la place !
    > L'ordre dans lequel nous fusionnons impacte l'encodage final, mais cela n'est pas gênant, car
    > - La taille de la séquence encodée ne devrait être impactée que minimalement, voire pas du tout.
    > - L'arbre est fourni afin de pouvoir décoder les données.

    Puis nous allons fusionner `[BCD: 4]` et `[E: 2]` :
    ```
    [A: 5]      [BCDE: 6]
                /       \
         [BCD: 4]       [E: 2]
         /      \
    [B: 2]      [CD: 2]
                /     \
           [C: 1]     [D: 1]
    ```

    Puis finalement nous allons fusionner `[A: 5]` et `[BCDE: 6]` :
    ```
         [ABCDE: 11]
         /         \
    [A: 5]         [BCDE: 6]
                   /       \
            [BCD: 4]       [E: 2]
            /      \
       [B: 2]      [CD: 2]
                   /     \
              [C: 1]     [D: 1]
    ```

</details><br>

Une fois cet arbre construit, l'encodage d'une séquence devient simplement un parcours d'arbre.
Pour chaque symbole de la séquence, l'arbre est parcouru en profondeur depuis la racine jusqu'à trouver la feuille correspondant au symbole.
Si une branche de gauche a été parcourue, on ajoute un `0` à l'encodage, sinon on ajoute un `1`.

<details>
<summary>Exemple d'encodage de la chaîne <code>"ABEACADABEA"</code></summary>

Selon l'arbre précédent, chaque caractère de la chaîne `"ABEACADABEA"` est encodé selon le tableau ci-dessous :

| Caractère | Encodage en binaire |
| :-------: | :-----------------: |
|    `A`    |         `0`         |
|    `B`    |        `100`        |
|    `C`    |       `1010`        |
|    `D`    |       `1011`        |
|    `E`    |        `11`         |

On pourra remarquer qu'effectivement aucun code n'est préfixe d'un autre.
La chaîne `"ABEACADABEA"` est donc encodée par `0 100 11 0 1010 0 1011 0 100 11 0`, soit $23$ bits.
</details>

------

Décoder est tout aussi simple !
Pour une chaîne encodée et un arbre de codes donnés, décoder la chaîne correspond à parcourir les bits un à un.
En partant de la racine de l'arbre, si l'on rencontre un `0` alors on parcourt la branche de gauche, et si l'on rencontre un `1` alors on parcourt la branche de droite.
Dans le cas où nous arrivons à une feuille, son symbole associé est ajouté à la séquence décodée, et nous recommençons à décoder depuis la racine de l'arbre.

#### A vous de coder

L'objectif est d'encoder des chaînes de caractères.
Commencez par écrire le type des arbres de code.
Puis quelques tests.
Et enfin les fonctions d'encodage et de décodage.

> [!IMPORTANT]
> Les fonctions d'encodage et de décodage doivent être inverses l'une de l'autre, c'est-à-dire que pour un arbre quelconque $T$ et une chaîne $S$, il faut que $\operatorname{decode}^{\mathit{Huffman}}_T(\operatorname{encode}^{\mathit{Huffman}}_T(S)) = S$ (ou autrement dit $\operatorname{decode}^{\mathit{Huffman}}_T \circ \operatorname{encode}^{\mathit{Huffman}}_T = \operatorname{id}$).
> Il pourrait s'agir d'un de vos tests !

### La transformée de Burrows-Wheeler

Le codage de Huffman est très efficace pour minimiser la taille en bits d'une séquence de symboles.
Cependant, il existe beaucoup de cas où il est possible de minimiser encore plus en analysant les répétitions de symboles (aussi bien avant qu'après avoir appliqué le codage de Huffman).
Par exemple, la chaîne `"ABABABAB"` pourrait être encodée plus efficacement que par `<code A><code B><code A><code B><code A><code B><code A><code B>`, en exploitant les répétitions.

Dans cette section, nous n'allons pas encore exploiter les répétitions (cela sera laissé en bonus), mais allons à la place implémenter un algorithme permettant d'augmenter le nombre de répétitions (dans la majorité des cas), de manière inversible : la [transformée de Burrows-Wheeler](https://fr.wikipedia.org/wiki/Transform%C3%A9e_de_Burrows-Wheeler).

#### L'algorithme

Soit une séquence $S$ de symboles de longueur $n$.
L'algorithme d'encodage se déroule ainsi :
1. Générer un tableau de toutes les rotations de $i$ rangs vers la droite (pour $0 \le i < n$) de la séquence $S$.
2. Trier les lignes du tableau par ordre lexicographique, tout en gardant en mémoire l'indice de la ligne contenant le texte d'origine (attention aux textes qui se répètent, par exemple `"ABAB"`).
3. Le texte encodé correspond à la dernière colonne du tableau, précédé de l'indice du texte original.

<details>
<summary>Exemple de transformation de la chaîne <code>"ABEACADABEA"</code></summary>

1. Voici la matrice générée à partir de toutes les rotations de la chaîne d'origine :

    |    Chaîne     |
    | :-----------: |
    | `ABEACADABEA` |
    | `AABEACADABE` |
    | `EAABEACADAB` |
    | `BEAABEACADA` |
    | `ABEAABEACAD` |
    | `DABEAABEACA` |
    | `ADABEAABEAC` |
    | `CADABEAABEA` |
    | `ACADABEAABE` |
    | `EACADABEAAB` |
    | `BEACADABEAA` |

2. Nous la trions en suite par ordre alphabétique (la ligne du texte de base est indiquée par une petite flèche $\leftarrow$) :

    |    Chaîne     | Position       |
    | :-----------: | :------------- |
    | `AABEACADABE` | 1              |
    | `ABEAABEACAD` | 2              |
    | `ABEACADABEA` | 3 $\leftarrow$ |
    | `ACADABEAABE` | 4              |
    | `ADABEAABEAC` | 5              |
    | `BEAABEACADA` | 6              |
    | `BEACADABEAA` | 7              |
    | `CADABEAABEA` | 8              |
    | `DABEAABEACA` | 9              |
    | `EAABEACADAB` | 10             |
    | `EACADABEAAB` | 11             |

3. Le résultat de la transformation est donc `3` suivi de la dernière colonne du tableau, soit `3EDAECAAAABB`.
   On peut remarquer que, là où le texte original avait beaucoup de `A`, la majeure partie de `A` ont été regroupés en une seule répétition, ce qui permet ensuite d'appliquer d'autres algorithmes de compression qui prennent en compte les répétitions de symboles/schémas.

</details>

-----

L'algorithme de décodage procède quant à lui de manière inverse, en reconstruisant le tableau d'origine depuis sa dernière colonne :
1. Pour $0 \le i < n$,
   1. Ajouter la séquence encodée dans la dernière colonne libre (le moins à gauche possible).
   2. Trier les lignes du tableau dans l'ordre lexicographique.
2. Sélectionner la ligne correspondant à l'indice sauvegardé avec la séquence encodée.

<details>
<summary>Retrouver <code>"ABEACADABEA"</code> depuis <code>"3EDAECAAAABB"</code></summary>

| Initialisation |  Tri | Ajout 1 |  Tri | Ajout 2 |   Tri | Ajout 3 |    Tri | Ajout 4 |     Tri |  Ajout 5 |      Tri |
| -------------: | ---: | ------: | ---: | ------: | ----: | ------: | -----: | ------: | ------: | -------: | -------: |
|            `E` |  `A` |    `EA` | `AA` |   `EAA` | `AAB` |  `EAAB` | `AABE` | `EAABE` | `AABEA` | `EAABEA` | `AABEAC` |
|            `D` |  `A` |    `DA` | `AB` |   `DAB` | `ABE` |  `DABE` | `ABEA` | `DABEA` | `ABEAA` | `DABEAA` | `ABEAAB` |
|            `A` |  `A` |    `AA` | `AB` |   `AAB` | `ABE` |  `AABE` | `ABEA` | `AABEA` | `ABEAC` | `AABEAC` | `ABEACA` |
|            `E` |  `A` |    `EA` | `AC` |   `EAC` | `ACA` |  `EACA` | `ACAD` | `EACAD` | `ACADA` | `EACADA` | `ACADAB` |
|            `C` |  `A` |    `CA` | `AD` |   `CAD` | `ADA` |  `CADA` | `ADAB` | `CADAB` | `ADABE` | `CADABE` | `ADABEA` |
|            `A` |  `B` |    `AB` | `BE` |   `ABE` | `BEA` |  `ABEA` | `BEAA` | `ABEAA` | `BEAAB` | `ABEAAB` | `BEAABE` |
|            `A` |  `B` |    `AB` | `BE` |   `ABE` | `BEA` |  `ABEA` | `BEAC` | `ABEAC` | `BEACA` | `ABEACA` | `BEACAD` |
|            `A` |  `C` |    `AC` | `CA` |   `ACA` | `CAD` |  `ACAD` | `CADA` | `ACADA` | `CADAB` | `ACADAB` | `CADABE` |
|            `A` |  `D` |    `AD` | `DA` |   `ADA` | `DAB` |  `ADAB` | `DABE` | `ADABE` | `DABEA` | `ADABEA` | `DABEAA` |
|            `B` |  `E` |    `BE` | `EA` |   `BEA` | `EAA` |  `BEAA` | `EAAB` | `BEAAB` | `EAABE` | `BEAABE` | `EAABEA` |
|            `B` |  `E` |    `BE` | `EA` |   `BEA` | `EAC` |  `BEAC` | `EACA` | `BEACA` | `EACAD` | `BEACAD` | `EACADA` |

|   Ajout 6 |       Tri |    Ajout 7 |        Tri |     Ajout 8 |         Tri |      Ajout 9 |          Tri |
| --------: | --------: | ---------: | ---------: | ----------: | ----------: | -----------: | -----------: |
| `EAABEAC` | `AABEACA` | `EAABEACA` | `AABEACAD` | `EAABEACAD` | `AABEACADA` | `EAABEACADA` | `AABEACADAB` |
| `DABEAAB` | `ABEAABE` | `DABEAABE` | `ABEAABEA` | `DABEAABEA` | `ABEAABEAC` | `DABEAABEAC` | `ABEAABEACA` |
| `AABEACA` | `ABEACAD` | `AABEACAD` | `ABEACADA` | `AABEACADA` | `ABEACADAB` | `AABEACADAB` | `ABEACADABE` |
| `EACADAB` | `ACADABE` | `EACADABE` | `ACADABEA` | `EACADABEA` | `ACADABEAA` | `EACADABEAA` | `ACADABEAAB` |
| `CADABEA` | `ADABEAA` | `CADABEAA` | `ADABEAAB` | `CADABEAAB` | `ADABEAABE` | `CADABEAABE` | `ADABEAABEA` |
| `ABEAABE` | `BEAABEA` | `ABEAABEA` | `BEAABEAC` | `ABEAABEAC` | `BEAABEACA` | `ABEAABEACA` | `BEAABEACAD` |
| `ABEACAD` | `BEACADA` | `ABEACADA` | `BEACADAB` | `ABEACADAB` | `BEACADABE` | `ABEACADABE` | `BEACADABEA` |
| `ACADABE` | `CADABEA` | `ACADABEA` | `CADABEAA` | `ACADABEAA` | `CADABEAAB` | `ACADABEAAB` | `CADABEAABE` |
| `ADABEAA` | `DABEAAB` | `ADABEAAB` | `DABEAABE` | `ADABEAABE` | `DABEAABEA` | `ADABEAABEA` | `DABEAABEAC` |
| `BEAABEA` | `EAABEAC` | `BEAABEAC` | `EAABEACA` | `BEAABEACA` | `EAABEACAD` | `BEAABEACAD` | `EAABEACADA` |
| `BEACADA` | `EACADAB` | `BEACADAB` | `EACADABE` | `BEACADABE` | `EACADABEA` | `BEACADABEA` | `EACADABEAA` |

|      Ajout 10 |           Tri |      Sélection |
| ------------: | ------------: | -------------: |
| `EAABEACADAB` | `AABEACADABE` |              1 |
| `DABEAABEACA` | `ABEAABEACAD` |              2 |
| `AABEACADABE` | `ABEACADABEA` | $\leftarrow$ 3 |
| `EACADABEAAB` | `ACADABEAABE` |              4 |
| `CADABEAABEA` | `ADABEAABEAC` |              5 |
| `ABEAABEACAD` | `BEAABEACADA` |              6 |
| `ABEACADABEA` | `BEACADABEAA` |              7 |
| `ACADABEAABE` | `CADABEAABEA` |              8 |
| `ADABEAABEAC` | `DABEAABEACA` |              9 |
| `BEAABEACADA` | `EAABEACADAB` |             10 |
| `BEACADABEAA` | `EACADABEAAB` |             11 |

</details>

#### A vous de coder

Faites le bon choix de structure de données pour stocker les matrices de caractères ! 
Vous pourrez ensuite coder directement les fonctions d'encodage et de décodage.
Attention à bien sauvegarder l'indice du texte de départ, pour la fonction d'encodage.

> [!IMPORTANT]
> Les fonctions d'encodage et de décodage doivent être inverses l'une de l'autre ! 
> Autrement dit, pour toute séquence $S$, il faut que $\operatorname{decode}^{\mathit{BWT}}(\operatorname{encode}^{\mathit{BWT}}(S)) = S$.

### Bonus 

> [!CAUTION]
> Veillez bien à avoir une base fonctionnelle du projet avant de vous attaquer à ça !
> 
Voici quelques axes d'amélioration/d'extension du projet :

1. Le codage de Huffman fonctionne réellement sur des séquences de symboles quelconques (au moins comparables), et pas uniquement des chaînes de caractères.
   Généralisez votre code afin que les fonctions d'encodage et de décodage de Huffman acceptent des séquences de symboles comparables (avec `Ord`).
   Faites de même avec la transformée de Burrows-Wheeler.
2. Des fonctions de compression de données ne sont pas très utiles si leurs résultats ne peuvent pas être écrits dans un fichier et partagé.
   Inventez un format textuel permettant de stocker l'arbre de codes de Huffman, ainsi que la position du texte original de la transformée de Burrows-Wheeler, et enfin le texte encodé.
   Il vous faudra écrire le parseur capable de comprendre ce format, et permettre d'écrire un tel format après encodage (dans un fichier ou sur la sortie standard).

   Vous pourrez vous inspirer de l'exemple suivant :
   ```
   FNS: BWT,HUFFMAN
   SYMBOLS: A:0,B:100,C:1010,D:1011,E:11
   POSITION: 3
   DATA: 11101101110100000100100
   ```
   Il peut être pertinent de stocker également l'ordre de composition des fonctions utilisées pour l'encodage (dans l'exemple ci-dessus : la transformée de Burrows-Wheeler, puis le codage de Huffman).

   > [!CAUTION]
   > Le texte encodé devra être une séquence de `0` et de `1` textuels, et non une séquence de bits (il ne doit pas être nécessaire d'ouvrir le fichier en mode binaire).
   > Le format lui-même devra aussi être textuel.
3. Les combinaisons de fonctions de compression précédentes ne sont pas forcément très satisfaisantes.
   Si on observe la sortie, notamment en appliquant dans l'ordre le codage de Huffman puis la transformée de Burrows-Wheeler, on peut remarquer que beaucoup de `0` et beaucoup de `1` sont mis les uns à côté des autres.
   Afin de ne pas gaspiller (trop) d'espace, on peut ainsi faire un [codage par plages](https://en.wikipedia.org/wiki/Run-length_encoding) après la transformée de Burrows-Wheeler.
   Attention aux cas où il n'y a pas de répétitions !
4. Il est possible de combiner les fonctions de compression dans différents ordres (tant que la décompression est réalisée dans l'ordre dual).
   Conjecturez, et montrez expérimentalement, quelles combinaisons de nos fonctions de compression sont les plus efficaces (en temps, usage mémoire, efficacité de la compression, etc).
   Vous pourrez prendre en compte d'autres fonctions de compression implémentées dans les questions bonus.

   Il ne s'agit pas particulièrement d'un exercice de code, mais d'un exercice de réflexion.

> [!IMPORTANT]
> N'essayez pas de tout faire ! 
> Il vaut mieux bien faire une question bonus, plutôt que mal faire plein de questions bonus.
> La quantité ne garantit pas un gain de points, seulement la qualité.

## Rendu

Une archive **Zip** ou **Tar GZ** dont le nom respecte le format `<nom1>-[<nom2>-]projet-haskell.(zip|tar.gz)`, qui contient :
- le code Haskell du projet, dans des dossiers `app/` et `src/` à la racine, et les fichiers générés par `stack`/`cabal` à la racine de l'archive (le projet doit être exécutable) ;
- un fichier `README.md` à la racine de l'archive, décrivant les aspects suivants de votre projet (en français ou en anglais au choix) :
  - comment le compiler (projet `stack` ou `cabal` ? bibliothèques à installer sur mon système ?) ;
  - comment l'exécuter (en particulier, est-ce que votre exécutable prend un fichier en argument ou du texte sur l'entrée standard) ;
  - les bibliothèques utilisées, et pourquoi ;
  - les potentielles fonctionnalités non implémentées, bug connus ou difficultés rencontrées ;
  - vos expérimentations (pour le bonus #3) ;
- tout autre fichier potentiel que vous pensez pertinents vis-à-vis de la correction (des exemples/tests de textes encodés, etc).

## Notation 

Le barème est donné à titre indicatif, et est sujet à changements mineurs. 
La répartition des points ne devrait cependant pas changer significativement.

- Respect des consignes : **1 point** (gratuit !)
- Codage de Huffman : **6 points**
- Transformée de Burrows-Wheeler : **8 points**
- Documentation du code : **2 points**
- Clarté, lisibilité, organisation du code et tests : **3 points**
- Questions bonus :
  - Bonus #1 : **2 points**
  - Bonus #2 : **3 points**
  - Bonus #3 : **2 points**
  - Bonus #4 : **1 point**

## Des questions ?

Si vous avez des questions relatives au projet, n'hésitez pas à m'envoyer un mail à l'adresse <ghilain.bergeron@inria.fr>.