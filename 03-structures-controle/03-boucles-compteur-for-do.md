🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.3 Boucles compteur (for-do)

## Introduction

Imaginez que vous devez afficher les nombres de 1 à 100. Sans boucle, vous devriez écrire 100 lignes de `WriteLn` ! Les boucles sont des structures qui permettent de répéter des instructions plusieurs fois. La boucle `for` est particulièrement adaptée quand vous savez **à l'avance** combien de fois vous voulez répéter une action.

## Qu'est-ce qu'une boucle ?

Une boucle est une structure qui permet de répéter un bloc d'instructions un certain nombre de fois. C'est comme dire : "Fais ceci 10 fois" ou "Répète cette action pour chaque nombre de 1 à 100".

### Exemple de la vie quotidienne

- "Monte 10 marches d'escalier" → répéter l'action "monter une marche" 10 fois
- "Compte de 1 à 10" → répéter l'action "dire un nombre" 10 fois
- "Distribue 5 cartes à chaque joueur" → répéter l'action "donner une carte" 5 fois

## La boucle FOR-TO-DO (croissante)

### Syntaxe de base

```pascal
for variable := valeur_debut to valeur_fin do
  instruction;
```

Cette boucle dit : "Pour la variable allant de valeur_debut jusqu'à valeur_fin (en ajoutant 1 à chaque fois), exécute l'instruction".

### Premier exemple simple

```pascal
program PremierFor;
var
  i: Integer;
begin
  for i := 1 to 5 do
    WriteLn('Bonjour !');

  ReadLn;
end.
```

**Résultat :**
```
Bonjour !
Bonjour !
Bonjour !
Bonjour !
Bonjour !
```

Le programme affiche "Bonjour !" 5 fois.

### Utiliser la variable de boucle

La variable de boucle (ici `i`) contient la valeur actuelle du compteur :

```pascal
program AffichageNumeros;
var
  i: Integer;
begin
  WriteLn('Comptage de 1 à 10 :');
  for i := 1 to 10 do
    WriteLn(i);

  ReadLn;
end.
```

**Résultat :**
```
Comptage de 1 à 10 :
1
2
3
4
5
6
7
8
9
10
```

### Exemple avec calculs

```pascal
program TableMultiplication;
var
  i: Integer;
begin
  WriteLn('Table de multiplication par 7 :');
  WriteLn;

  for i := 1 to 10 do
    WriteLn('7 x ', i, ' = ', 7 * i);

  ReadLn;
end.
```

**Résultat :**
```
Table de multiplication par 7 :

7 x 1 = 7
7 x 2 = 14
7 x 3 = 21
...
7 x 10 = 70
```

## Plusieurs instructions dans la boucle

Pour exécuter plusieurs instructions à chaque itération, utilisez `begin-end` :

### Syntaxe avec BEGIN-END

```pascal
for variable := debut to fin do
begin
  instruction1;
  instruction2;
  instruction3;
end;
```

### Exemple

```pascal
program BoucleMultiple;
var
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    WriteLn('--- Itération numéro ', i, ' ---');
    WriteLn('Le double de ', i, ' est ', i * 2);
    WriteLn('Le triple de ', i, ' est ', i * 3);
    WriteLn;
  end;

  ReadLn;
end.
```

**Résultat :**
```
--- Itération numéro 1 ---
Le double de 1 est 2
Le triple de 1 est 3

--- Itération numéro 2 ---
Le double de 2 est 4
Le triple de 2 est 6

--- Itération numéro 3 ---
Le double de 3 est 6
Le triple de 3 est 9
```

## La boucle FOR-DOWNTO-DO (décroissante)

Pour compter à l'envers, utilisez `downto` au lieu de `to` :

### Syntaxe

```pascal
for variable := valeur_debut downto valeur_fin do
  instruction;
```

### Exemple de compte à rebours

```pascal
program CompteARebours;
var
  i: Integer;
begin
  WriteLn('Compte à rebours pour le lancement :');
  WriteLn;

  for i := 10 downto 1 do
  begin
    WriteLn(i, '...');
    // En réalité, ici on ajouterait une pause
  end;

  WriteLn('DÉCOLLAGE !');
  ReadLn;
end.
```

**Résultat :**
```
Compte à rebours pour le lancement :

10...
9...
8...
...
1...
DÉCOLLAGE !
```

### Comparaison TO vs DOWNTO

```pascal
program ComparaisonToDownto;
var
  i: Integer;
begin
  WriteLn('Avec TO (croissant) :');
  for i := 1 to 5 do
    Write(i, ' ');
  WriteLn;
  WriteLn;

  WriteLn('Avec DOWNTO (décroissant) :');
  for i := 5 downto 1 do
    Write(i, ' ');
  WriteLn;

  ReadLn;
end.
```

**Résultat :**
```
Avec TO (croissant) :
1 2 3 4 5

Avec DOWNTO (décroissant) :
5 4 3 2 1
```

## Variables utilisables dans FOR

La variable de boucle doit être d'un **type ordinal** :

### Types autorisés

- **Integer** : le plus courant
- **Char** : pour parcourir des caractères
- **Boolean** : rarement utilisé mais possible
- **Types énumérés** : nous les verrons plus tard

### Exemple avec Integer

```pascal
var
  compteur: Integer;
begin
  for compteur := 1 to 10 do
    WriteLn(compteur);
end.
```

### Exemple avec Char

```pascal
program AlphabetMajuscules;
var
  lettre: Char;
begin
  WriteLn('L''alphabet en majuscules :');

  for lettre := 'A' to 'Z' do
    Write(lettre, ' ');

  WriteLn;
  ReadLn;
end.
```

**Résultat :**
```
L'alphabet en majuscules :
A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
```

### Exemple avec Char en minuscules

```pascal
program Voyelles;
var
  lettre: Char;
begin
  WriteLn('Voyelles en minuscules :');

  for lettre := 'a' to 'e' do
    WriteLn(lettre);

  ReadLn;
end.
```

## Boucles imbriquées

Vous pouvez placer une boucle `for` à l'intérieur d'une autre boucle `for`. C'est ce qu'on appelle des boucles imbriquées.

### Syntaxe

```pascal
for i := debut1 to fin1 do
begin
  for j := debut2 to fin2 do
  begin
    // Instructions utilisant i et j
  end;
end;
```

### Exemple : Tableau de multiplication

```pascal
program TableMultiplicationComplete;
var
  i, j, resultat: Integer;
begin
  WriteLn('TABLE DE MULTIPLICATION (1 à 5)');
  WriteLn;

  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
    begin
      resultat := i * j;
      Write(resultat:4);  // :4 pour aligner sur 4 caractères
    end;
    WriteLn;  // Retour à la ligne après chaque ligne du tableau
  end;

  ReadLn;
end.
```

**Résultat :**
```
TABLE DE MULTIPLICATION (1 à 5)

   1   2   3   4   5
   2   4   6   8  10
   3   6   9  12  15
   4   8  12  16  20
   5  10  15  20  25
```

### Exemple : Dessin de motifs

```pascal
program DessinTriangle;
var
  ligne, colonne: Integer;
begin
  WriteLn('Triangle d''étoiles :');
  WriteLn;

  for ligne := 1 to 5 do
  begin
    for colonne := 1 to ligne do
      Write('*');
    WriteLn;
  end;

  ReadLn;
end.
```

**Résultat :**
```
Triangle d'étoiles :

*
**
***
****
*****
```

## Calculs avec des boucles

Les boucles sont très utiles pour effectuer des calculs répétitifs.

### Exemple : Somme de nombres

```pascal
program SommeNombres;
var
  i, somme: Integer;
begin
  somme := 0;

  for i := 1 to 100 do
    somme := somme + i;

  WriteLn('La somme des nombres de 1 à 100 est : ', somme);
  ReadLn;
end.
```

### Exemple : Calcul de factorielle

```pascal
program Factorielle;
var
  n, i: Integer;
  resultat: Int64;  // Int64 pour les grands nombres
begin
  Write('Entrez un nombre : ');
  ReadLn(n);

  resultat := 1;

  for i := 1 to n do
    resultat := resultat * i;

  WriteLn('La factorielle de ', n, ' est : ', resultat);
  ReadLn;
end.
```

### Exemple : Recherche du maximum

```pascal
program RechercheMaximum;
var
  i, nombre, maximum: Integer;
begin
  WriteLn('Entrez 5 nombres :');

  // Lire le premier nombre comme maximum initial
  Write('Nombre 1 : ');
  ReadLn(maximum);

  // Lire les 4 autres nombres
  for i := 2 to 5 do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);

    if nombre > maximum then
      maximum := nombre;
  end;

  WriteLn;
  WriteLn('Le plus grand nombre est : ', maximum);
  ReadLn;
end.
```

## Valeurs de début et fin variables

Les limites de la boucle peuvent être des variables ou des expressions :

### Exemple avec variables

```pascal
program BoucleVariable;
var
  i, debut, fin: Integer;
begin
  Write('Nombre de départ : ');
  ReadLn(debut);
  Write('Nombre de fin : ');
  ReadLn(fin);

  WriteLn;
  WriteLn('Comptage de ', debut, ' à ', fin, ' :');

  for i := debut to fin do
    WriteLn(i);

  ReadLn;
end.
```

### Exemple avec expressions

```pascal
program BoucleExpression;
var
  i, n: Integer;
begin
  Write('Entrez un nombre : ');
  ReadLn(n);

  // Affiche les nombres de 1 jusqu'au double de n
  for i := 1 to (n * 2) do
    WriteLn(i);

  ReadLn;
end.
```

## Particularités importantes

### 1. La variable de boucle est automatiquement incrémentée

Vous n'avez pas besoin d'écrire `i := i + 1`. C'est fait automatiquement !

```pascal
// CORRECT - Ne touchez pas à i
for i := 1 to 10 do
  WriteLn(i);

// ERREUR ! Ne modifiez jamais i dans la boucle
for i := 1 to 10 do
begin
  WriteLn(i);
  i := i + 1;  // NE FAITES JAMAIS CECI !
end;
```

### 2. Valeur de début supérieure à valeur de fin

Si la valeur de début est supérieure à la valeur de fin avec `to`, la boucle ne s'exécute pas du tout :

```pascal
// Cette boucle ne s'exécute jamais
for i := 10 to 1 do
  WriteLn(i);  // Rien ne s'affiche

// Pour compter à l'envers, utilisez DOWNTO
for i := 10 downto 1 do
  WriteLn(i);  // Ceci fonctionne
```

### 3. La variable conserve sa dernière valeur

Après la boucle, la variable garde sa dernière valeur :

```pascal
program ValeurApresFor;
var
  i: Integer;
begin
  for i := 1 to 5 do
    WriteLn(i);

  WriteLn('Après la boucle, i vaut : ', i);  // i vaut 5
  ReadLn;
end.
```

## Exemples pratiques

### Générateur de motifs

```pascal
program GenerateurMotifs;
var
  ligne, espace, etoile: Integer;
  hauteur: Integer;
begin
  Write('Hauteur du sapin : ');
  ReadLn(hauteur);
  WriteLn;

  for ligne := 1 to hauteur do
  begin
    // Espaces avant les étoiles
    for espace := 1 to (hauteur - ligne) do
      Write(' ');

    // Étoiles
    for etoile := 1 to (2 * ligne - 1) do
      Write('*');

    WriteLn;
  end;

  // Tronc du sapin
  for ligne := 1 to 2 do
  begin
    for espace := 1 to (hauteur - 1) do
      Write(' ');
    Write('|');
    WriteLn;
  end;

  ReadLn;
end.
```

**Résultat pour hauteur = 5 :**
```
    *
   ***
  *****
 *******
*********
    |
    |
```

### Validation d'entrée avec tentatives limitées

```pascal
program ValidationAvecTentatives;
var
  i, nombre: Integer;
  correct: Boolean;
const
  MOT_DE_PASSE = 1234;
  MAX_TENTATIVES = 3;
begin
  correct := false;

  WriteLn('=== SYSTÈME DE SÉCURITÉ ===');
  WriteLn('Vous avez ', MAX_TENTATIVES, ' tentatives.');
  WriteLn;

  for i := 1 to MAX_TENTATIVES do
  begin
    Write('Tentative ', i, ' - Entrez le code : ');
    ReadLn(nombre);

    if nombre = MOT_DE_PASSE then
    begin
      correct := true;
      WriteLn('✓ Code correct ! Accès autorisé.');
      Break;  // Sort de la boucle immédiatement
    end
    else
    begin
      if i < MAX_TENTATIVES then
        WriteLn('✗ Code incorrect. Il vous reste ', MAX_TENTATIVES - i, ' tentative(s).')
      else
        WriteLn('✗ Code incorrect. Accès bloqué !');
    end;
    WriteLn;
  end;

  ReadLn;
end.
```

### Statistiques sur une série de nombres

```pascal
program StatistiquesNombres;
var
  i, n, nombre: Integer;
  somme, minimum, maximum: Integer;
  moyenne: Real;
begin
  Write('Combien de nombres voulez-vous entrer ? ');
  ReadLn(n);
  WriteLn;

  // Initialisation
  Write('Nombre 1 : ');
  ReadLn(nombre);
  somme := nombre;
  minimum := nombre;
  maximum := nombre;

  // Lecture des autres nombres
  for i := 2 to n do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);

    somme := somme + nombre;

    if nombre < minimum then
      minimum := nombre;

    if nombre > maximum then
      maximum := nombre;
  end;

  moyenne := somme / n;

  WriteLn;
  WriteLn('=== STATISTIQUES ===');
  WriteLn('Somme : ', somme);
  WriteLn('Moyenne : ', moyenne:0:2);
  WriteLn('Minimum : ', minimum);
  WriteLn('Maximum : ', maximum);

  ReadLn;
end.
```

### Affichage de calendrier

```pascal
program CalendrierMois;
var
  jour, premierJour, joursDansMois: Integer;
  espace: Integer;
begin
  Write('Premier jour du mois (1=Lundi, 7=Dimanche) : ');
  ReadLn(premierJour);
  Write('Nombre de jours dans le mois : ');
  ReadLn(joursDansMois);
  WriteLn;

  WriteLn('Lun Mar Mer Jeu Ven Sam Dim');
  WriteLn('---------------------------');

  // Espaces avant le premier jour
  for espace := 1 to (premierJour - 1) do
    Write('    ');

  // Affichage des jours
  for jour := 1 to joursDansMois do
  begin
    Write(jour:3, ' ');

    // Retour à la ligne après dimanche
    if (premierJour + jour - 1) mod 7 = 0 then
      WriteLn;
  end;

  WriteLn;
  ReadLn;
end.
```

## Erreurs courantes

### 1. Modifier la variable de boucle

```pascal
// ERREUR ! Ne modifiez jamais la variable de boucle
for i := 1 to 10 do
begin
  WriteLn(i);
  i := i + 5;  // ❌ INTERDIT !
end;

// CORRECT - Laissez for gérer i
for i := 1 to 10 do
  WriteLn(i);
```

### 2. Confondre TO et DOWNTO

```pascal
// Ceci ne fonctionne pas comme prévu
for i := 10 to 1 do  // ❌ La boucle ne s'exécute jamais
  WriteLn(i);

// CORRECT pour compter à rebours
for i := 10 downto 1 do  // ✓
  WriteLn(i);
```

### 3. Oublier BEGIN-END pour plusieurs instructions

```pascal
// ERREUR ! Seule la première instruction est dans la boucle
for i := 1 to 5 do
  WriteLn('Ligne ', i);
  WriteLn('Suite');  // ❌ Ceci ne s'exécute qu'une fois, après la boucle !

// CORRECT
for i := 1 to 5 do
begin
  WriteLn('Ligne ', i);
  WriteLn('Suite');
end;
```

### 4. Utiliser des types non ordinaux

```pascal
// ERREUR ! Real n'est pas un type ordinal
var
  x: Real;
begin
  for x := 1.0 to 10.0 do  // ❌ ERREUR DE COMPILATION
    WriteLn(x);
end;

// CORRECT - Utilisez Integer
var
  i: Integer;
begin
  for i := 1 to 10 do
    WriteLn(i);
end;
```

### 5. Compter sur la valeur finale exacte

```pascal
// Attention à la valeur après la boucle
var
  i: Integer;
begin
  for i := 1 to 10 do
    WriteLn(i);

  // Après la boucle, i peut valoir 10 ou 11 selon le compilateur
  // Ne comptez pas sur une valeur précise
end;
```

## Bonnes pratiques

### 1. Noms de variables significatifs

```pascal
// MOINS BON
for i := 1 to n do

// MEILLEUR
for numeroEleve := 1 to nombreEleves do
for ligne := 1 to hauteur do
for tentative := 1 to maxTentatives do
```

Cependant, `i`, `j`, `k` sont acceptables pour de courtes boucles simples.

### 2. Utiliser des constantes

```pascal
const
  MIN_AGE = 18;
  MAX_AGE = 65;
begin
  for age := MIN_AGE to MAX_AGE do
    // traitement
end;
```

### 3. Éviter les boucles trop complexes

Si votre boucle fait plus de 20 lignes, envisagez d'extraire le code dans une procédure.

### 4. Commentaires pour les boucles imbriquées

```pascal
for ligne := 1 to hauteur do
begin
  // Affichage des espaces
  for espace := 1 to (hauteur - ligne) do
    Write(' ');

  // Affichage des étoiles
  for etoile := 1 to ligne do
    Write('*');

  WriteLn;
end;
```

## Exemple récapitulatif complet

```pascal
program AnalyseurTexte;
var
  texte: String;
  i: Integer;
  caractere: Char;
  compteurVoyelles, compteurConsonnes: Integer;
  compteurChiffres, compteurEspaces: Integer;
begin
  compteurVoyelles := 0;
  compteurConsonnes := 0;
  compteurChiffres := 0;
  compteurEspaces := 0;

  WriteLn('=== ANALYSEUR DE TEXTE ===');
  WriteLn;
  Write('Entrez un texte : ');
  ReadLn(texte);
  WriteLn;

  WriteLn('Analyse en cours...');
  WriteLn;

  // Parcourir chaque caractère
  for i := 1 to Length(texte) do
  begin
    caractere := texte[i];

    // Analyser le type de caractère
    case caractere of
      'A', 'E', 'I', 'O', 'U', 'Y',
      'a', 'e', 'i', 'o', 'u', 'y':
        compteurVoyelles := compteurVoyelles + 1;

      'B'..'D', 'F'..'H', 'J'..'N', 'P'..'T', 'V'..'X', 'Z',
      'b'..'d', 'f'..'h', 'j'..'n', 'p'..'t', 'v'..'x', 'z':
        compteurConsonnes := compteurConsonnes + 1;

      '0'..'9':
        compteurChiffres := compteurChiffres + 1;

      ' ':
        compteurEspaces := compteurEspaces + 1;
    end;
  end;

  // Affichage des résultats
  WriteLn('=== RÉSULTATS ===');
  WriteLn('Longueur totale : ', Length(texte), ' caractères');
  WriteLn('Voyelles : ', compteurVoyelles);
  WriteLn('Consonnes : ', compteurConsonnes);
  WriteLn('Chiffres : ', compteurChiffres);
  WriteLn('Espaces : ', compteurEspaces);
  WriteLn('Autres caractères : ',
    Length(texte) - compteurVoyelles - compteurConsonnes -
    compteurChiffres - compteurEspaces);

  WriteLn;
  WriteLn('Aperçu caractère par caractère :');
  for i := 1 to Length(texte) do
  begin
    Write(texte[i]);
    if i mod 10 = 0 then
      WriteLn;  // Retour à la ligne tous les 10 caractères
  end;

  WriteLn;
  ReadLn;
end.
```

## Résumé

La boucle `for` permet de répéter des instructions un nombre connu de fois :

- **Syntaxe croissante** : `for i := debut to fin do`
- **Syntaxe décroissante** : `for i := debut downto fin do`
- La variable est **automatiquement incrémentée/décrémentée**
- Utilisez `begin-end` pour plusieurs instructions
- **Ne modifiez jamais** la variable de boucle manuellement
- Types autorisés : Integer, Char, Boolean, types énumérés
- Utile pour : comptages, calculs répétitifs, parcours de structures
- Peut être imbriquée pour créer des motifs ou tableaux

La boucle `for` est idéale quand vous savez exactement combien de fois vous voulez répéter une action. Pour les cas où le nombre de répétitions dépend d'une condition, nous verrons les boucles `while` et `repeat` dans les sections suivantes.

⏭️ [Boucles conditionnelles (while-do, repeat-until)](/03-structures-controle/04-boucles-conditionnelles-while-repeat.md)
