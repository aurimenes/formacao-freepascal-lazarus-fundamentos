🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.1 Concepts des bases de données relationnelles

## Qu'est-ce qu'une base de données ?

Une **base de données** est un système organisé permettant de stocker, gérer et retrouver des informations de manière efficace. Imaginez une bibliothèque : au lieu d'empiler les livres au hasard, on les classe par catégories, auteurs, titres... Une base de données fait la même chose avec vos données informatiques.

Dans vos programmes jusqu'à présent, vous avez probablement stocké des informations dans des variables, des tableaux ou des fichiers texte. Ces solutions fonctionnent bien pour de petites quantités de données, mais deviennent rapidement limitées :

- Comment rechercher rapidement une information parmi 10 000 enregistrements ?
- Comment éviter les doublons ?
- Comment maintenir la cohérence des données ?
- Comment permettre à plusieurs utilisateurs d'accéder aux mêmes données simultanément ?

Les bases de données répondent à ces problématiques.

## Le modèle relationnel : l'analogie du tableur

Le **modèle relationnel** est le type de base de données le plus répandu. Son fonctionnement ressemble à celui d'un tableur comme Excel ou Calc, mais en beaucoup plus puissant.

### Principe de base

Dans un tableur, vous organisez vos données en **lignes** et **colonnes** dans des **feuilles**. Une base de données relationnelle fait exactement la même chose :

- Une **feuille** devient une **table**
- Une **ligne** reste une **ligne** (ou un **enregistrement**)
- Une **colonne** reste une **colonne** (ou un **champ**)

**Exemple concret : une liste de contacts**

Imaginons que vous vouliez gérer vos contacts. Dans un tableur, vous pourriez créer ceci :

```
| ID | Prénom  | Nom     | Email                | Téléphone    |
|----|---------|---------|----------------------|--------------|
| 1  | Pierre  | Dubois  | pierre.d@email.fr    | 0601020304   |
| 2  | Marie   | Martin  | marie.m@email.fr     | 0605060708   |
| 3  | Jacques | Durand  | j.durand@email.fr    | 0609101112   |
```

Dans une base de données, cette structure s'appellerait une **table** nommée `Contacts`.

## Les tables : le cœur du système

Une **table** est une collection de données organisées en lignes et colonnes. Chaque table représente un type spécifique d'information.

### Structure d'une table

Chaque table possède :

1. **Un nom** : qui décrit ce qu'elle contient (ex: `Clients`, `Produits`, `Commandes`)
2. **Des colonnes (champs)** : qui définissent les propriétés (ex: `Nom`, `Prénom`, `DateNaissance`)
3. **Des lignes (enregistrements)** : qui contiennent les données réelles

### Types de données

Chaque colonne a un **type de données** qui définit ce qu'elle peut contenir :

- **INTEGER** : nombres entiers (1, 42, -15)
- **REAL / FLOAT** : nombres décimaux (3.14, 19.99)
- **VARCHAR / TEXT** : chaînes de caractères ("Bonjour", "Paris")
- **DATE** : dates (2025-10-15)
- **BOOLEAN** : valeurs logiques (VRAI/FAUX)

C'est similaire aux types de données que vous connaissez en Pascal !

## La clé primaire : l'identifiant unique

Chaque ligne d'une table doit pouvoir être identifiée de manière unique. C'est le rôle de la **clé primaire** (Primary Key).

### Pourquoi une clé primaire ?

Imaginez que vous ayez deux clients nommés "Marie Martin" dans votre base. Comment les distinguer ? La clé primaire résout ce problème en donnant un identifiant unique à chaque enregistrement.

**Exemple :**

```
Table: Clients
| ID_Client | Nom    | Prénom | Ville  |
|-----------|--------|--------|--------|
| 1         | Martin | Marie  | Paris  |
| 2         | Martin | Marie  | Lyon   |
| 3         | Dupont | Jean   | Paris  |
```

Ici, `ID_Client` est la clé primaire. Même si deux clients ont le même nom, leur ID est unique.

### Caractéristiques d'une clé primaire

- **Unique** : pas deux lignes avec la même valeur
- **Non nulle** : ne peut jamais être vide
- **Immuable** : ne change jamais une fois attribuée
- Souvent de type **entier auto-incrémenté** (1, 2, 3, 4...)

## Les relations entre tables : le cœur du modèle relationnel

Le terme "relationnel" ne vient pas du mot "relation" au sens courant, mais de la théorie mathématique des relations. En pratique, cela signifie que **les tables peuvent être liées entre elles**.

### Pourquoi lier des tables ?

Imaginez une application de gestion de bibliothèque. Vous avez des livres et des auteurs. Un auteur peut écrire plusieurs livres. Si vous stockez tout dans une seule table :

```
| Titre                | Auteur        | Nationalité |
|----------------------|---------------|-------------|
| Les Misérables       | Victor Hugo   | Française   |
| Notre-Dame de Paris  | Victor Hugo   | Française   |
| Le Comte de Monte... | Alexandre Dumas | Française |
```

Vous **répétez** les informations sur Victor Hugo ! C'est du gaspillage et source d'erreurs (que se passe-t-il si vous faites une faute de frappe ?).

### Solution : séparer en deux tables

**Table Auteurs :**
```
| ID_Auteur | Nom             | Nationalité |
|-----------|-----------------|-------------|
| 1         | Victor Hugo     | Française   |
| 2         | Alexandre Dumas | Française   |
```

**Table Livres :**
```
| ID_Livre | Titre                        | ID_Auteur |
|----------|------------------------------|-----------|
| 1        | Les Misérables               | 1         |
| 2        | Notre-Dame de Paris          | 1         |
| 3        | Le Comte de Monte-Cristo     | 2         |
```

La colonne `ID_Auteur` dans la table `Livres` fait référence à `ID_Auteur` dans la table `Auteurs`. C'est ce qu'on appelle une **clé étrangère** (Foreign Key).

## Les clés étrangères : créer des liens

Une **clé étrangère** est une colonne (ou ensemble de colonnes) dans une table qui fait référence à la clé primaire d'une autre table.

### Rôle des clés étrangères

Les clés étrangères servent à :

1. **Créer des relations** entre tables
2. **Maintenir l'intégrité** des données (on ne peut pas référencer un ID qui n'existe pas)
3. **Éviter la duplication** d'informations

### Exemple concret : système de commandes

**Table Clients :**
```
| ID_Client | Nom    | Email              |
|-----------|--------|--------------------|
| 1         | Dubois | dubois@email.fr    |
| 2         | Martin | martin@email.fr    |
```

**Table Commandes :**
```
| ID_Commande | Date       | Montant | ID_Client |
|-------------|------------|---------|-----------|
| 101         | 2025-10-01 | 150.00  | 1         |
| 102         | 2025-10-05 | 200.00  | 2         |
| 103         | 2025-10-10 | 75.00   | 1         |
```

Ici, `ID_Client` dans la table `Commandes` est une clé étrangère qui pointe vers `ID_Client` dans la table `Clients`. Cela signifie :

- La commande 101 appartient au client Dubois
- La commande 102 appartient au client Martin
- La commande 103 appartient aussi au client Dubois

## Types de relations

Il existe trois types principaux de relations entre tables :

### 1. Relation Un-à-Plusieurs (1:N)

C'est la relation la plus courante. Un enregistrement de la table A peut être lié à plusieurs enregistrements de la table B, mais un enregistrement de B n'est lié qu'à un seul enregistrement de A.

**Exemple :** Un client peut avoir plusieurs commandes, mais une commande appartient à un seul client.

```
Client (1) ←→ (N) Commandes
```

### 2. Relation Plusieurs-à-Plusieurs (N:N)

Plusieurs enregistrements de A peuvent être liés à plusieurs enregistrements de B, et vice-versa.

**Exemple :** Un étudiant peut suivre plusieurs cours, et un cours peut avoir plusieurs étudiants.

Pour implémenter cette relation, on utilise une **table de liaison** (ou table d'association) :

```
Etudiants ←→ Inscriptions ←→ Cours
```

**Table Etudiants :**
```
| ID_Etudiant | Nom    |
|-------------|--------|
| 1           | Pierre |
| 2           | Marie  |
```

**Table Cours :**
```
| ID_Cours | Intitulé        |
|----------|-----------------|
| 101      | Mathématiques   |
| 102      | Informatique    |
```

**Table Inscriptions :**
```
| ID_Etudiant | ID_Cours |
|-------------|----------|
| 1           | 101      |
| 1           | 102      |
| 2           | 101      |
```

Cela signifie : Pierre suit Maths et Info, Marie suit Maths.

### 3. Relation Un-à-Un (1:1)

Un enregistrement de A est lié à un seul enregistrement de B, et vice-versa. C'est rare et généralement utilisé pour séparer des données sensibles ou optionnelles.

**Exemple :** Une personne a un seul passeport, et un passeport appartient à une seule personne.

## Intégrité référentielle

L'**intégrité référentielle** est un ensemble de règles qui garantissent la cohérence des relations entre tables.

### Principe

Si une table B contient une clé étrangère vers la table A, alors :

- On ne peut **pas insérer** dans B une valeur qui n'existe pas dans A
- On ne peut **pas supprimer** de A un enregistrement référencé par B (ou alors on doit gérer la cascade)

**Exemple :**

Si vous essayez de créer une commande avec `ID_Client = 99` alors que ce client n'existe pas, la base de données refusera l'opération.

De même, si vous essayez de supprimer un client qui a des commandes, vous devrez d'abord décider quoi faire de ses commandes.

## Normalisation : organiser efficacement

La **normalisation** est un processus qui consiste à organiser les tables de manière à :

1. **Éliminer la redondance** (ne pas répéter les informations)
2. **Assurer la cohérence** des données
3. **Faciliter la maintenance**

### Exemple de mauvaise organisation (non normalisée)

```
Table Commandes
| ID_Commande | Client_Nom | Client_Email    | Produit        | Prix  |
|-------------|------------|-----------------|----------------|-------|
| 1           | Dubois     | dubois@mail.fr  | Clavier        | 25.00 |
| 2           | Dubois     | dubois@mail.fr  | Souris         | 15.00 |
| 3           | Martin     | martin@mail.fr  | Écran          | 200.00|
```

**Problèmes :**
- Les infos client sont répétées
- Si Dubois change d'email, il faut modifier plusieurs lignes
- Risque d'incohérence (faute de frappe sur l'email)

### Exemple normalisé

**Table Clients :**
```
| ID_Client | Nom    | Email          |
|-----------|--------|----------------|
| 1         | Dubois | dubois@mail.fr |
| 2         | Martin | martin@mail.fr |
```

**Table Produits :**
```
| ID_Produit | Nom     | Prix  |
|------------|---------|-------|
| 1          | Clavier | 25.00 |
| 2          | Souris  | 15.00 |
| 3          | Écran   | 200.00|
```

**Table Commandes :**
```
| ID_Commande | ID_Client | ID_Produit |
|-------------|-----------|------------|
| 1           | 1         | 1          |
| 2           | 1         | 2          |
| 3           | 2         | 3          |
```

Maintenant, chaque information n'est stockée qu'une seule fois !

## Avantages du modèle relationnel

### 1. Pas de redondance
Les informations ne sont stockées qu'une seule fois, ce qui économise de l'espace et évite les incohérences.

### 2. Intégrité des données
Les contraintes (clés primaires, clés étrangères) garantissent que vos données restent cohérentes.

### 3. Flexibilité des requêtes
Vous pouvez combiner les données de différentes tables de multiples façons grâce au langage SQL.

### 4. Sécurité
Vous pouvez contrôler finement qui a accès à quelles tables et quelles opérations.

### 5. Concurrence
Plusieurs utilisateurs peuvent accéder aux mêmes données simultanément sans conflit.

## Vocabulaire récapitulatif

Pour bien comprendre les bases de données relationnelles, voici les termes essentiels :

- **Base de données** : ensemble organisé de tables
- **Table** : collection de données structurées (comme une feuille de tableur)
- **Ligne / Enregistrement / Tuple** : une entrée dans une table
- **Colonne / Champ / Attribut** : une propriété des enregistrements
- **Clé primaire** : identifiant unique d'une ligne
- **Clé étrangère** : référence vers la clé primaire d'une autre table
- **Relation** : lien logique entre deux tables
- **Intégrité référentielle** : règles garantissant la cohérence des relations
- **Normalisation** : processus d'organisation optimale des tables

## Analogie finale : le système de fiches

Imaginez une bibliothèque municipale avant l'informatique :

- **Fiches auteurs** : un tiroir avec une fiche par auteur (nom, nationalité, dates)
- **Fiches livres** : un autre tiroir avec une fiche par livre (titre, ISBN, numéro de l'auteur)
- **Fiches emprunts** : un troisième tiroir avec les emprunts (numéro livre, numéro adhérent, date)

Pour savoir qui a écrit "Les Misérables" :
1. Je cherche le livre dans les fiches livres
2. Je note le numéro d'auteur
3. Je cherche ce numéro dans les fiches auteurs
4. Je trouve "Victor Hugo"

C'est exactement le principe des bases de données relationnelles : des tables indépendantes reliées par des numéros (clés) !

## Transition vers la pratique

Maintenant que vous comprenez les concepts fondamentaux, vous êtes prêt à découvrir :

- **Le langage SQL** pour interroger et manipuler ces données
- **Les systèmes de gestion de bases de données** (SQLite, PostgreSQL, MariaDB)
- **L'intégration dans vos applications Pascal/Lazarus**

Dans les sections suivantes, nous verrons comment mettre ces concepts en pratique avec du code concret.

---

*Cette section pose les bases théoriques nécessaires. Ne vous inquiétez pas si tout n'est pas parfaitement clair immédiatement : la pratique rendra ces concepts beaucoup plus concrets !*

⏭️ [Introduction au SQL](/16-bases-donnees-maitrise-approfondie/02-introduction-sql.md)
