🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.6 Organigrammes et structuration de la pensée

## Introduction

Les organigrammes (ou logigrammes, flowcharts en anglais) sont des représentations graphiques d'algorithmes. Ils permettent de visualiser le flux d'exécution d'un programme de manière claire et intuitive. Dans cette section, nous allons apprendre à lire, créer et utiliser les organigrammes pour structurer notre pensée avant de programmer.

## Qu'est-ce qu'un organigramme ?

### Définition

Un **organigramme** est un diagramme qui représente visuellement la séquence d'opérations à effectuer pour résoudre un problème ou accomplir une tâche.

**Avantages des organigrammes :**
- **Visuel** : plus facile à comprendre qu'un texte
- **Universel** : compris par tous, indépendamment du langage de programmation
- **Clair** : met en évidence la logique du programme
- **Communication** : facilite les discussions en équipe
- **Documentation** : sert de référence pour comprendre le code

**Inconvénients :**
- Peut devenir complexe pour de grands programmes
- Prend du temps à dessiner
- Difficile à maintenir quand le code évolue

### Quand utiliser les organigrammes ?

Les organigrammes sont particulièrement utiles pour :
- Planifier un nouveau programme
- Comprendre un algorithme existant
- Identifier les erreurs de logique
- Expliquer un concept à d'autres personnes
- Documenter des processus complexes

## Les symboles standard des organigrammes

Les organigrammes utilisent des formes géométriques standardisées. Voici les symboles les plus courants :

### Symboles de base

**1. Terminal (Début/Fin)**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═══════════╝
```
- Forme : Ovale ou rectangle aux coins arrondis
- Usage : Marque le début et la fin d'un programme
- Texte : "DÉBUT", "FIN", "START", "END"

**2. Traitement (Instruction)**
```
     ┌─────────────┐
     │ Instruction │
     └─────────────┘
```
- Forme : Rectangle
- Usage : Représente une action, un calcul, une affectation
- Exemples : "somme ← a + b", "compteur ← compteur + 1"

**3. Entrée/Sortie**
```
     ╱───────────╲
    ╱ Afficher X  ╲
    ╲─────────────╱
```
- Forme : Parallélogramme
- Usage : Lecture de données ou affichage de résultats
- Exemples : "LIRE nombre", "AFFICHER résultat"

**4. Décision (Condition)**
```
          ╱╲
         ╱  ╲
        ╱Cond╲
        ╲ ? ╱
         ╲  ╱
          ╲╱
```
- Forme : Losange (diamant)
- Usage : Test d'une condition, choix entre deux chemins
- Sorties : Deux flèches (OUI/NON, VRAI/FAUX)
- Exemple : "x > 0 ?"

**5. Connecteur**
```
        ( A )
```
- Forme : Cercle
- Usage : Connexion entre parties d'un organigramme (évite les flèches trop longues)
- Contient : Une lettre ou un numéro

**6. Flèche de liaison**
```
        →  ou  ↓
```
- Usage : Indique le sens du flux, l'ordre d'exécution
- Toujours orientée

**7. Sous-programme (Appel de fonction/procédure)**
```
     ┌─────────────┐
     │ Fonction()  │
     │─────────────│
     └─────────────┘
```
- Forme : Rectangle avec deux barres verticales sur les côtés
- Usage : Appel d'une fonction ou procédure
- Exemple : "CalculerMoyenne()"

**8. Boucle (préparation)**
```
     ┌─────────────┐
     │   i = 1,10  │
     └─────────────┘
```
- Forme : Hexagone
- Usage : Initialisation d'une boucle FOR
- Exemple : "i DE 1 À 10"

## Construction d'organigrammes simples

### Exemple 1 : Programme séquentiel simple

**Problème :** Calculer et afficher la somme de deux nombres.

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ LIRE a, b    ╲
    ╲──────┬────────╱
           │
     ┌─────┴──────┐
     │ somme ← a+b│
     └─────┬──────┘
           │
     ╱─────┴──────╲
    ╱ AFFICHER     ╲
    ╲    somme     ╱
     ╲─────┬──────╱
           │
     ╔═════╧═════╗
     ║    FIN    ║
     ╚═══════════╝
```

**Pseudo-code correspondant :**
```
DÉBUT
    LIRE a, b
    somme ← a + b
    AFFICHER somme
FIN
```

### Exemple 2 : Structure de décision (SI...ALORS...SINON)

**Problème :** Déterminer si un nombre est positif ou négatif.

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ LIRE nombre  ╲
    ╲──────┬────────╱
           │
          ╱╲
         ╱  ╲
        ╱ nb ╲
       ╱  >=0 ╲____NON___
       ╲   ?  ╱           ╲
        ╲    ╱             │
         ╲  ╱              │
          ╲╱               │
       OUI │               │
           │               │
     ╱─────┴──────╲   ╱────┴─────╲
    ╱ AFFICHER     ╲ ╱ AFFICHER   ╲
    ╲  "Positif"   ╱ ╲  "Négatif" ╱
     ╲─────┬──────╱   ╲────┬─────╱
           │               │
           └───────┬───────┘
                   │
             ╔═════╧═════╗
             ║    FIN    ║
             ╚═══════════╝
```

**Pseudo-code correspondant :**
```
DÉBUT
    LIRE nombre
    SI nombre >= 0 ALORS
        AFFICHER "Positif"
    SINON
        AFFICHER "Négatif"
    FIN SI
FIN
```

### Exemple 3 : Boucle POUR

**Problème :** Afficher les nombres de 1 à 5.

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ┌─────┴──────┐
     │  i ← 1     │
     └─────┬──────┘
           │
          ╱╲
         ╱  ╲
        ╱ i  ╲
       ╱  <=5 ╲____NON___
       ╲   ?  ╱           ╲
        ╲    ╱             │
         ╲  ╱              │
          ╲╱               │
       OUI │               │
           │               │
     ╱─────┴──────╲        │
    ╱ AFFICHER i   ╲       │
    ╲──────┬────────╱       │
           │                │
     ┌─────┴──────┐         │
     │ i ← i + 1  │         │
     └─────┬──────┘         │
           │                │
           └────────────────┘
                            │
                      ╔═════╧═════╗
                      ║    FIN    ║
                      ╚═══════════╝
```

**Pseudo-code correspondant :**
```
DÉBUT
    i ← 1
    TANT QUE i <= 5 FAIRE
        AFFICHER i
        i ← i + 1
    FIN TANT QUE
FIN
```

### Exemple 4 : Boucle RÉPÉTER...JUSQU'À

**Problème :** Demander un mot de passe jusqu'à ce qu'il soit correct.

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ LIRE         ╲
    ╲   motDePasse ╱
     ╲─────┬──────╱
           │
          ╱╲
         ╱  ╲
        ╱ MDP╲
       ╱correct╲___NON___
       ╲   ?   ╱          ╲
        ╲     ╱            │
         ╲   ╱             │
          ╲╱               │
       OUI │               │
           │          ╱────┴─────╲
           │         ╱ AFFICHER   ╲
           │         ╲  "Erreur"  ╱
           │          ╲────┬─────╱
           │               │
           │               │
           │               │
           │  ╱────────────┘
           │ ╱
     ╱─────┴──────╲
    ╱ AFFICHER     ╲
    ╲  "Accès OK"  ╱
     ╲─────┬──────╱
           │
     ╔═════╧═════╗
     ║    FIN    ║
     ╚═══════════╝
```

## Exemples d'organigrammes complets

### Exemple 5 : Calcul de factorielle

**Problème :** Calculer n! (factorielle de n)

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ LIRE n       ╲
    ╲──────┬────────╱
           │
     ┌─────┴──────┐
     │ fact ← 1   │
     │ i ← 1      │
     └─────┬──────┘
           │
          ╱╲
         ╱  ╲
        ╱ i  ╲
       ╱  <= n╲____NON___
       ╲   ?  ╱           ╲
        ╲    ╱             │
         ╲  ╱              │
          ╲╱               │
       OUI │               │
           │               │
     ┌─────┴──────┐        │
     │fact ← fact │        │
     │     × i    │        │
     └─────┬──────┘        │
           │               │
     ┌─────┴──────┐        │
     │ i ← i + 1  │        │
     └─────┬──────┘        │
           │               │
           └───────────────┘
                           │
                     ╱─────┴──────╲
                    ╱ AFFICHER     ╲
                    ╲    fact      ╱
                     ╲─────┬──────╱
                           │
                     ╔═════╧═════╗
                     ║    FIN    ║
                     ╚═══════════╝
```

### Exemple 6 : Recherche du maximum dans un tableau

**Problème :** Trouver le plus grand élément d'un tableau.

**Organigramme :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ LIRE tableau ╲
    ╲   et taille  ╱
     ╲─────┬──────╱
           │
     ┌─────┴──────┐
     │ max ←      │
     │ tableau[0] │
     │ i ← 1      │
     └─────┬──────┘
           │
          ╱╲
         ╱  ╲
        ╱ i  ╲
       ╱<taille╲___NON___
       ╲   ?   ╱          ╲
        ╲     ╱            │
         ╲   ╱             │
          ╲╱               │
       OUI │               │
           │               │
          ╱╲               │
         ╱  ╲              │
        ╱tab[i]╲           │
       ╱  > max╲__NON__    │
       ╲   ?   ╱       ╲   │
        ╲     ╱         │  │
         ╲   ╱          │  │
          ╲╱            │  │
       OUI │            │  │
           │            │  │
     ┌─────┴──────┐     │  │
     │ max ←      │     │  │
     │ tableau[i] │     │  │
     └─────┬──────┘     │  │
           │            │  │
           └────┬───────┘  │
                │          │
     ┌──────────┴──────┐   │
     │ i ← i + 1       │   │
     └──────────┬──────┘   │
                │          │
                └──────────┘
                           │
                     ╱─────┴──────╲
                    ╱ AFFICHER     ╲
                    ╲     max      ╱
                     ╲─────┬──────╱
                           │
                     ╔═════╧═════╗
                     ║    FIN    ║
                     ╚═══════════╝
```

### Exemple 7 : Menu avec choix multiples

**Problème :** Afficher un menu et exécuter l'option choisie.

**Organigramme simplifié :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ╱─────┴──────╲
    ╱ AFFICHER menu╲
    ╲──────┬────────╱
           │
     ╱─────┴──────╲
    ╱ LIRE choix   ╲
    ╲──────┬────────╱
           │
          ╱╲
         ╱  ╲
        ╱choix╲
       ╱  = 1  ╲___NON___
       ╲   ?   ╱          ╲
        ╲     ╱            │
         ╲   ╱             │
          ╲╱               │
       OUI │              ╱╲
           │             ╱  ╲
     ┌─────┴──────┐     ╱choix╲
     │  Option 1  │    ╱  = 2  ╲___NON___
     └─────┬──────┘    ╲   ?   ╱          ╲
           │            ╲     ╱            │
           │             ╲   ╱            ╱╲
           │              ╲╱              ╱  ╲
           │           OUI │             ╱choix╲
           │               │            ╱  = 3  ╲___NON___
           │         ┌─────┴──────┐    ╲   ?   ╱          ╲
           │         │  Option 2  │     ╲     ╱            │
           │         └─────┬──────┘      ╲   ╱             │
           │               │              ╲╱               │
           │               │           OUI │               │
           │               │               │               │
           │               │         ┌─────┴──────┐  ┌─────┴──────┐
           │               │         │  Option 3  │  │   Erreur   │
           │               │         └─────┬──────┘  └─────┬──────┘
           │               │               │               │
           └───────┬───────┴───────┬───────┴───────┬───────┘
                   │               │               │
             ╔═════╧═══════════════╧═══════════════╧═════╗
             ║                  FIN                      ║
             ╚═══════════════════════════════════════════╝
```

## Structuration de la pensée avec les organigrammes

### Méthode de conception descendante (Top-Down)

La conception descendante consiste à décomposer un problème complexe en sous-problèmes plus simples.

**Étapes :**

1. **Identifier le problème principal**
   - Que doit faire le programme globalement ?

2. **Décomposer en grandes étapes**
   - Quelles sont les phases principales ?

3. **Raffiner chaque étape**
   - Détailler chaque phase en sous-étapes

4. **Continuer jusqu'aux opérations élémentaires**
   - Arrêter quand chaque bloc est simple

**Exemple : Programme de gestion de notes d'étudiants**

**Niveau 1 (global) :**
```
     ╔═══════════╗
     ║   DÉBUT   ║
     ╚═════╤═════╝
           │
     ┌─────┴──────┐
     │   Saisir   │
     │    notes   │
     └─────┬──────┘
           │
     ┌─────┴──────┐
     │  Calculer  │
     │  moyenne   │
     └─────┬──────┘
           │
     ┌─────┴──────┐
     │  Afficher  │
     │  résultats │
     └─────┬──────┘
           │
     ╔═════╧═════╗
     ║    FIN    ║
     ╚═══════════╝
```

**Niveau 2 (détaillé) :**
Chaque bloc du niveau 1 devient un organigramme complet.

### Identification des structures

Apprenez à reconnaître les patterns :

**1. Séquence (suite d'actions)**
```
Action A → Action B → Action C
```

**2. Alternative (choix)**
```
        Condition ?
         ╱      ╲
      OUI        NON
       │          │
    Action A   Action B
```

**3. Répétition (boucle)**
```
    ┌───────────┐
    │  Condition? ──NON──> Sortie
    └──┬────────┘
       OUI
       │
    ┌──┴────┐
    │ Action│
    └──┬────┘
       │
       └──> (retour)
```

## Passage de l'organigramme au code

### Correspondances directes

**Structure séquentielle :**
```
Organigramme:          Pascal:
┌─────────┐            a := 5;
│ a ← 5   │            b := 10;
└────┬────┘            c := a + b;
┌────┴────┐
│ b ← 10  │
└────┬────┘
┌────┴────┐
│c ← a+b  │
└─────────┘
```

**Structure alternative :**
```
Organigramme:          Pascal:
    ╱╲                 if x > 0 then
   ╱x>0╲                 WriteLn('Positif')
   ╲  ?╱               else
  OUI NON                WriteLn('Négatif');
   │   │
 ┌─┴─┐ ┌─┴─┐
 │Pos│ │Nég│
 └───┘ └───┘
```

**Structure répétitive :**
```
Organigramme:          Pascal:
 ┌─────────┐           i := 1;
 │ i ← 1   │           while i <= 10 do
 └────┬────┘           begin
     ╱╲                  WriteLn(i);
    ╱i≤10╲               i := i + 1;
    ╲  ?╱              end;
  OUI │ NON
   ┌──┴──┐
   │Aff i│
   └──┬──┘
   ┌──┴──┐
   │i←i+1│
   └──┬──┘
      │
   (retour)
```

## Outils pour créer des organigrammes

### Outils en ligne (gratuits)

**1. Draw.io (diagrams.net)**
- Gratuit et open source
- Interface intuitive
- Nombreux symboles prédéfinis
- Export en PNG, SVG, PDF

**2. Lucidchart**
- Version gratuite disponible
- Collaboration en temps réel
- Bibliothèques de formes

**3. Google Drawings**
- Intégré à Google Drive
- Simple et accessible
- Partage facile

### Logiciels de bureau

**1. Microsoft Visio**
- Professionnel et complet
- Payant
- Standard en entreprise

**2. LibreOffice Draw**
- Gratuit et open source
- Fonctionnalités complètes
- Compatible Windows, Linux, macOS

**3. Dia**
- Gratuit et open source
- Spécialisé dans les diagrammes techniques
- Léger et simple

### Outils de programmation

**1. Flowgorithm**
- Spécialement conçu pour l'apprentissage
- Permet d'exécuter l'organigramme
- Génère du code dans plusieurs langages

**2. yEd**
- Gratuit
- Mise en page automatique
- Excellent pour les grands diagrammes

## Bonnes pratiques pour les organigrammes

### 1. Commencez toujours par DÉBUT et terminez par FIN

```
✓ Correct:        ✗ Incorrect:
╔════════╗        ┌─────────┐
║ DÉBUT  ║        │ Action  │
╚═══╤════╝        └────┬────┘
    │             ┌────┴────┐
┌───┴────┐        │ Action  │
│ Action │        └─────────┘
└───┬────┘
╔═══╧════╗
║  FIN   ║
╚════════╝
```

### 2. Utilisez des flèches claires

- Une seule direction par flèche
- Évitez les croisements
- Privilégiez le flux de haut en bas et de gauche à droite

### 3. Soyez cohérent dans les symboles

Utilisez toujours les mêmes formes pour les mêmes types d'opérations.

### 4. Gardez-le simple

Si l'organigramme devient trop complexe :
- Décomposez en sous-organigrammes
- Utilisez des sous-programmes
- Créez plusieurs niveaux de détail

### 5. Annotez si nécessaire

Ajoutez des commentaires pour clarifier les parties complexes.

### 6. Testez mentalement votre organigramme

Suivez le chemin avec des valeurs d'exemple pour vérifier la logique.

## Erreurs courantes à éviter

### 1. Boucle infinie

```
✗ Incorrect:
    ╱╲
   ╱  ╲
  ╱ i>0╲
  ╲ ?  ╱
   ╲  ╱
    ╲╱
  OUI│
  ┌──┴──┐
  │i←i+1│  ← i augmente, ne diminue jamais !
  └──┬──┘
     │
  (retour au test)
```

### 2. Condition sans action

```
✗ Incorrect:
    ╱╲
   ╱  ╲
  ╱ a>b╲
  ╲ ?  ╱
   ╲  ╱
    ╲╱
  OUI│NON  ← Que faire dans chaque cas ?
     │
```

### 3. Flux non défini

Chaque chemin doit mener quelque part (FIN ou retour).

### 4. Trop de détails

Ne mettez pas des détails triviaux comme "ouvrir le programme" ou "fermer la fenêtre".

### 5. Oublier les cas limites

N'oubliez pas de gérer :
- Tableaux vides
- Valeurs nulles
- Divisions par zéro

## Organigrammes vs Pseudo-code

### Quand utiliser l'un ou l'autre ?

**Organigrammes :**
- ✓ Visualiser la logique globale
- ✓ Expliquer à des non-programmeurs
- ✓ Identifier les flux alternatifs
- ✓ Algorithmes courts et moyens

**Pseudo-code :**
- ✓ Programmes longs et complexes
- ✓ Plus proche du code réel
- ✓ Plus rapide à écrire
- ✓ Facilite la traduction en code

**Meilleure approche :**
Utiliser les deux ensemble :
1. Organigramme pour la vue d'ensemble
2. Pseudo-code pour les détails

## Conclusion

Les organigrammes sont un outil puissant pour visualiser et structurer votre pensée avant de programmer. Ils vous permettent de :
- Clarifier la logique de votre programme
- Identifier les erreurs avant de coder
- Communiquer vos idées efficacement
- Planifier la structure de votre code

**Points clés à retenir :**
- Les symboles standard (ovale, rectangle, losange, parallélogramme) ont chacun un usage précis
- Un organigramme commence par DÉBUT et se termine par FIN
- Les flèches indiquent le flux d'exécution
- Décomposez les problèmes complexes en sous-organigrammes
- Testez mentalement votre organigramme avant de coder
- Combinez organigrammes et pseudo-code pour une meilleure conception

**Conseil pratique :**
Pour vos premiers programmes, dessinez toujours un organigramme simple avant de commencer à coder. Cette habitude vous fera gagner beaucoup de temps et vous évitera de nombreuses erreurs de logique.

Dans la prochaine section, nous allons découvrir les systèmes d'exploitation et comment interagir avec eux, ce qui nous préparera à installer et utiliser FreePascal et Lazarus.

⏭️ [Introduction aux systèmes d'exploitation](/01-prerequis-bases-programmation/07-introduction-systemes-exploitation.md)
