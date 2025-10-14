🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.1 Concept d'exception

## Introduction

Jusqu'à présent, nous avons écrit des programmes en supposant que tout se passerait bien : l'utilisateur entre des données correctes, les fichiers existent, la mémoire est disponible, etc. Mais dans la réalité, les choses ne se passent pas toujours comme prévu. Les exceptions sont un mécanisme puissant pour gérer ces situations problématiques de manière élégante et structurée.

## Qu'est-ce qu'une erreur ?

Avant de parler d'exceptions, parlons d'erreurs. Une erreur est une situation anormale qui empêche votre programme de fonctionner correctement. Il existe plusieurs types d'erreurs :

### Erreurs de compilation
Ce sont les erreurs détectées par le compilateur avant même que votre programme ne s'exécute. Par exemple :
- Une faute de syntaxe
- Une variable non déclarée
- Un type incompatible

Ces erreurs sont faciles à gérer car le compilateur vous empêche de lancer le programme tant qu'elles ne sont pas corrigées.

### Erreurs d'exécution (Runtime Errors)
Ces erreurs surviennent pendant que votre programme s'exécute. Par exemple :
- Division par zéro
- Accès à un fichier qui n'existe pas
- Mémoire insuffisante
- Conversion de texte en nombre impossible ("abc" ne peut pas devenir un entier)
- Accès à un élément hors des limites d'un tableau

C'est pour gérer ce type d'erreurs que les exceptions ont été créées.

## Qu'est-ce qu'une exception ?

Une **exception** est un événement qui se produit pendant l'exécution d'un programme et qui perturbe le déroulement normal des instructions.

### Analogie de la vie quotidienne

Imaginez que vous suivez une recette de cuisine :
1. Sortir les ingrédients
2. Préchauffer le four à 180°C
3. Mélanger la farine et le sucre
4. Ajouter les œufs
5. Enfourner pendant 30 minutes

Mais que se passe-t-il si :
- Vous n'avez plus d'œufs ? (Exception : ressource manquante)
- Le four ne fonctionne pas ? (Exception : matériel défaillant)
- Vous laissez tomber le bol ? (Exception : incident inattendu)

Dans chacun de ces cas, vous ne pouvez pas continuer la recette normalement. Vous devez **gérer l'exception** : aller acheter des œufs, appeler un réparateur, nettoyer et recommencer, etc.

C'est exactement ce que font les exceptions en programmation : elles permettent de gérer les situations anormales sans que le programme ne s'arrête brutalement.

## Comment fonctionnaient les programmes avant les exceptions ?

Avant l'invention des exceptions, les programmeurs devaient vérifier le résultat de chaque opération :

```pascal
var
  f: TextFile;
  resultat: Integer;
begin
  resultat := OuvrirFichier(f, 'donnees.txt');
  if resultat <> 0 then
  begin
    WriteLn('Erreur d''ouverture');
    Exit;
  end;

  resultat := LireLigne(f);
  if resultat <> 0 then
  begin
    WriteLn('Erreur de lecture');
    FermerFichier(f);
    Exit;
  end;

  // ... et ainsi de suite
end;
```

Ce code devient vite illisible et difficile à maintenir. Les vérifications d'erreurs se mélangent avec la logique principale du programme.

## L'avantage des exceptions

Avec les exceptions, le code devient beaucoup plus clair :

```pascal
try
  OuvrirFichier(f, 'donnees.txt');
  LireLigne(f);
  TraiterDonnees;
  FermerFichier(f);
except
  WriteLn('Une erreur s''est produite');
end;
```

La logique principale (le code qu'on veut exécuter) est séparée de la gestion des erreurs. C'est plus lisible et plus facile à maintenir.

## Quand une exception est-elle "levée" ?

On dit qu'une exception est **levée** (ou **déclenchée**, en anglais "raised" ou "thrown") quand une erreur se produit. À ce moment :

1. L'exécution normale du programme s'arrête immédiatement
2. Le programme cherche un bloc de code capable de gérer cette exception
3. Si un gestionnaire est trouvé, il est exécuté
4. Si aucun gestionnaire n'est trouvé, le programme s'arrête avec un message d'erreur

## Situations courantes générant des exceptions

Voici quelques situations classiques où des exceptions peuvent survenir :

### Opérations mathématiques
- Division par zéro
- Dépassement de capacité (un nombre trop grand)
- Racine carrée d'un nombre négatif

### Manipulation de fichiers
- Fichier introuvable
- Permissions insuffisantes
- Disque plein
- Fichier déjà ouvert

### Gestion de la mémoire
- Mémoire insuffisante
- Accès à une zone mémoire invalide

### Conversion de données
- Conversion de chaîne en nombre impossible
- Format de date invalide

### Accès aux collections (tableaux, listes)
- Index hors limites
- Liste vide

## La philosophie des exceptions

Les exceptions reposent sur un principe simple : **ne pas ignorer les erreurs**.

Sans exceptions, un programme peut :
- Continuer avec des données incorrectes
- Produire des résultats faux sans que personne ne s'en aperçoive
- Corrompre des fichiers ou des bases de données

Avec les exceptions, vous êtes **obligé** de penser aux erreurs possibles et de décider comment les gérer.

## Le vocabulaire des exceptions

Quelques termes importants à connaître :

- **Lever (Raise)** : déclencher une exception
- **Capturer (Catch/Handle)** : intercepter et gérer une exception
- **Propager** : laisser l'exception remonter au niveau supérieur
- **Type d'exception** : la catégorie de l'erreur (division par zéro, fichier introuvable, etc.)
- **Message d'exception** : une description textuelle de l'erreur

## Ce que nous allons apprendre

Dans les sections suivantes de ce chapitre, nous verrons :

- Comment capturer et gérer les exceptions avec `try-except-finally`
- Comment créer nos propres exceptions
- Les différents types d'exceptions disponibles en FreePascal
- Les bonnes pratiques pour une gestion efficace des erreurs

## Conclusion

Les exceptions sont un outil fondamental de la programmation moderne. Elles permettent de :

- Séparer la logique principale du code de gestion des erreurs
- Rendre le code plus lisible et maintenable
- Garantir que les erreurs ne passent pas inaperçues
- Créer des programmes plus robustes et fiables

Au début, les exceptions peuvent sembler compliquées, mais avec la pratique, elles deviendront un réflexe naturel. Vous vous demanderez même comment vous avez pu programmer sans elles !

---

**Points clés à retenir :**

- Une exception est un événement anormal qui perturbe l'exécution du programme
- Les exceptions permettent de gérer les erreurs de manière structurée
- Elles séparent la logique métier de la gestion des erreurs
- Une exception non gérée arrête le programme
- Les exceptions rendent le code plus robuste et plus facile à maintenir

⏭️ [Try-except-finally](/13-gestion-exceptions/02-try-except-finally.md)
