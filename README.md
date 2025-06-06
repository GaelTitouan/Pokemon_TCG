# Projet SAE 601 – Traitement de Données du Jeu Pokémon TCG

Ce projet a été réalisé dans le cadre de la SAE 601. L'objectif est d'exploiter les données des tournois du jeu de cartes **Pokémon TCG Pocket**.
Nous avons récupéré des données au format JSON, pour les transformer et les centraliser dans une base de données PostgreSQL, puis nous avons réalisé les analyses sur RStudio.

## ▶️ Exécution des scripts

### 1. Extraction des données des tournois

Le script `pokemon_json_en_csv.py` lit des fichiers JSON du dossier "Output" contenant des informations de tournois Pokémon TCG (joueurs, scores, decks...) et génère trois fichiers CSV.

#### 📌 Pour exécuter :

```bash
python extraction de données/pokemon_json_en_csv.py # Adapté le chemin d'accès en fonction de ou vous avez mit le fichier .py
```

!! Assurez-vous de modifier les chemins suivants dans le script :

```python
# Chemins
input_folder = r'D:\DATA_SAE601\output'
output_joueurs = r'H:\SAE_601\Pokemon_TCG\SAE601_2025\Data\joueur_match_duels\joueurs.csv'
output_matchs = r'H:\SAE_601\Pokemon_TCG\SAE601_2025\Data\joueur_match_duels\matchs.csv'
output_duels = r'H:\SAE_601\Pokemon_TCG\SAE601_2025\Data\joueur_match_duels\duels.csv'
```

### 🔄 Fichiers générés :
- `joueurs.csv` : Informations générales sur chaque joueur et leur deck.
- `matchs.csv` : Score de chaque joueur dans chaque match.
- `duels.csv` : Résultat des duels entre les joueurs dans chaque match.

---

### 2. Extraction des cartes Pokémon
On a récupéré la liste des carte pokemon de TCG Pocket via un repository git accessible via ce lien https://github.com/hugoburguete/pokemon-tcg-pocket-card-database , il liste les données des cartes dans des fichiers json.
Le script `liste_card_TCG.py` lit tous les fichiers `.json` contenant des cartes depuis le dossier source, et génère un fichier `list_card_TCG.csv` contenant les attributs principaux de chaque carte (type, rareté...).

#### 📌 Pour exécuter :

```bash
python extraction de données/liste_card_TCG.py # Adapté le chemin d'accès en fonction de ou vous avez mit le fichier .py
```

!! Assurez-vous de modifier les chemins suivants dans le script si besoin :

```python
input_folder = r'D:\DATA_SAE601\pokemon-tcg-pocket-card-database\cards\en' #git clone https://github.com/hugoburguete/pokemon-tcg-pocket-card-database
output_csv = r'H:\SAE_601\Pokemon_TCG\SAE601_2025\Data\list_card_TCG.csv'
```

Le fichier CSV final contient les colonnes suivantes :
- `id`, `name`, `type`, `subtype`, `element`, `health`, `evolvesFrom`, `rarity`

---
# PostGreSQL
Notre base de données PostgreSQL contient les données des fichiers générer avec les données de tournois ainsi que les données sur les caractéristiques des cartes pokémons et dresseurs.
Cette Base de données PostgreSQL est rempli grace au script python fichier **main_postgre** qui va chercher ce qu'il y a dans les fichier csv. Pour nous connecter à la base, il a fallu créer un fichier contenant les variables d'environnement de la base.

# Data-Viz
La Data vizualisation a été réaliser sur Rstudio, elle contient 4 pages : 
  - Accueil
  - Cartes
  - Decks
  - Statistiques

Nous avons décider de crée l'application en anglais, car les données des cartes sont en anglais et l'appli a plutot pour but d'être utilisée à l'international

## ✅ Résultat final

Au terme de l'exécution, les fichiers `.csv` générés permettent d’analyser :
- Les caractéristiques des cartes Pokémon (nom, type, rareté, points de vie, etc.),
- Les performances des joueurs en tournoi,
- Les résultats détaillés des matchs et des duels.

Ce travail permet une **exploitation automatisée** et structurée des données brutes fournies au format JSON, en vue d'une **analyse statistique**, d’une **visualisation** ou d’une **intégration dans une base de données**.

---

## 👤 Auteur

- **Thomas Defoulounoux** 
- **Gaël Hellegouarch**
- **Titouan Le Gall**
- **Lukas Le Plaire**

