import os
import json
import csv
from collections import defaultdict

# Chemins
input_folder = r'D:\DATA_SAE601\output - Copie'
output_joueurs = r'H:\SAE_601\joueurs.csv'
output_matchs = r'H:\SAE_601\matchs.csv'
output_duels = r'H:\SAE_601\duels.csv'

# Stockage
joueurs_rows = []
matchs_rows = []
duels_rows = []

# Parcours de chaque fichier JSON
for filename in os.listdir(input_folder):
    if filename.endswith('.json'):
        filepath = os.path.join(input_folder, filename)
        with open(filepath, 'r', encoding='utf-8') as f:
            try:
                data = json.load(f)
            except json.JSONDecodeError:
                print(f"Erreur JSON : {filename}")
                continue

            # Infos tournoi
            tid = data.get("id")
            tname = data.get("name")
            tdate = data.get("date")
            organizer = data.get("organizer")
            format_ = data.get("format")
            nb_players = data.get("nb_players")

            # Dictionnaire joueur
            player_dict = {p["id"]: p for p in data.get("players", [])}

            # Score total
            player_scores = defaultdict(int)

            for match_index, match in enumerate(data.get("matches", []), start=1):
                results = match.get("match_results", [])
                if len(results) == 2:
                    p1 = results[0]
                    p2 = results[1]

                    duels_rows.append([
                        tid, match_index,
                        p1["player_id"], p1["score"],
                        p2["player_id"], p2["score"]
                    ])

                for res in results:
                    pid = res.get("player_id")
                    score = res.get("score", 0)
                    player_scores[pid] += score
                    matchs_rows.append([tid, match_index, pid, score])

            for pid, player in player_dict.items():
                pname = player.get("name")
                placing = player.get("placing")
                country = player.get("country")
                score_total = player_scores.get(pid, 0)

                decklist_items = []
                for card in player.get("decklist", []):
                    count = card.get("count", 0)
                    cname = card.get("name", "")
                    ctype = card.get("type", "")
                    decklist_items.append(f"{count}x {cname} ({ctype})")
                decklist = " | ".join(decklist_items)

                joueurs_rows.append([
                    tid, tname, tdate, organizer, format_, nb_players,
                    pid, pname, placing, country, score_total, decklist
                ])

# Création du dossier si besoin
os.makedirs(os.path.dirname(output_joueurs), exist_ok=True)

# Écriture des joueurs
with open(output_joueurs, 'w', newline='', encoding='utf-8-sig') as f:
    writer = csv.writer(f)
    writer.writerow([
        "tournament_id", "tournament_name", "tournament_date", "organizer", "format", "nb_players",
        "player_id", "player_name", "placing", "country", "score_total", "decklist"
    ])
    writer.writerows(joueurs_rows)

# Écriture des matchs
with open(output_matchs, 'w', newline='', encoding='utf-8-sig') as f:
    writer = csv.writer(f)
    writer.writerow([
        "tournament_id", "match_number", "player_id", "match_score"
    ])
    writer.writerows(matchs_rows)

# Écriture des duels
with open(output_duels, 'w', newline='', encoding='utf-8-sig') as f:
    writer = csv.writer(f)
    writer.writerow([
        "tournament_id", "match_number",
        "player1_id", "player1_score",
        "player2_id", "player2_score"
    ])
    writer.writerows(duels_rows)

print("✅ 3 fichiers CSV générés :")
print(f" - {output_joueurs}")
print(f" - {output_matchs}")
print(f" - {output_duels}")
