import os
import json
import csv

input_folder = r'D:\DATA_SAE601\pokemon-tcg-pocket-card-database\cards\en'
output_csv = r'H:\SAE_601\Pokemon_TCG\list_card_TCG.csv'

header = ["id", "name", "type", "subtype", "element", "health", "evolvesFrom", "rarity"]

rows = []

for filename in os.listdir(input_folder):
    if filename.endswith('.json'):
        filepath = os.path.join(input_folder, filename)
        with open(filepath, 'r', encoding='utf-8') as f:
            try:
                cards = json.load(f)
            except json.JSONDecodeError:
                print(f"Erreur JSON dans : {filename}")
                continue

            # Ici, cards est une liste d'objets (cartes)
            for card in cards:
                rows.append([
                    card.get("id", ""),
                    card.get("name", ""),
                    card.get("type", ""),
                    card.get("subtype", ""),
                    card.get("element", ""),
                    str(card.get("health", "")),
                    card.get("evolvesFrom") or "",
                    card.get("rarity", "")
                ])

with open(output_csv, 'w', newline='', encoding='utf-8-sig') as f:
    writer = csv.writer(f)
    writer.writerow(header)
    writer.writerows(rows)

print(f"✅ Fichier CSV généré avec {len(rows)} cartes : {output_csv}")
