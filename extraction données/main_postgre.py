from dotenv import load_dotenv
import psycopg
import os
import json
import csv
from datetime import datetime
from pathlib import Path

load_dotenv(dotenv_path=Path(__file__).parent / "Environnement.env")

# DEBUG : affiche les variables pour vérifier qu'elles ne sont pas None
for var in (
    "POSTGRES_DB",
    "POSTGRES_USER",
    "POSTGRES_PASSWORD",
    "POSTGRES_HOST",
    "POSTGRES_PORT",
):
    print(f"{var} = {os.environ.get(var)!r}")

postgres_db=os.environ.get('POSTGRES_DB')
postgres_user=os.environ.get('POSTGRES_USER')
postgres_password=os.environ.get('POSTGRES_PASSWORD')
postgres_host=os.environ.get('POSTGRES_HOST')
postgres_port=os.environ.get('POSTGRES_PORT')

output_directory = "C:/Users/lukas/OneDrive/Bureau/601/output"
csv_directory = "C:/Users/lukas/OneDrive/Bureau/601/New_Data"

def get_connection_string():
  return f"postgresql://{postgres_user}:{postgres_password}@{postgres_host}:{postgres_port}/{postgres_db}"

def execute_sql_script(path: str):
  with psycopg.connect(get_connection_string()) as conn:
    with conn.cursor() as cur:
      with open(path) as f:
        cur.execute(f.read())

def insert_wrk_tournaments():
  tournament_data = []
  for file in os.listdir(output_directory):
    with open(f"{output_directory}/{file}") as f:
      tournament = json.load(f)
      tournament_data.append((
        tournament['id'], 
        tournament['name'], 
        datetime.strptime(tournament['date'], '%Y-%m-%dT%H:%M:%S.000Z'),
        tournament['organizer'], 
        tournament['format'], 
        int(tournament['nb_players'])
        ))
  
  with psycopg.connect(get_connection_string()) as conn:
    with conn.cursor() as cur:
      cur.executemany("INSERT INTO public.wrk_tournaments values (%s, %s, %s, %s, %s, %s)", tournament_data)

def insert_wrk_decklists():
  decklist_data = []
  for file in os.listdir(output_directory):
    with open(f"{output_directory}/{file}") as f:
      tournament = json.load(f)
      tournament_id = tournament['id']
      for player in tournament['players']:
        player_id = player['id']
        for card in player['decklist']:
          decklist_data.append((
            tournament_id,
            player_id,
            card['type'],
            card['name'],
            card['url'],
            int(card['count']),
          ))
  
  with psycopg.connect(get_connection_string()) as conn:
    with conn.cursor() as cur:
      cur.executemany("INSERT INTO public.wrk_decklists values (%s, %s, %s, %s, %s, %s)", decklist_data)

def insert_players_from_csv():
    player_data = []
    with open("C:/Users/lukas/OneDrive/Bureau/601/New_Data/players.csv", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f, delimiter = ',')
        for row in reader:
            player_data.append((
                row["tournament_id"],
                row["tournament_name"],
                row["tournament_date"],
                row["organizer"],
                row["format"],
                int(row["nb_players"]),
                row["player_id"],
                row["player_name"],
                int(row["placing"]),
                row["country"],
                int(row["score_total"]),
                row["decklist"]
            ))

    with psycopg.connect(get_connection_string()) as conn:
        with conn.cursor() as cur:
            cur.executemany("INSERT INTO public.wrk_players values (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", player_data)


def insert_wrk_duels_from_csv():
    duels_data = []
    with open("C:/Users/lukas/OneDrive/Bureau/601/New_Data/duels.csv", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f, delimiter = ',')
        for row in reader:
            duels_data.append((
                row["tournament_id"],
                int(row["match_number"]),
                row["player1_id"],
                int(row["player1_score"]),
                row["player2_id"],
                int(row["player2_score"])
            ))

    with psycopg.connect(get_connection_string()) as conn:
        with conn.cursor() as cur:
            cur.executemany("INSERT INTO public.wrk_duels values (%s, %s, %s, %s, %s, %s)", duels_data)

def insert_wrk_matchs_from_csv():
    matchs_data = []
    with open("C:/Users/lukas/OneDrive/Bureau/601/New_Data/matchs.csv", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f, delimiter = ',')
        for row in reader:
            matchs_data.append((
                row["tournament_id"],
                int(row["match_number"]),
                row["player_id"],
                int(row["match_score"])
            ))

    with psycopg.connect(get_connection_string()) as conn:
        with conn.cursor() as cur:
            cur.executemany("INSERT INTO public.wrk_matchs values (%s, %s, %s, %s)", matchs_data)

def int_or_none(value):
    if value is None:
        return None
    value = value.strip()
    if value == "" or value.lower() == "none":
        return None
    return int(value)


def insert_cards_new_from_csv():
    cards_data = []
    with open("C:/Users/lukas/OneDrive/Bureau/601/New_Data/cards.csv", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f, delimiter = ';')
        for row in reader:
            cards_data.append((
                row["id"],
                row["card_name"],
                row["card_type"],
                row["card_subtype"],
                row["card_element"],
                int_or_none(row["health"]),
                row["evolvesFrom"],
                row["rarity"]
            ))

    with psycopg.connect(get_connection_string()) as conn:
        with conn.cursor() as cur:
            cur.executemany("INSERT INTO public.cards_new values (%s, %s, %s, %s, %s, %s, %s, %s)", cards_data)


print("creating work tables")
execute_sql_script("C:/Users/lukas/OneDrive/Bureau/601/00_create_wrk_tables.sql")

print("insert raw tournament data")
insert_wrk_tournaments()

print("insert raw decklist data")
insert_wrk_decklists()

print("construct card database")
execute_sql_script("C:/Users/lukas/OneDrive/Bureau/601/01_dwh_cards.sql")

print("creating work tables for players, duels and matchs")
execute_sql_script("C:/Users/lukas/OneDrive/Bureau/601/02_create_wrk_tables_players.sql")

print("Import des données players depuis CSV")
insert_players_from_csv()

print("Import des duels depuis CSV")
insert_wrk_duels_from_csv()

print("Import des résultats de match depuis CSV")
insert_wrk_matchs_from_csv()

print("creating new cards data")
execute_sql_script("C:/Users/lukas/OneDrive/Bureau/601/03_cards_new.sql")

print("Import des résultats de match depuis CSV")
insert_cards_new_from_csv()