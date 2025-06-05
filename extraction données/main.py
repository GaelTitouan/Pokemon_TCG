from bs4 import BeautifulSoup, Tag
from dataclasses import dataclass, asdict
import aiohttp
import aiofile
import asyncio
import os
import json
import re

base_url = "https://play.limitlesstcg.com"
headers = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.106 Safari/537.36'}

# Sanitize Windows-reserved file/folder names
def sanitize_path_component(name):
    reserved_names = {"con", "prn", "aux", "nul", "com1", "lpt1"}
    name = name.lower()
    return "reserved" if name in reserved_names else name

# Dataclasses used for json generation
@dataclass
class DeckListItem:
    type: str
    url: str
    name: str
    count: int

@dataclass
class Player:
    id: str
    name: str
    placing: str
    country: str
    decklist: list[DeckListItem]

@dataclass
class MatchResult:
    player_id: str
    score: int

@dataclass
class Match:
    match_results: list[MatchResult]

@dataclass
class Tournament:
    id: str
    name: str
    date: str
    organizer: str
    format: str
    nb_players: str
    players: list[Player]
    matches: list[Match]

# Extract the tr tags from a table, omitting the first header
def extract_trs(soup: BeautifulSoup, table_class: str):
    trs = soup.find(class_=table_class).find_all("tr")
    trs.pop(0)  # Remove header
    return trs

# Url helpers
def construct_standings_url(tournament_id: str):
    return f"/tournament/{tournament_id}/standings?players"

def construct_pairings_url(tournament_id: str):
    return f"/tournament/{tournament_id}/pairings"

def construct_decklist_url(tournament_id: str, player_id: str):
    return f"/tournament/{tournament_id}/player/{player_id}/decklist"

# Check if the pairing page is a bracket (single elimination)
def is_bracket_pairing(pairings: BeautifulSoup):
    return pairings.find("div", class_="live-bracket") is not None

# Check if the pairing page is a table (swiss rounds)
regex_tournament_id = re.compile(r'[a-zA-Z0-9_\-]*')
def is_table_pairing(pairings: BeautifulSoup):
    pairings = pairings.find("div", class_="pairings")
    if pairings is not None:
        table = pairings.find("table", {'data-tournament': regex_tournament_id})
        return table is not None
    return False

# Return a list of matches from a bracket-style pairing page
def extract_matches_from_bracket_pairings(pairings: BeautifulSoup):
    matches = []
    matches_div = pairings.find("div", class_="live-bracket").find_all("div", class_="bracket-match")
    for match in matches_div:
        if match.find("a", class_="bye") is not None:
            continue
        players_div = match.find_all("div", class_="live-bracket-player")
        match_results = []
        for index in range(len(players_div)):
            player = players_div[index]
            match_results.append(MatchResult(
                player.attrs["data-id"],
                int(player.find("div", class_="score").attrs["data-score"])
            ))
        matches.append(Match(match_results))
    return matches
# Return a list of matches from a table-style pairing page
def extract_matches_from_table_pairings(pairings: BeautifulSoup):
    matches = []
    rows = pairings.find("div", class_="pairings").find("table").find_all("tr")[1:]  # Skip header
    for row in rows:
        match_results = []
        for td in row.find_all("td")[1:3]:  # Columns 1 and 2
            score = td.find("span", class_="match-score")
            score = int(score.text) if score else 0
            player_id = td.find("a")["href"].split("/")[-1]
            match_results.append(MatchResult(player_id, score))
        matches.append(Match(match_results))
    return matches

# Extracts all card entries from the decklist table
def extract_cards(decklist: BeautifulSoup, category: str):
    table = decklist.find("h2", string=category)
    if not table:
        return []

    table = table.find_next("table")
    cards = []
    for row in table.find_all("tr"):
        columns = row.find_all("td")
        count = int(columns[0].text.strip())
        name = columns[1].text.strip()
        url = base_url + columns[1].find("a")["href"]
        cards.append(DeckListItem(category, url, name, count))
    return cards

# Load tournament decklists and matches
async def fetch_tournament_data(session, tournament_id: str) -> Tournament:
    async with session.get(f"{base_url}/tournament/{tournament_id}") as response:
        soup = BeautifulSoup(await response.text(), "html.parser")
    name = soup.find("h1", class_="tournament-title").text.strip()
    info = soup.find("div", class_="tournament-meta").find_all("span")
    date = info[0].text.strip()
    organizer = info[1].text.strip()
    format = info[2].text.strip()
    nb_players = soup.find("span", class_="player-count").text.strip()

    standings_url = base_url + construct_standings_url(tournament_id)
    async with session.get(standings_url) as standings_response:
        standings_soup = BeautifulSoup(await standings_response.text(), "html.parser")

    players = []
    rows = extract_trs(standings_soup, "standings")
    for row in rows:
        cols = row.find_all("td")
        placing = cols[0].text.strip()
        player_id = cols[1].find("a")["href"].split("/")[-1]
        name = cols[1].text.strip()
        country = cols[2].text.strip()

        decklist_url = base_url + construct_decklist_url(tournament_id, player_id)
        async with session.get(decklist_url) as deck_response:
            deck_soup = BeautifulSoup(await deck_response.text(), "html.parser")

        cards = (
            extract_cards(deck_soup, "Pokémon") +
            extract_cards(deck_soup, "Trainer") +
            extract_cards(deck_soup, "Energy")
        )

        players.append(Player(player_id, name, placing, country, cards))

    pairings_url = base_url + construct_pairings_url(tournament_id)
    async with session.get(pairings_url) as pairing_response:
        pairing_soup = BeautifulSoup(await pairing_response.text(), "html.parser")

    if is_bracket_pairing(pairing_soup):
        matches = extract_matches_from_bracket_pairings(pairing_soup)
    elif is_table_pairing(pairing_soup):
        matches = extract_matches_from_table_pairings(pairing_soup)
    else:
        matches = []

    return Tournament(tournament_id, name, date, organizer, format, nb_players, players, matches)
# Write the tournament data to a JSON file
def write_json(tournament: Tournament, out_path: Path):
    data = tournament.model_dump(mode="json")
    json_data = json.dumps(data, indent=4)
    out_path.write_text(json_data, encoding="utf-8")

# Sanitize a string to be a valid filename
def sanitize_path_component(s: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_-]", "_", s)

# Scrape a single tournament and write it to JSON
async def scrape_tournament(tournament_id: str, out_dir: Path, proxy: Optional[str] = None):
    connector = TCPConnector(ssl=False)
    timeout = ClientTimeout(total=60)

    async with aiohttp.ClientSession(
        connector=connector,
        timeout=timeout,
        trust_env=True,  # Use system proxy settings (needed for some environments)
    ) as session:
        if proxy:
            session._default_headers["Proxy"] = proxy  # Explicit proxy, not ideal
        try:
            tournament = await fetch_tournament_data(session, tournament_id)
            safe_name = sanitize_path_component(tournament.name)
            filename = f"{safe_name}_{tournament.id}.json"
            out_path = out_dir / filename
            write_json(tournament, out_path)
            print(f"Saved: {out_path}")
        except Exception as e:
            print(f"Error scraping tournament {tournament_id}: {e}")

# Main async function
async def main():
    connector = aiohttp.TCPConnector(limit=20)
    sem = asyncio.Semaphore(50)

    async with aiohttp.ClientSession(
        base_url=base_url,
        connector=connector,
        proxy="http://ocytohe.univ-ubs.fr:3128"
    ) as session:
        # Exemple d'appel à une page (à adapter)
        await handle_tournament_list_page(
            session, sem, first_tournament_page="/tournaments?page=1"
        )

# Lancement du script
if __name__ == "__main__":
    asyncio.run(main())
