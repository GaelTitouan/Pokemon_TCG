library(plotly)
library(shiny)
library(DT)
library(glue)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(DBI)
library(RMySQL)

con <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,
  dbname = "postgres",
  user = "postgres",
  password = "SAE-601",
  connect_timeout = 10,
  sslmode = "prefer"
)

players <- dbGetQuery(con, "SELECT * FROM wrk_players")
cards_new <- dbGetQuery(con, "SELECT * FROM cards_new")
matchs <- dbGetQuery(con, "SELECT * FROM wrk_matchs")
duels <- dbGetQuery(con, "SELECT * FROM wrk_duels")
tournaments <- dbGetQuery(con, "SELECT * FROM wrk_tournaments")
cards <- dbGetQuery(con, "SELECT * FROM dwh_cards")
decklist <- dbGetQuery(con, "SELECT * FROM wrk_decklists")

dbDisconnect(con)

duels_outcomes <- duels %>%
  mutate(
    winner_id = case_when(
      player1_score > player2_score ~ player1_id,
      player2_score > player1_score ~ player2_id,
      TRUE ~ NA_character_
    ),
    loser_id = case_when(
      player1_score < player2_score ~ player1_id,
      player2_score < player1_score ~ player2_id,
      TRUE ~ NA_character_
    )
  )

wins_by_player <- duels_outcomes %>%
  filter(!is.na(winner_id)) %>%
  group_by(tournament_id, player_id = winner_id) %>%
  summarise(wins = n(), .groups = "drop")
  
losses_by_player <- duels_outcomes %>%
  filter(!is.na(loser_id)) %>%
  group_by(tournament_id, player_id = loser_id) %>%
  summarise(losses = n(), .groups = "drop")

players_with_results <- players %>%
  left_join(wins_by_player, by = c("tournament_id", "player_id")) %>%
  left_join(losses_by_player, by = c("tournament_id", "player_id")) %>%
  mutate(
    wins = coalesce(wins, 0),
    losses = coalesce(losses, 0)
  )


# Fonction pour extraire les deux Pokémon ex les plus évolués
extract_top2_ex_or_full_evolved_verbose <- function(decklist, index = NULL) {
  if (!is.null(index)) cat("Traitement ligne :", index, "\n")
  
  if (is.na(decklist) || decklist == "") return(NA_character_)
  
  cards <- str_split(decklist, "\\s*\\|\\s*")[[1]]
  
  df <- tibble(card = cards) %>%
    filter(str_detect(card, "\\(Pokémon\\)")) %>%
    mutate(
      count_name = str_extract(card, "^[0-9]+x\\s[^\\(]+"),
      name = str_trim(str_remove(count_name, "^[0-9]+x\\s")),
      number = str_extract(card, "A[0-9]+-[0-9]+"),
      base_name = str_remove(name, "\\s+ex$"),
      is_ex = str_detect(name, "\\s+ex$"),
      set = str_extract(number, "A[0-9]+"),
      num = as.integer(str_extract(number, "(?<=-)\\d+"))
    ) %>%
    filter(!is.na(number))
  
  if (nrow(df) == 0) return(NA_character_)
  
  # Si au moins 1 ex, on garde que les ex
  if (any(df$is_ex)) {
    df <- df %>% filter(is_ex)
  } else {
    # Sinon, on garde uniquement les cartes full évoluées :
    # Garder uniquement la plus évoluée par chaîne évolutive
    max_per_chain <- df %>%
      group_by(base_name, set) %>%
      filter(num == max(num)) %>%
      ungroup()
    
    # Supprimer les pré-évolutions si la carte plus évoluée existe dans la même série
    df <- df %>% 
      filter(map_lgl(row_number(), function(i) {
        row <- df[i, ]
        max_num_in_chain <- max_per_chain %>%
          filter(base_name == row$base_name, set == row$set) %>%
          pull(num)
        length(max_num_in_chain) > 0 && row$num == max_num_in_chain
      }))
  }
  
  # Trier ex d'abord, puis num décroissant
  sorted <- df %>%
    arrange(desc(is_ex), desc(num))
  
  top2 <- sorted %>% slice_head(n = 2)
  
  paste(top2$name, collapse = " / ")
}

players_with_results <- players_with_results %>%
  mutate(row_index = row_number())

players_with_results$top2_pokemon <- NA_character_

for (i in seq_len(nrow(players_with_results))) {
  cat("Traitement ligne :", i, "\n")
  players_with_results$top2_pokemon[i] <- extract_top2_ex_or_full_evolved_verbose(
    players_with_results$decklist[i], 
    i
  )
}

players_with_results <- players_with_results %>% select(-row_index)

saveRDS(cards, "C:/Users/tdefoulounoux/Desktop/Rshiny/cards.rds")
saveRDS(cards_new, "C:/Users/tdefoulounoux/Desktop/Rshiny/cards_new.rds")
saveRDS(decklist, "C:/Users/tdefoulounoux/Desktop/Rshiny/decklist.rds")
saveRDS(duels_outcomes, "C:/Users/tdefoulounoux/Desktop/Rshiny/duels_oucomes.rds")
saveRDS(matchs, "C:/Users/tdefoulounoux/Desktop/Rshiny/matchs.rds")
saveRDS(players_with_results, "C:/Users/tdefoulounoux/Desktop/Rshiny/players_with_results.rds")
saveRDS(tournaments, "C:/Users/tdefoulounoux/Desktop/Rshiny/tournaments.rds")

cards <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/cards.rds")
cards_new <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/cards_new.rds")
decklist <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/decklist.rds")
duels_outcomes <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/duels_oucomes.rds")
matchs <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/matchs.rds")
players_with_results <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/players_with_results.rds")
tournaments <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/tournaments.rds")


duels_outcomes <- duels_outcomes %>%
  # Ajout des top2 pour player1
  left_join(players_with_results %>% 
              select(tournament_id, player_id, top2_pokemon) %>%
              rename(player1_id = player_id, top2_pokemon_1 = top2_pokemon),
            by = c("tournament_id", "player1_id")) %>%
  # Ajout des top2 pour player2
  left_join(players_with_results %>% 
              select(tournament_id, player_id, top2_pokemon) %>%
              rename(player2_id = player_id, top2_pokemon_2 = top2_pokemon),
            by = c("tournament_id", "player2_id"))

duels_analysis <- duels_outcomes %>%
  mutate(
    duo1 = top2_pokemon_1,
    duo2 = top2_pokemon_2,
    winner = case_when(
      player1_score > player2_score ~ "duo1",
      player1_score < player2_score ~ "duo2",
      TRUE ~ "draw"
    )
  ) %>%
  filter(winner != "draw")  # Exclure les égalités

# Étape 2 : Calcul des stats par matchup
matchups <- duels_analysis %>%
  group_by(duo1, duo2) %>%
  summarise(
    total_matches = n(),
    wins = sum(winner == "duo1"),
    win_rate = round(wins / total_matches, 3),
    .groups = "drop"
  ) %>%
  filter(total_matches >= 10)  # 👉 Seuil minimum de 10 duels

# Étape 3 : Extraire les 3 meilleurs et 3 pires matchups pour chaque duo
best_matchups <- matchups %>%
  group_by(duo1) %>%
  arrange(desc(win_rate)) %>%
  slice_head(n = 3) %>%
  mutate(type = "best")

worst_matchups <- matchups %>%
  group_by(duo1) %>%
  arrange(win_rate) %>%
  slice_head(n = 3) %>%
  mutate(type = "worst")

# Étape 4 : Combine les résultats
top_matchups_summary <- bind_rows(best_matchups, worst_matchups)


library(lubridate)

# Tableau des extensions (tu peux l’ajuster si besoin)
extensions <- tribble(
  ~extension_code, ~extension_name, ~release_date,
  "A1",   "Puissance Génétique",          ymd("2024-10-30"),
  "A1a",  "L'Île Fabuleuse",              ymd("2024-12-17"),
  "A2",   "Choc Spatio-Temporel",         ymd("2025-01-30"),
  "A2a",  "Lumière Triomphale",           ymd("2025-02-28"),
  "A2b",  "Réjouissances Rayonnantes",    ymd("2025-03-27"),
  "A3",   "Gardiens Astraux",             ymd("2025-04-30"),
  "A3a",  "Crise Interdimensionnelle",    ymd("2025-05-29")
)

players_with_results <- players_with_results %>%
  mutate(tournament_date = as.Date(tournament_date))

# Création d'un vecteur vide pour stocker les extensions
extensions_vec <- character(nrow(players_with_results))

for (i in seq_len(nrow(players_with_results))) {
  if (i %% 100 == 0 || i == 1) {  # Affiche toutes les 100 lignes + la 1ère
    cat("Traitement ligne", i, "sur", nrow(players_with_results), "\n")
  }
  
  t_date <- players_with_results$tournament_date[i]
  ext_row <- extensions %>%
    filter(release_date <= t_date) %>%
    arrange(desc(release_date)) %>%
    slice_head(n = 1)
  
  if (nrow(ext_row) == 0) {
    extensions_vec[i] <- NA_character_
  } else {
    extensions_vec[i] <- ext_row$extension_name
  }
}

# Ajout de la colonne extension dans le dataframe
players_with_results$extension <- extensions_vec


# Calcul winrate par duo et par extension
winrate_by_duo <- players_with_results %>%
  group_by(extension, top2_pokemon) %>%
  summarise(
    wins = sum(wins, na.rm = TRUE),
    losses = sum(losses, na.rm = TRUE),
    total_matches = wins + losses,
    win_rate = round(wins / total_matches, 3),
    .groups = "drop"
  ) %>%
  filter(total_matches > 0)  # Optionnel : ne garder que les duos qui ont joué


deck_cards <- players_with_results %>%
  select(decklist, wins, losses) %>%
  mutate(total_matches = wins + losses) %>%
  filter(!is.na(decklist)) %>%
  # Séparer chaque carte dans une liste
  mutate(cards_list = str_split(decklist, "\\|")) %>%
  select(-decklist) %>%
  unnest(cards_list) %>%
  mutate(cards_list = str_trim(cards_list))  # enlever espaces

# 2. Extraire le nombre d'exemplaires et le nom de la carte
deck_cards <- deck_cards %>%
  mutate(
    number = as.numeric(str_extract(cards_list, "^\\d+")),
    card_name = str_trim(str_remove(cards_list, "^\\d+x\\s*"))
  ) %>%
  filter(!is.na(number))  # garder uniquement les cartes bien parsées

# 3. Calculer la contribution "jouée * matchs"
deck_cards <- deck_cards %>%
  mutate(
    played_weighted = number * total_matches
  )

# 4. Calculer le winrate pondéré de chaque carte (winrate = total wins / total matches des decks où la carte est jouée)
# Pour cela, on doit aussi récupérer le nombre de wins pondérés

deck_cards <- deck_cards %>%
  mutate(
    wins_weighted = number * wins
  )

# 5. Agréger par carte
cards_summary <- deck_cards %>%
  group_by(card_name) %>%
  summarise(
    times_played_weighted = sum(played_weighted, na.rm = TRUE),
    wins_weighted = sum(wins_weighted, na.rm = TRUE),
    winrate_weighted = wins_weighted / times_played_weighted,
    .groups = "drop"
  )


clean_card_name <- function(name) {
  # Exemple : "Weezing (A1a-50) (Pokémon)" -> "Weezing (A1a-50)"
  # Enlever le dernier groupe entre parenthèses s'il est "Pokémon", "Trainer", etc.
  
  str_trim(str_remove(name, "\\s*\\((Pokémon|Trainer|Supporter|Item|Stadium|Energy)\\)$"))
}

# Nettoyer les noms dans cards
cards_summary <- cards_summary %>%
  mutate(card_name_clean = clean_card_name(card_name))

# Nettoyer les noms extraits des decklists
cards <- cards %>%
  mutate(card_name_clean = clean_card_name(card_name))

cards <- cards %>%
  left_join(cards_summary, by = c("card_name_clean" = "card_name_clean"))

cards <- cards[, !(names(cards) %in% c("card_name_clean", "card_name.y", "times_played_weighted.y", "wins_weighted.y", "winrate_weighted.y"))]

cards <- cards %>%
  rename(
    card_name = card_name.x,
    times_played_weighted = times_played_weighted.x,
    wins_weighted = wins_weighted.x,
    winrate_weighted = winrate_weighted.x,
    times_played_weighted = times_played_weighted.x,
    times_played_weighted = times_played_weighted.x,
  )
cards <- cards[, !(names(cards) %in% c( "Wins", "Winsrate", "Played"))]


cards <- cards %>%
  rename(
    Name = card_name,
    Wins = wins_weighted,
    Winsrate = winrate_weighted,
    Played = times_played_weighted
  )

winrate_by_duo <- winrate_by_duo %>%
  mutate(extension = ifelse(is.na(extension), "Puissance Génétique", extension))

saveRDS(top_matchups_summary, "C:/Users/tdefoulounoux/Desktop/Rshiny/top_matchups_summary.rds")
saveRDS(cards, "C:/Users/tdefoulounoux/Desktop/Rshiny/cards.rds")
saveRDS(winrate_by_duo, "C:/Users/tdefoulounoux/Desktop/Rshiny/winrate_by_duo.rds")
