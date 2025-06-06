-- public.players definition
DROP TABLE IF EXISTS public.wrk_players;
CREATE TABLE public.wrk_players (
  tournament_id varchar NULL,
  tournament_name varchar NULL,
  tournament_date timestamp NULL,
  organizer varchar NULL,
  format varchar NULL,
  nb_players int NULL,
  player_id varchar NULL,
  player_name varchar NULL,
  "placing" int NULL,
  country varchar NULL,
  score_total int NULL,
  decklist varchar NULL
);

DROP TABLE IF EXISTS public.wrk_duels;
CREATE TABLE public.wrk_duels (
  tournament_id varchar NULL,
  match_number int NULL,
  player1_id varchar NULL,
  player1_score int NULL,
  player2_id varchar NULL,
  player2_score int NULL
);

DROP TABLE IF EXISTS public.wrk_matchs;
CREATE TABLE public.wrk_matchs (
  tournament_id varchar NULL,
  match_number int NULL,
  player_id varchar NULL,
  match_score int NULL
);