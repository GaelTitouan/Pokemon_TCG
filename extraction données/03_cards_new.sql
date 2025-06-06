-- public.players definition
DROP TABLE IF EXISTS public.cards_new;
CREATE TABLE public.cards_new (
  id varchar NULL,
  card_name varchar NULL,
  card_type varchar NULL,
  card_subtype varchar NULL,
  card_element varchar NULL,
  health int NULL,
  evolvesFrom varchar NULL,
  rarity varchar NULL
)