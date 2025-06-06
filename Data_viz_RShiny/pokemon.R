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

options(scipen = 999)

cards <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/cards.rds")
top_matchups_summary <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/top_matchups_summary.rds")
winrate_by_duo <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/winrate_by_duo.rds")
duels_oucomes <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/duels_oucomes.rds")

tournaments <- readRDS("C:/Users/tdefoulounoux/Desktop/Rshiny/tournaments.rds")

ttcards <- nrow(cards)
ttduels <- nrow(duels_oucomes)
tttournaments <- nrow(tournaments)

  
Match_up <- top_matchups_summary

Match_up <- Match_up %>%
  rename(
    Deck_1 = duo1,
    Deck_2 = duo2
  )

cartes <- cards %>%
  select(Name, Wins, Winsrate, Played, card_type)

decks <- winrate_by_duo

ttdecks <- nrow(decks)

get_image_url <- function(pokemon_name) {
  name <- tolower(pokemon_name)
  name <- gsub("é", "e", name)
  name <- gsub("è", "e", name)
  name <- gsub("ê", "e", name)
  name <- gsub("'", "", name)
  name <- gsub(" ", "-", name)
  paste0("https://img.pokemondb.net/artwork/", name, ".jpg")
}

carte_ex <- data.frame(
  Name = c("Mewtwo EX", "Charizard EX", "Pikachu EX", "Blastoise EX", "Rayquaza EX", "Celebi EX", "Lickilicky EX", "Garchomp EX", "Darkrai EX", "Dialga EX"),
  Img = c(
    "https://img.game8.co/3995561/9ab8b09b4013f8a5eafb5cd08f8faf11.png/original",
    "https://img.game8.co/3995526/ac9d9361799eb484958ffd4873a7c091.png/original",
    "https://img.game8.co/3995580/151d2c9455f83899618147d85881a75e.png/original",
    "https://img.game8.co/4003405/fc6b1543ab68abd28e0d0b93666aa88e.png/original",
    "https://img.game8.co/3998355/f14c91467792bea73eb38ea1b73c394d.png/original",
    "https://img.game8.co/4069626/1a61e1ae8d9edb247a1b5fa0ea976f6c.png/original",
    "https://img.game8.co/4093207/b13399ce9c887b1a82abc8cd313a1532.png/original",
    "https://img.game8.co/4119608/50fe8c3b376c09dbce72d1237d604c0c.png/original",
    "https://img.game8.co/4093185/0bff2b2046d7597c20262dd1ecf7926d.png/original",
    "https://img.game8.co/4088373/a1610910adcf247d45e25b09e6140864.png/original"
  ),
  stringsAsFactors = FALSE
)


ui <- navbarPage(
  title = "Pokémon TCGP",
  header = tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap');

      body {
        font-family: 'Press Start 2P', cursive, Arial, sans-serif;
        margin: 0; padding: 0;
        background: #FFDE00;
      }
      .accueil-panel {
        color: #000000;
        padding: 20px;
        text-align: center;
        overflow: hidden;
      }
      .clickable-img {
        cursor: pointer;
        display: inline-block;
      }
      #slider {
        position: relative;
        width: 100%;
        height: 460px;
        overflow: hidden;
        margin-top: 30px;
        border: none;
      }
      #slider img {
        position: absolute;
        height: 260px;
        border-radius: 10px;
        box-shadow: 0 0 8px #555;
        transition: transform 0.3s ease;
        user-select: none;
      }

      /* Fond blanc pour le tableau Cards */
      #card_table table.dataTable {
        background-color: white !important;
      }
      
      /* Style onglet info (i dans un rond) collé à droite */
      ul.nav.navbar-nav > li:last-child {
        margin-left: auto !important;
        padding-left: 20px;
      }
      ul.nav.navbar-nav > li:last-child > a {
        font-weight: bold;
        font-size: 20px;
        border: 2px solid black;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        line-height: 26px;
        text-align: center;
        padding: 0;
        margin-top: 8px;
      }
    "))
  ),
  tags$script(HTML(sprintf("
    document.addEventListener('DOMContentLoaded', function() {
      const slider = document.getElementById('slider');
      const imagesSrc = %s;
      const images = [];
      const speeds = [];
      const scales = [];
      const containerWidth = slider.clientWidth;
      const containerHeight = slider.clientHeight;
      const imgHeight = 260;
      const imgWidth = 190;
      const spacing = 60;
  
      const imagesDoubled = imagesSrc.concat(imagesSrc);
  
      for(let i=0; i<imagesDoubled.length; i++) {
        const img = document.createElement('img');
        img.src = imagesDoubled[i];
        img.style.left = (i * (imgWidth + spacing)) + 'px';
  
        const scale = 1;
        scales.push(scale);
        img.style.transform = 'scale(' + scale + ')';
  
        const realHeight = imgHeight * scale;
        const topPos = Math.random() * (containerHeight - realHeight);
        img.style.top = topPos + 'px';
  
        slider.appendChild(img);
        images.push(img);
  
        speeds.push(0.8 + Math.random()*1.5);
      }
  
      function animate() {
        for(let i=0; i<images.length; i++) {
          let left = parseFloat(images[i].style.left);
          left -= speeds[i];
          if(left < -imgWidth) {
            const maxLeft = images.reduce((max, img) => Math.max(max, parseFloat(img.style.left)), 0);
            left = maxLeft + imgWidth + spacing;
  
            const scale = 1;
            scales[i] = scale;
            images[i].style.transform = 'scale(' + scale + ')';
  
            const realHeight = imgHeight * scale;
            const newTop = Math.random() * (containerHeight - realHeight);
            images[i].style.top = newTop + 'px';
          }
          images[i].style.left = left + 'px';
        }
        requestAnimationFrame(animate);
      }
      animate();
    });
  ", jsonlite::toJSON(carte_ex$Img))))
  ,
  
  tabPanel("Home",
           div(class = "accueil-panel",
               fluidPage(
                 h2("Welcome to the world of Pokémon cards!"),
                 p("Use the tabs above to explore your cards, create decks, and check your stats."),
                 tags$a(
                   href = "https://tcgpocket.pokemon.com/fr-fr/",
                   target = "_blank",
                   rel = "noopener noreferrer",
                   tags$img(src = "Pokemon_Trading_Card_Game_logo.svg.png", height = "200px", class = "clickable-img")
                 ),
                 div(id = "slider")
               )
           )
  ),
  
  tabPanel("Cards",
           div(class = "cartes-panel",
               fluidPage(
                 h3("Explore Pokémon cards"),
                 selectInput("filter", "Filter by :",
                             choices = c("Tous", "Plus de victoire", "Plus joué", "Moins de victoire", "Moins joué")),
                 selectInput("filter_type", "Filter by card type :", 
                             choices = c("Tous", unique(cartes$card_type)), selected = "Tous"),
                 DTOutput("card_table")
               )
           )
  ),
  
  tabPanel("Decks",
           fluidPage(
             h3("Decks Statistics"),
             selectInput("selected_extension", "Filter by extension :",
                         choices = unique(winrate_by_duo$extension),
                         selected = unique(winrate_by_duo$extension)[1],
                         multiple = TRUE),
             DTOutput("deck_stats_table")
           )
  ),
  tabPanel("Extension statistics",
           fluidPage(
             fluidRow(
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Number of rounds played"),
                        h2(textOutput("total_duels"))
                      )
               ),
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Number of current cards"),
                        h2(textOutput("total_cards"))
                      )
               ),
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Number of tournaments played"),
                        h2(textOutput("total_tournaments"))
                      )
               )
             ),
             br(), br(),
             plotlyOutput("deck_plot", height = "600px"),
             verbatimTextOutput("clicked_point")
           )
  ),
  tabPanel("Deck Statistics",
           fluidPage(
             fluidRow(
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Number of decks"),
                        h2(textOutput("total_decks"))
                      )
               ),
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Last plays"),
                        h2(textOutput("last_extension_plays"))
                      )
               ),
               column(4,
                      div(
                        style = "background-color:#f0f0f0; padding:20px; border-radius:8px; text-align:center;",
                        h3("Last Wins"),
                        h2(textOutput("last_extension_wins"))
                      )
               )
             ),
             
             br(),
             
             selectInput(
               "deck_choice",
               "Choose a deck:",
               choices = unique(decks$top2_pokemon),
               selected = "Mewtwo ex"
             ),
             
             plotOutput("deck_stats_plot2")
           )
  ),
  
  tabPanel("Match up",
           div(class = "stats-panel",
               fluidPage(
                 h3("Recherche Match up"),
                 
                 # Barre de recherche deck (Deck_1)
                 selectInput("deck_matchup_choice",
                             "Choose a deck:",
                             choices = unique(Match_up$Deck_1),
                             selected = "Mewtwo ex",  # si tu veux aussi sélectionner par défaut Mewtwo ex
                             selectize = TRUE),
                 
                 hr(),
                 
                 # Tableau affichant les résultats filtrés
                 DT::dataTableOutput("matchup_table")
               )
           )
  ),
  
  # Onglet spécial 'i' info
  tabPanel(
    title = span("i"),
    value = "info",
    fluidPage(
      h3("Informations importantes"),
      tags$br(),
      tags$br(),
      tags$br(),
      p("Ce projet étudiant, réalisé dans le cadre d’un BUT Science des données (3ᵉ année),"),
      p("dans l'objectif  de la collecte, le traitement, le stockage et la visualisation de données."),
      p("Il s’inscrit dans le cadre de l’enseignement du développement d’un outil décisionnel."),
      tags$br(),
      tags$br(),
      tags$br(),
      p("Ce travail a été mené par Thomas Defoulounoux, Gaël Hellegouarch, Titouan Le Gall et Lukas Le Plaire.")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value pour stocker les decks modifiés (modifiable)
  rv_decks <- reactiveVal(decks)
  
  # Pour garder le deck original intact (non modifiable)
  original_decks <- decks
  
  # Filtrer les decks selon recherche (présence carte dans le deck)
  filtered_deck_names <- reactive({
    all_decks <- names(rv_decks())
    search_term <- tolower(input$deck_search)
    if (is.null(search_term) || search_term == "") {
      return(all_decks)
    }
    Filter(function(deck_name) {
      any(grepl(search_term, tolower(rv_decks()[[deck_name]])))
    }, all_decks)
  })
  
  # Affichage des decks sous forme de liste cliquable (boutons actionLink)
  output$deck_list_ui <- renderUI({
    deck_names <- filtered_deck_names()
    if (length(deck_names) == 0) return(tags$p("No deck found"))
    tagList(
      lapply(deck_names, function(deck_name) {
        actionLink(paste0("select_", deck_name), deck_name, style = "display:block; margin-bottom: 5px;")
      })
    )
  })
  
  # Deck sélectionné
  selected_deck <- reactive({
    input$deck_choice
  })
  
  
  # Titre du deck affiché à droite
  output$deck_title <- renderText({
    deck <- selected_deck()
    if (is.null(deck)) return("No deck selected")
    paste("Deck :", deck)
  })
  
  make_deck_table <- function(deck_vector) {
    if (is.null(deck_vector) || length(deck_vector) == 0) return(NULL)
    
    df_counts <- as.data.frame(table(deck_vector), stringsAsFactors = FALSE)
    names(df_counts) <- c("Name", "Quantité")
    
    # Nettoyage du nom pour l'image : on enlève " (A2-45)", etc.
    df_counts$CleanName <- gsub(" \\(.*\\)", "", df_counts$Name)
    
    # Colonne Image en HTML
    df_counts$Image <- paste0(
      '<img src="', sapply(df_counts$CleanName, get_image_url), 
      '" height="50" style="vertical-align:middle; margin-right:10px;">'
    )
    
    # Réordonner colonnes : Image, Name (complet avec le code), Quantité
    df_counts[, c("Image", "Name", "Quantité")]
  }
  
  # Affichage deck original (non modifiable) à gauche
  output$deck_original <- renderDT({
    deck_name <- selected_deck()
    if (is.null(deck_name)) return(NULL)
    df <- make_deck_table(original_decks[[deck_name]])
    datatable(df,
              selection = "none",
              escape = FALSE,
              rownames = FALSE,
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE
              )
    )
  }, server = FALSE)
  
  # Affichage deck modifiable à droite
  output$deck_cards <- renderDT({
    deck_name <- selected_deck()
    if (is.null(deck_name)) return(NULL)
    deck_mod <- rv_decks()[[deck_name]]
    df <- make_deck_table(deck_mod)
    datatable(df,
              selection = 'single',
              escape = FALSE,
              rownames = FALSE,
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE
              )
    )
  }, server = FALSE)
  
  # Ajouter carte au deck modifié
  observeEvent(input$add_card_btn, {
    deck_name <- selected_deck()
    if (is.null(deck_name)) return()
    card_to_add <- input$add_card
    if (card_to_add == "" || is.null(card_to_add)) return()
    deck_mod <- rv_decks()[[deck_name]]
    deck_mod <- c(deck_mod, card_to_add)
    rv_decks(modifiedList <- {
      tmp <- rv_decks()
      tmp[[deck_name]] <- deck_mod
      tmp
    })
  })
  
  # Supprimer carte sélectionnée dans deck modifié
  observeEvent(input$remove_card_btn, {
    deck_name <- selected_deck()
    if (is.null(deck_name)) return()
    s <- input$deck_cards_rows_selected
    if (length(s) == 0) return()
    deck_mod <- rv_decks()[[deck_name]]
    df <- make_deck_table(deck_mod)
    # Récupérer le nom de la carte sélectionnée
    card_to_remove <- df$Name[s]
    # Retirer une occurrence seulement
    remove_index <- match(card_to_remove, deck_mod)
    if (!is.na(remove_index)) {
      deck_mod <- deck_mod[-remove_index]
      rv_decks(modifiedList <- {
        tmp <- rv_decks()
        tmp[[deck_name]] <- deck_mod
        tmp
      })
    }
  })
  
  output$total_decks <- renderText({
    nrow(decks)  # ttdecks
  })
  
  # Filtrer et afficher table cartes avec images
  filtered_cards <- reactive({
    filter_type <- input$filter
    filter_card_type <- input$filter_type
    df <- cartes
    
    # Filtrer par card_type si différent de "Tous"
    if (!is.null(filter_card_type) && filter_card_type != "Tous") {
      df <- df[df$card_type == filter_card_type, ]
    }
    
    # Trier selon le filtre de tri
    if (is.null(filter_type) || filter_type == "Tous") return(df)
    if (filter_type == "Plus de victoire") {
      df <- df[order(-df$Wins), ]
    } else if (filter_type == "Plus joué") {
      df <- df[order(-df$Played), ]
    } else if (filter_type == "Moins de victoire") {
      df <- df[order(df$Wins), ]
    } else if (filter_type == "Moins joué") {
      df <- df[order(df$Played), ]
    }
    df
  })
  
  output$card_table <- renderDT({
    df <- filtered_cards()
    # Calcul du winrate
    df$Winrate <- round(df$Wins / df$Played * 100, 2)
    # Créer colonne Image HTML (si tu veux l'afficher)
    df$Image <- paste0('<img src="', sapply(df$Name, get_image_url), '" height="50">')
    # Colonnes dans l’ordre : Image, Name, Type, Wins, Played, Winrate
    df <- df[, c("Name", "Wins", "Played", "Winrate")]
    datatable(df,
              escape = FALSE,
              rownames = FALSE,
              options = list(pageLength = 10)
    )
  })
  
  
  
  observeEvent(input$send_email_btn, {
    deck_name <- selected_deck()
    if (is.null(deck_name)) {
      showNotification("Aucun deck sélectionné.", type = "error")
      return()
    }
    
    recipient <- input$recipient_email
    if (is.null(recipient) || recipient == "") {
      showNotification("Veuillez saisir une adresse e-mail.", type = "error")
      return()
    }
    
    deck_mod <- rv_decks()[[deck_name]]
    if (length(deck_mod) == 0) {
      showNotification("Le deck est vide.", type = "error")
      return()
    }
    
    # Créer le contenu du mail
    deck_table <- as.data.frame(table(deck_mod), stringsAsFactors = FALSE)
    colnames(deck_table) <- c("Carte", "Quantité")
    
    deck_body <- paste0(
      "Voici le contenu du deck '", deck_name, "' :\n\n",
      paste(apply(deck_table, 1, function(row) paste0("- ", row[1], " x", row[2])), collapse = "\n")
    )
    
    tryCatch({
      send.mail(
        from = "noreply@la-longue-vue.fr",
        to = recipient,
        subject = paste0("Votre deck Pokémon : ", deck_name),
        body = deck_body,
        smtp = list(
          host.name = "smtp.office365.com",
          port = 587,
          user.name = "noreply@la-longue-vue.fr",
          passwd = "Bay75537",  # 🔒 attention : à sécuriser
          tls = TRUE
        ),
        authenticate = TRUE,
        send = TRUE
      )
      showNotification("E-mail envoyé avec succès !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'envoi :", e$message), type = "error")
    })
  })
  
  # KPI 2 : nombre de Trainer (à adapter si tu as une catégorie "Trainer" dans tes données)
  output$nb_trainer <- renderText({
    # Si tu as un type "Trainer", adapte ici.
    nrow(filtered_usage()[filtered_usage()$Type == "Trainer", ])
  })
  
  # Carte la plus utilisée
  output$most_used_card <- renderText({
    tab <- table(filtered_usage()$Name)
    if (length(tab) == 0) return("Aucune carte")
    names(sort(tab, decreasing = TRUE))[1]
  })
  
  # Graphique de tendance d'une carte sélectionnée
  output$card_usage_plot <- renderPlot({
    df <- filtered_usage()
    req(input$selected_card)
    
    # Ajouter colonne "Mois" pour regrouper par mois
    df$Month <- format(as.Date(df$Date), "%Y-%m")
    
    # Sous-ensemble pour la carte sélectionnée
    card_data <- df[df$Name == input$selected_card, ]
    
    # Total parties par mois (toutes cartes confondues)
    total_monthly <- as.data.frame(table(df$Month))
    names(total_monthly) <- c("Month", "TotalGames")
    
    # Parties jouées et gagnées par mois pour la carte sélectionnée
    card_monthly <- aggregate(is_win ~ Month, data = card_data, function(x) c(Used = length(x), Wins = sum(x)))
    card_monthly <- do.call(data.frame, card_monthly)
    names(card_monthly) <- c("Month", "Used", "Wins")
    
    # Fusion des deux datasets
    merged <- merge(total_monthly, card_monthly, by = "Month", all.x = TRUE)
    merged[is.na(merged)] <- 0
    
    # Calcul des pourcentages
    merged$UsageRate <- 100 * merged$Used / merged$TotalGames
    merged$WinRate <- ifelse(merged$Used > 0, 100 * merged$Wins / merged$Used, NA)
    
    # Pour l'ordre chronologique des mois
    merged$Month <- as.Date(paste0(merged$Month, "-01"))
    
    # Affichage avec ggplot
    library(ggplot2)
    ggplot(merged, aes(x = Month)) +
      geom_line(aes(y = UsageRate, color = "Taux d'utilisation (%)"), size = 1.2) +
      geom_line(aes(y = WinRate, color = "Taux de victoire (%)"), size = 1.2, linetype = "dashed") +
      labs(
        y = "Pourcentage (%)",
        x = "Mois",
        title = paste("Statistiques mensuelles -", input$selected_card)
      ) +
      scale_color_manual(name = "Indicateur", values = c("Taux d'utilisation (%)" = "blue", "Taux de victoire (%)" = "green")) +
      theme_minimal()
  })
  
  deck_stats <- reactive({
    df <- filtered_usage()
    
    # Calculer pour chaque deck
    results <- lapply(names(decks), function(deck_name) {
      cards_in_deck <- decks[[deck_name]]
      
      # Sous-ensemble de parties jouées avec au moins une carte du deck
      subset_df <- df[df$Name %in% cards_in_deck, ]
      
      used <- nrow(subset_df)
      wins <- sum(subset_df$is_win)
      winrate <- ifelse(used > 0, wins / used * 100, NA)
      
      data.frame(
        Deck = deck_name,
        Used = used,
        Winrate = winrate
      )
    })
    
    res_df <- do.call(rbind, results)
    res_df <- res_df[order(-res_df$Used), ]  # Trier par utilisation décroissante
    
    # Ne garder que les decks avec utilisation > 0
    res_df <- res_df[res_df$Used > 0, ]
    res_df
  })
  ttcards <- reactive({ nrow(cards) })
  ttduels <- reactive({ nrow(duels_oucomes) })
  tttournaments <- reactive({ nrow(tournaments) })
  
  output$total_cards <- renderText({
    format(ttcards(), big.mark = ",")
  })
  
  output$total_duels <- renderText({
    format(ttduels(), big.mark = ",")
  })
  
  output$total_tournaments <- renderText({
    format(tttournaments(), big.mark = ",")
  })
  
  output$deck_stats_plot <- renderPlotly({
    req(selected_deck())  # S'assurer qu'un deck est sélectionné
    
    deck_name <- selected_deck()
    deck_cards <- rv_decks()[[deck_name]]
    
    stats <- aggregate(is_win ~ Name, data = filtered_usage, FUN = function(x) {
      c(wins = sum(x), plays = length(x))
    })
    
    stats_df <- do.call(data.frame, stats)
    stats_df$WinRate <- round(100 * stats_df$is_win.wins / stats_df$is_win.plays, 1)
    
    plot_ly(
      data = stats_df,
      x = ~Name,
      y = ~WinRate,
      type = 'bar',
      text = ~paste0("Win Rate: ", WinRate, "%"),
      hoverinfo = "text",
      marker = list(color = 'rgba(255, 99, 71, 0.7)')
    ) %>%
      layout(
        title = paste("Statistiques du deck :", deck_name),
        yaxis = list(title = "Taux de victoire (%)", range = c(0, 100)),
        xaxis = list(title = "Carte"),
        margin = list(t = 50)
      )
  })
  # Table filtrée selon les extensions choisies
  filtered_decks <- reactive({
    req(input$selected_extension)
    subset(winrate_by_duo, extension %in% input$selected_extension)
  })
  
  # Affichage du tableau
  output$deck_stats_table <- renderDT({
    req(input$selected_extension)
    
    # Filtrage et tri
    filtered <- winrate_by_duo %>%
      filter(extension %in% input$selected_extension) %>%
      arrange(desc(total_matches)) %>%
      select(-extension)  # On retire la colonne extension ici
    
    datatable(filtered,
              options = list(
                pageLength = 10
              ),
              rownames = FALSE)
  })
  
  filtered_matchup <- reactive({
    req(input$deck_matchup_choice)
    Match_up[Match_up$Deck_1 == input$deck_matchup_choice, ]
  })
  
  output$matchup_table <- DT::renderDataTable({
    filtered_matchup()
  }, options = list(
    pageLength = 9,
    dom = 'ip'  # i = info, p = pagination, sans search ni length menu
  ))
  
  extension_dates <- c(
    "Puissance Génétique" = as.Date("2024-10-30"),
    "L'Île Fabuleuse" = as.Date("2024-12-17"),
    "Choc Spatio-Temporel" = as.Date("2025-01-30"),
    "Lumière Triomphale" = as.Date("2025-02-28"),
    "Réjouissances Rayonnantes" = as.Date("2025-03-27"),
    "Gardiens Astraux" = as.Date("2025-04-30"),
    "Crise Interdimensionnelle" = as.Date("2025-05-29")
  )
  
  # Données initiales triées, top 40 decks
  df_top <- reactive({
    decks %>%
      slice_max(order_by = total_matches, n = 100, with_ties = FALSE) %>%
      mutate(extension_date = extension_dates[extension]) %>%
      arrange(extension_date)
  })
  
  # Graphique des bulles initial
  output$deck_plot <- renderPlotly({
    df <- df_top()
    
    plot_ly(
      data = df,
      x = ~factor(extension, levels = unique(extension)),
      y = ~win_rate,
      type = 'scatter',
      mode = 'markers',
      text = ~paste("Deck:", top2_pokemon,
                    "<br>Winrate:", scales::percent(win_rate),
                    "<br>Total Matches:", total_matches),
      marker = list(
        size = ~total_matches / 2,
        sizemode = 'area',
        sizeref = 2.0 * max(df$total_matches / 2) / (40^2),
        sizemin = 5,
        color = ~win_rate,
        colorscale = 'Viridis',
        showscale = TRUE
      )
    ) %>%
      layout(
        title = "Répartition des decks par Extension",
        xaxis = list(title = "Extension", categoryorder = "array", categoryarray = unique(df$extension)),
        yaxis = list(title = "Winrate"),
        hovermode = "closest"
      )
  })
  
  # Données du deck sélectionné par clic
  selected_deck <- reactive({
    click_data <- event_data("plotly_click", source = "deck_plot")
    if (is.null(click_data)) return(NULL)
    
    # click_data contient les coordonnées du point cliqué
    # On récupère le deck par exemple avec le texte ou x/y
    # Ici on récupère le deck par matching extension + win_rate + total_matches potentiellement
    
    # Pour simplifier, on peut utiliser le point x et y, puis chercher le deck dans df_top()
    df <- df_top()
    
    # L'extension cliquée (x) :
    ext_clicked <- click_data$x
    
    # Win_rate cliqué (y) :
    wr_clicked <- click_data$y
    
    # Filtrage des decks ayant extension = ext_clicked, win_rate proche de wr_clicked
    deck_clicked <- df %>%
      filter(extension == ext_clicked) %>%
      filter(abs(win_rate - wr_clicked) < 0.001) # seuil pour éviter plusieurs matches
    
    if (nrow(deck_clicked) == 0) return(NULL)
    
    deck_clicked$top2_pokemon[1]
  })
  
  output$deck_stats_plot2 <- renderPlot({
    req(input$deck_choice)
    selected_decks <- as.character(input$deck_choice)
    
    # Total matches par extension, tous decks confondus
    total_matches_per_extension <- decks %>%
      group_by(extension) %>%
      summarise(total_matches_extension = sum(total_matches, na.rm = TRUE)) 
    
    # Préparation des données pour les decks sélectionnés
    plot_data <- decks %>%
      filter(top2_pokemon %in% selected_decks) %>%
      group_by(extension, top2_pokemon) %>%
      summarise(
        mean_winrate = mean(win_rate, na.rm = TRUE),
        total_matches = sum(total_matches, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # Joindre le total matches par extension pour calculer le taux d'utilisation
      left_join(total_matches_per_extension, by = "extension") %>%
      mutate(
        extension = factor(extension, levels = names(sort(extension_dates))),
        usage_rate = total_matches / total_matches_extension
      )
    
    print(plot_data)  # debug
    
    ggplot(plot_data, aes(x = extension, group = top2_pokemon)) +
      # Courbe du winrate (axe y gauche)
      geom_line(aes(y = mean_winrate, color = top2_pokemon), size = 1.2) +
      geom_point(aes(y = mean_winrate, color = top2_pokemon), size = 3) +
      scale_y_continuous(
        name = "Winrate moyen",
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1),
        sec.axis = sec_axis(~ ., name = "Taux d'utilisation")  # axe secondaire
      ) +
      # Courbe du taux d'utilisation (axe y droite)
      geom_line(aes(y = usage_rate, color = "Utilisation"), size = 1.2, linetype = "dashed") +
      geom_point(aes(y = usage_rate, color = "Utilisation"), size = 3) +
      scale_color_manual(
        name = "Légende",
        values = c("Utilisation" = "blue") %>% c(setNames(RColorBrewer::brewer.pal(8, "Set1"), unique(plot_data$top2_pokemon)))
      ) +
      labs(
        title = "Winrate et taux d'utilisation du deck par extension",
        x = "Extension"
      ) +
      theme_minimal()
  })
  
  output$last_extension_plays <- renderText({
    req(input$deck_choice)
    selected <- input$deck_choice
    
    last_ext <- decks %>%
      filter(top2_pokemon == selected) %>%
      mutate(extension = factor(extension, levels = names(sort(extension_dates)))) %>%
      filter(!is.na(extension)) %>%
      slice_max(order_by = as.integer(extension), n = 1, with_ties = FALSE) %>%
      pull(extension)
    
    value <- decks %>%
      filter(top2_pokemon == selected, extension == last_ext) %>%
      summarise(total = sum(total_matches, na.rm = TRUE)) %>%
      pull(total)
    
    if (length(value) == 0 || is.na(value)) {
      return("0")
    } else {
      return(format(value, big.mark = ","))
    }
  })
  
  output$last_extension_wins <- renderText({
    req(input$deck_choice)
    selected <- input$deck_choice
    
    last_ext <- decks %>%
      filter(top2_pokemon == selected) %>%
      mutate(extension = factor(extension, levels = names(sort(extension_dates)))) %>%
      filter(!is.na(extension)) %>%
      slice_max(order_by = as.integer(extension), n = 1, with_ties = FALSE) %>%
      pull(extension)
    
    value <- decks %>%
      filter(top2_pokemon == selected, extension == last_ext) %>%
      summarise(total_wins = sum(wins, na.rm = TRUE)) %>%
      pull(total_wins)
    
    if (length(value) == 0 || is.na(value)) {
      return("0")
    } else {
      return(format(value, big.mark = ","))
    }
  })
    
}


shinyApp(ui, server, options = list(launch.browser = TRUE))
