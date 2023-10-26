# Charger les bibliothèques nécessaires
library(shiny)
library(DT)
library(httr)
library(jsonlite)
library(shinydashboard)
library(plotly)
library(leaflet)
library(geosphere)
library(dplyr)
library(tidygeocoder)

# Charger les données à partir du fichier CSV
file_path <- "L:\\BUT\\SD\\Promo 2022\\rpicard\\SD2\\data_postcode.csv" # Modifier avec vos fichiers loco
data_csv <- read.csv(file_path, stringsAsFactors = FALSE)

# Charger les données à partir de l'API JCDecaux
data_api <- data.frame(
  fromJSON(
    rawToChar(
      (GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=d7df6e5b67b83f57bf7033708d4f380e882246ce"))$content
    ),
    flatten = TRUE
  )
)

# Fonction pour récupérer les données
loadData <- function() {
  return(data_api)
}

# Définir l'interface utilisateur avec un thème
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de bord Velov"),
  dashboardSidebar(
    sidebarMenu(
      # Onglets du menu latéral
      menuItem("Tableau de données", tabName = "data_table", icon = icon("table")),
      menuItem("Statistiques", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Connexion", tabName = "login", icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #dd4b39;
          color: #ffffff;
        }
        .skin-blue .main-sidebar {
          background-color: #222d32;
        }
        .skin-blue .main-sidebar a {
          color: #b8c7ce;
        }
        .skin-blue .content-wrapper, .skin-blue .right-side {
          background: #f9fafc;
        }
      '))
    ),
    tags$style(HTML("
      .box {
        min-height: 900px;
        width: 550px;
      }
      .selectize-input {
        width: 150px;
      }
      .dataTables_length {
        display: none;
      }
    ")),   tabItems(
      tabItem(
        tabName = "data_table",
        fluidPage(
          titlePanel("Explorateur de données Velov"),
          sidebarLayout(
            sidebarPanel(
              selectInput("postcode_filter",
                          "Filtrer par code postal:",
                          c("Tous", unique(data_csv$postcode)), width = "150px"),
              actionButton("refreshButton", "Actualiser les données")  # Ajouter un bouton d'actualisation
            ),
            mainPanel(
              DTOutput("table")
            )
          )
        )
      ),
      tabItem(
        tabName = "statistics",
        fluidPage(
          titlePanel("Statistiques Velov"),
          fluidRow(
            column(4,
                   box(
                     title = "Répartition des emplacements pour les vélos",
                     plotOutput("bike_stands_histogram", width = "100%", height = "700px")
                   )
            ),
            column(4,
                   box(
                     title = "Répartition des vélos disponibles",
                     plotOutput("available_bikes_histogram", width = "100%", height = "700px")
                   )
            ),
            column(4,
                   box(
                     title = "Proportion des vélos disponibles",
                     plotOutput("proportion_chart", width = "100%", height = "700px")
                   )
            )
          )
        )
      ),
      # tabItem pour l'onglet "Carte"
      tabItem(
        tabName = "map",
        fluidPage(
          titlePanel("Carte Velov"),
          sidebarLayout(
            sidebarPanel(
              textInput('loc', 'Votre adresse', value = ""),
              actionButton('go', 'Géocoder')
            ),
            mainPanel(
              leafletOutput('map', height = "800px")  # Ajuster la taille de la carte ici
            )
          )
        )
      ),
      tabItem(
        tabName = "login",
        fluidPage(
          titlePanel("Inscription et Connexion"),
          sidebarLayout(
            sidebarPanel(
              textInput("new_username", "Nom d'utilisateur"),
              passwordInput("new_password", "Mot de passe"),
              actionButton("registerButton", "S'inscrire"),
              br(),
              textInput("username", "Nom d'utilisateur"),
              passwordInput("password", "Mot de passe"),
              actionButton("loginButton", "Se Connecter"),
              br(),
              conditionalPanel(
                condition = "output.logoutButton",
                actionButton("logoutButton", "Se Déconnecter")
              )
            ),
            mainPanel(
              textOutput("message"),
              conditionalPanel(
                condition = "output.logoutButton",
                textOutput("loggedUser")
              )
            )
          )
        )
      )
    )
  )
)

# Définition du serveur
server <- function(input, output, session) {
  data_api <- reactiveVal(loadData())  # Utiliser reactiveVal pour stocker et mettre à jour les données de l'API
  data_csv <- data_csv
  
  # Filtrer les données de l'API en fonction du code postal
  filtered_data <- reactive({
    data_filtered <- data_api()
    if (!is.null(input$postcode_filter) && input$postcode_filter != "Tous") {
      number_to_filter <- data_csv$number[data_csv$postcode == input$postcode_filter]
      data_filtered <- data_filtered[data_filtered$number %in% number_to_filter, ]
    }
    data_filtered
  })
  
  output$table <- DT::renderDataTable({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, dom = 'tip'),  # Afficher 10 lignes par page et améliorer la mise en page
      class = 'display'
    )
  })
  # Gérer le clic sur le bouton pour actualiser les données
  observeEvent(input$refreshButton, {
    data(loadData())  # Recharger et mettre à jour les données
    updateSelectInput(session, "postcode_filter", selected = "Tous")  # Réinitialiser le filtre
  })
  
  output$bike_stands_histogram <- renderPlot({
    req(filtered_data())
    ggplot(data = filtered_data(), aes(x = `bike_stands`)) +
      geom_histogram(binwidth = 5, fill = "#0072B2", color = "black") +
      labs(title = "Répartition des emplacements pour les vélos",
           x = "Nombre d'emplacements",
           y = "Fréquence")
  })
  
  output$available_bikes_histogram <- renderPlot({
    req(filtered_data())
    ggplot(data = filtered_data(), aes(x = `available_bikes`)) +
      geom_histogram(binwidth = 5, fill = "#009E73", color = "black") +
      labs(title = "Répartition des vélos disponibles",
           x = "Nombre de vélos disponibles",
           y = "Fréquence")
  })
  
  output$proportion_chart <- renderPlot({
    req(filtered_data())
    total_bikes <- sum(filtered_data()$`bike_stands`)
    available_bikes <- sum(filtered_data()$`available_bikes`)
    unavailable_bikes <- total_bikes - available_bikes
    
    data <- data.frame(
      Catégorie = c("Vélos disponibles", "Vélos non disponibles"),
      Compte = c(available_bikes, unavailable_bikes)
    )
    
    pie(data$Compte, labels = data$Catégorie, col = c("#009E73", "#D55E00"), main = "Proportion des vélos disponibles")
  })
  
  observeEvent(input$go, {
    req(filtered_data())
    loc_saisie <- input$loc
    if (loc_saisie != "") {
      addres <- tibble::tribble(~addr, loc_saisie)
      lat_longs <- addres %>%
        geocode(addr, method = 'osm', lat = latitude, long = longitude)
      
      distances <- geosphere::distVincentySphere(
        cbind(filtered_data()$position.lng, filtered_data()$position.lat),
        cbind(lat_longs$longitude, lat_longs$latitude)
      )
      
      closest_indices <- order(distances)[1:3]
      
      closest_data <- filtered_data()[closest_indices, ]
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(
            data = closest_data,
            lat = ~position.lat,
            lng = ~position.lng,
            popup = ~paste(name, status,
                           "<br>Nombre de vélos disponibles:", available_bikes,
                           "<br>Nombre de places de stationnement disponibles:", bike_stands)
          )
      })
    }
  })
  
  observeEvent(input$registerButton, {
    req(input$new_username, input$new_password)
    new_user <- isolate(input$new_username)
    new_password <- isolate(input$new_password)
    
    data_users <- user_data()
    if (new_user %in% data_users$username) {
      output$message <- renderText("Nom d'utilisateur déjà pris.")
    } else {
      data_users <- rbind(data_users, data.frame(username = new_user, password = new_password, stringsAsFactors = FALSE))
      user_data(data_users)
      output$message <- renderText("Inscription réussie.")
    }
  })
  
  observeEvent(input$loginButton, {
    req(input$username, input$password)
    username <- isolate(input$username)
    password <- isolate(input$password)
    
    data_users <- user_data()
    if (username %in% data_users$username && password == data_users$password[data_users$username == username]) {
      isLoggedIn(TRUE)
      output$message <- renderText("Connexion réussie.")
    } else {
      output$message <- renderText("Nom d'utilisateur ou mot de passe incorrect.")
    }
  })
  
  observeEvent(input$logoutButton, {
    isLoggedIn(FALSE)
    output$message <- renderText("Déconnexion réussie.")
  })
  
  output.logoutButton <- renderUI({
    if (isLoggedIn()) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  output$loggedUser <- renderText({
    paste("Connecté en tant que:", input$username)
  })
}

shinyApp(ui, server)
