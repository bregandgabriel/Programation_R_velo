library(shiny)
library(httr)
library(jsonlite)
library(RMySQL)

ui <- fluidPage(
  titlePanel("Exemple de Rafraîchissement des Données"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refreshButton", "Rafraîchir les Données")
    ),
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output, session) {
  # Connexion à SQL
  database_host  <- "sql8.freesqldatabase.com"
  database_name  <- "sql8646678"
  database_username  <- "sql8646678"
  database_pasword  <- "sncHTNH9Tb"
  
  con <- dbConnect(MySQL(),
                   user = database_username,
                   password = database_pasword,
                   host = database_host,
                   dbname = database_name)
  
  # Données initiales de la base sql
  data  <- dbGetQuery(con, "SELECT * FROM Velib_Lyon_Info")
  
  # Créer une variable réactive pour les données
  updatedData <- reactiveVal(data)
  
  # Rafraîchir les données lorsque le bouton est cliqué
  observeEvent(input$refreshButton, {
    # Recupere des nouvel info avec l'API
    new_data <- data.frame(
      fromJSON(
        rawToChar(
          (GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=d7df6e5b67b83f57bf7033708d4f380e882246ce"))$content)
        , flatten = TRUE)
    )
    # Supp les anscienne données
    dbGetQuery(con, "DROP TABLE Velib_Lyon_Info")
    # Update BDD
    dbWriteTable(con,"Velib_Lyon_Info",new_data)
    updatedData(new_data)  # Mettre à jour les données réactives
  })
  
  output$dataTable <- renderTable({
    updatedData()  # Afficher les données réactives
  })
}

shinyApp(ui, server)