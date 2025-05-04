library(shiny)
library(shinythemes)
library(FNN)
library(stringdist)
library(httr)
library(base64enc)

setwd("C:\\Users\\dtafm\\OneDrive\\Desktop\\data.science\\R\\Work.R\\lis4805\\final_proj_shinyapp")

# Load data
clust1 <- read.csv("data/cluster1.csv")
clust2 <- read.csv("data/cluster2.csv")

songs1 <- read.csv("data/song_name_cluster1.csv")
songs2 <- read.csv("data/song_name_cluster2.csv")

# Creates lookup table
lookup <- data.frame(
  song_name = c(songs1$full_name, songs2$full_name),
  cluster_id = c(rep(1, nrow(songs1)), rep(2, nrow(songs2))),
  stringsAsFactors = FALSE
)

# Fuzzy matching function
find_closest_song <- function(input_song_name, lookup_table) {
  distances <- stringdist::stringdist(tolower(input_song_name), tolower(lookup_table$song_name), method = "jw")
  sorted_indices <- order(distances)
  top_indices <- sorted_indices[1:6]
  top_distances <- distances[top_indices]
  top_names <- lookup_table$song_name[top_indices]
  top_clusters <- lookup_table$cluster_id[top_indices]
  
  return(data.frame(song_name = top_names, distance = top_distances, cluster_id = top_clusters))
}

# Function to get a new access token
get_spotify_access_token <- function(client_id, client_secret) {
  credentials <- paste(client_id, client_secret, sep = ":")
  encoded_credentials <- base64encode(charToRaw(credentials))
  
  response <- POST(
    url = "https://accounts.spotify.com/api/token",
    add_headers(Authorization = paste("Basic", encoded_credentials)),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  
  token_info <- content(response)
  return(token_info$access_token)
}

# UI
ui <- fluidPage(
  theme = shinytheme("darkly"),  # Dark theme
  
  tags$style(HTML("
    .centered-input {
      width: 100%;
      display: flex;
      justify-content: center;
    }
    .centered-input input {
      text-align: center;
      width: 100%;
    }
  ")),
  
  tags$div(
    style = "text-align: center; margin-top: 30px;",
    tags$h1(
      "Song Finder",
      style = "color: white;"
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      align = "center",
      div(
        style = "width: 50%; margin: auto;",
        div(
          class = "centered-input",
          textInput("song_name", 
                    label = "Enter Song Name: ", 
                    placeholder = "Song Name - Artist")
        ),
        actionButton("find_btn", "Find Song"),
        br(), br(),
        uiOutput("advanced_ui")
      ),
      br(),
      div(
        style = "width: 50%; margin: auto;",
        textOutput("current_song_name"), 
        tableOutput("neighbors")
      ),
      div(
        style = "width: 50%; margin: auto; margin-top: 20px;",
        uiOutput("alternative_songs_ui")
      ),
      br(), br(),
      div(
        style = "width: 100%; text-align: center; margin-top: 40px; color: grey;",
        tags$h5("Database is not exhaustive due to new Spotify API restrictions. Limited songs after 2022.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  token_reactive <- reactiveVal()
  top_matches <- reactiveVal()
  best_match <- reactiveVal()
  selection_made <- reactiveVal(FALSE)
  knn_done <- reactiveVal(FALSE)
  
  #fill with api keys
  client_id <- ''
  client_secret <- ''
  
  access_token <- get_spotify_access_token(client_id, client_secret)
  token_reactive(access_token)
  
  # Spotify link search function
  get_spotify_link <- function(song_title_artist) {
    query <- URLencode(song_title_artist)
    url <- paste0("https://api.spotify.com/v1/search?q=", query, "&type=track&limit=1&market=US")
    
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", token_reactive()))
    )
    
    result <- content(response)
    
    if (!is.null(result$tracks$items) && length(result$tracks$items) > 0) {
      return(result$tracks$items[[1]]$external_urls$spotify)
    } else {
      return(NA)
    }
  }
  
  #function to run knn
  perform_knn <- function(selected_match) {
    matched_song_name <- selected_match$song_name
    query_cluster_id <- selected_match$cluster_id
    
    if (query_cluster_id == 1) {
      cluster_data <- clust1
      cluster_song_names <- songs1
      relative_query_index <- which(songs1$full_name == matched_song_name)
    } else {
      cluster_data <- clust2
      cluster_song_names <- songs2
      relative_query_index <- which(songs2$full_name == matched_song_name)
    }
    
    knn_result <- get.knnx(
      data = cluster_data,
      query = cluster_data[relative_query_index, , drop = FALSE],
      k = 6
    )
    
    neighbor_indices <- knn_result$nn.index[1, 2:6]
    neighbor_distances <- knn_result$nn.dist[1, 2:6]
    neighbors_data <- cluster_song_names[neighbor_indices, ]
    
    return(list(
      indices = neighbor_indices,
      distances = neighbor_distances,
      song_names = neighbors_data$full_name
    ))
  }
  # runs the knn function when find button is clicked
  observeEvent(input$find_btn, {
    input_song_name <- input$song_name
    matches <- find_closest_song(input_song_name, lookup)
    
    top_matches(matches)
    best_match(matches[1, ])
    
    selection_made(FALSE)
    knn_done(FALSE)
    
    output$current_song_name <- renderText({
      paste("Showing Recommendations for:", best_match()$song_name)
    })
    
    output$alternative_songs_ui <- renderUI({
      selectInput(
        "alternative_song", 
        "Not the right song? Check Below:", 
        choices = c("Alternative Matches" = "", matches$song_name[2:6]),
        selected = NULL
      )
    })
    
    knn_results <- perform_knn(best_match())
    knn_done(TRUE)
    
    spotify_links <- sapply(knn_results$song_names, get_spotify_link)
    
    output$neighbors <- renderTable({
      data.frame(
        Order = 1:5,
        Song_Name = knn_results$song_names,
        URLs = sprintf('<a href="%s" target="_blank">Link</a>', spotify_links),
        stringsAsFactors = FALSE
      )
    }, sanitize.text.function = function(x) x)
  })
  #renders button after a song has been queried
  output$advanced_ui <- renderUI({
    if (knn_done()) {
      actionButton("show_advanced_btn", "Show Advanced KNN Results")
    }
  })
  #re-runs if sogn from dropdown is chosen
  observeEvent(input$alternative_song, {
    if (!selection_made()) {
      selection_made(TRUE)
      return()
    }
    
    req(input$alternative_song)
    selected_song <- input$alternative_song
    selected_match <- top_matches()[top_matches()$song_name == selected_song, ]
    
    best_match(selected_match)
    
    output$current_song_name <- renderText({
      paste("Showing Recommendations for:", best_match()$song_name)
    })
    
    knn_results <- perform_knn(selected_match)
    knn_done(TRUE)
    
    spotify_links <- sapply(knn_results$song_names, get_spotify_link)
    
    output$neighbors <- renderTable({
      data.frame(
        Order = 1:5,
        Song_Name = knn_results$song_names,
        URLs = sprintf('<a href="%s" target="_blank">Link</a>', spotify_links),
        stringsAsFactors = FALSE
      )
    }, sanitize.text.function = function(x) x)
  })
  
  # re-run knnx and observe distance matrix. Fix this...
  observeEvent(input$show_advanced_btn, {
    selected_match <- best_match()
    
    knn_results <- perform_knn(selected_match)
    
    spotify_links <- sapply(knn_results$song_names, get_spotify_link)
    
    output$neighbors <- renderTable({
      data.frame(
        Neighbor_Index = knn_results$indices,
        Distance = knn_results$distances,
        Neighbor_Song = knn_results$song_names,
        URLs = sprintf('<a href="%s" target="_blank">Link</a>', spotify_links),
        stringsAsFactors = FALSE
      )
    }, sanitize.text.function = function(x) x)
  })
  
} 
# Run the app
shinyApp(ui = ui, server = server)
