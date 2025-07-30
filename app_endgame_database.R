library(arrow)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(stringr)

dat <- open_dataset("data")

ui <- fluidPage(

  # Give the page a title
  titlePanel("Tablebase evaluations and actual results of Lichess games"),
  
  tags$head(
      tags$style(HTML("
      .piece-symbol { font-size: 20px; margin-right: 10px; }
      .total-count { font-weight: bold; margin-top: 10px; }
      .error { color: red; }
      .success { color: green; }
      
    .white-pieces .control-label {
      color: white;
      text-shadow: 1px 1px 1px black, -1px -1px 1px black, 1px -1px 1px black, -1px 1px 1px black;
      font-weight: normal;
    }
    
    .black-pieces .control-label {
      color: #333;
      font-weight: normal;
      text-shadow: 1px 1px 1px black, -1px -1px 1px black, 1px -1px 1px black, -1px 1px 1px black;
    }
    "))
  ),

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(
      checkboxGroupButtons(
            inputId = "time_control",
            label = "Time control",
            choices = c(
                "UltraBullet", "Bullet", "Blitz", "Rapid", "Classical"
            ),
            selected = c(
                "UltraBullet", "Bullet", "Blitz", "Rapid", "Classical"
            )),
      sliderInput(
        "rating",
        label = "Rating range",
        min = 400,
        max = 3300,
        value = c(400, 3300),
        step = 100
      ),
      
      fluidRow(
          # White pieces column
          column(6,
                 div(class = "white-pieces",
                     numericInput(
                         "white_queen", "♕ Queen", value = 0, min = 0, max = 3, width = "100%"
                    ),
                    numericInput(
                        "white_rook", "♖ Rook", value = 0, min = 0, max = 3, width = "100%"
                    ),
                    numericInput(
                        "white_bishop", "♗ Bishop", value = 0, min = 0, max = 3, width = "100%"
                    ),
                    numericInput(
                        "white_knight", "♘ Knight", value = 0, min = 0, max = 3, width = "100%"
                    ),
                    numericInput(
                        "white_pawn", "♙ Pawn", value = 0, min = 0, max = 3, width = "100%"
                    )
                 )
          ),
          
          # Black pieces column  
          column(6,
                 div(class = "black-pieces",
                     numericInput(
                         "black_queen", "♕ Queen", value = 0, min = 0, max = 3, width = "100%"
                     ),
                     numericInput(
                         "black_rook", "♖ Rook", value = 0, min = 0, max = 3, width = "100%"
                     ),
                     numericInput(
                         "black_bishop", "♗ Bishop", value = 0, min = 0, max = 3, width = "100%"
                     ),
                     numericInput(
                         "black_knight", "♘ Knight", value = 0, min = 0, max = 3, width = "100%"
                     ),
                     numericInput(
                         "black_pawn", "♙ Pawn", value = 0, min = 0, max = 3, width = "100%"
                     )
                 )
          )
      ),
      
      searchInput(
          inputId = "fen_search",
          label = "Search position",
          placeholder = "Enter FEN",
          btnSearch = icon("search"),
          btnReset = icon("remove")
      )
    ),

    # Create a spot for the barplot
    mainPanel(
        withSpinner(
            plotlyOutput("plot", width = "100%", height = "80vh")
        )
    )
  )
)

server <- function(input, output) {
  # keep only the time control and rating ranges chosen
    
  filtered_dat <- reactive({
    dat <- dat %>%
      filter(time_control %in% input$time_control) %>%
      filter(avg_elo >= input$rating[1], avg_elo <= input$rating[2])
    
    # store total games and observations
    
    total_positions <<- nrow(dat)
    
    total_games <<- dat %>%
        summarise(total_games = n_distinct(id)) %>%
        collect() %>%
        pull(total_games)

    # filter specific fen
    if (input$fen_search != "") {
        
        stripped_position <- input$fen_search %>%
            stringr::str_extract(".*? [wb]")
        
        dat <- dat %>%
            filter(
                stringr::str_detect(fens, stripped_position)
            )
    }
    
    # count material
    total_pieces <- input$white_queen + input$white_rook + input$white_knight +
        input$white_bishop + input$black_queen + input$white_pawn + input$black_pawn +
        input$black_rook + input$black_knight + input$black_bishop
    
    # Validate piece count
    shiny::validate(
        need(total_pieces <= 3, "Too many pieces selected. Maximum is 3 pieces.")
    )
    
    # impose material
    if (total_pieces > 0) {
        
        dat <- dat %>%
            filter(material_count == total_pieces) %>%
            filter(
                white_queen == input$white_queen, 
                black_queen == input$black_queen,
                white_rook == input$white_rook,
                black_rook == input$black_rook,
                white_knight == input$white_knight,
                black_knight == input$black_knight,
                white_bishop == input$white_bishop,
                black_bishop == input$black_bishop,
                white_pawn == input$white_pawn, 
                black_pawn == input$black_pawn
            )
    }
    
    num_position <<- nrow(dat)
    
    num_game <<- dat %>%
        summarise(total_games = n_distinct(id)) %>%
        collect() %>%
        pull(total_games)
    
    # aggregate results
    
    # first, eval to result layer

    eval_to_result <- dat %>%
      group_by(eval, result) %>%
      summarise(
        num_positions = n(),
        num_games = n_distinct(id),
        .groups = "drop"
      ) %>%
        collect() %>%
        mutate(
            source = case_match(
                eval,
                2 ~ "0",
                1 ~ "1",
                0 ~ "2",
                -1 ~ "3",
                -2 ~ "4"
            ),
            target = case_match(
                result,
                1 ~ "5",
                0 ~ "6",
                -1 ~ "7"
            )
        )
    
    # result to termination

    result_to_termination <- dat %>%
        group_by(eval, result, termination) %>%
        summarise(
            num_positions = n(),
            num_games = n_distinct(id),
            .groups = "drop"
        ) %>%
        collect() %>%
        mutate(
            source = case_match(
                result,
                1 ~ "5",
                0 ~ "6",
                -1 ~ "7"
            ),
            target = case_match(
                termination,
                "Normal" ~ "8",
                "Time forfeit" ~ "9",
                "Rules infraction" ~ "10"
            )
        )
    
    # altogether
    
    dat <- bind_rows(
        eval_to_result, result_to_termination
    ) %>%
        arrange(source, target)

    dat
  })

  output$plot <- renderPlotly({
    df_p <- filtered_dat()
    
    all_nodes <- union(unique(df_p$source), unique(df_p$target))
    
    node_labels <- case_match(
        all_nodes,
        "0" ~ "Winning",
        "1" ~ "Cursed Win",
        "2" ~ "Drawn",
        "3" ~ "Blessed draw",
        "4" ~ "Losing",
        "5" ~ "Win",
        "6" ~ "Draw",
        "7" ~ "Loss",
        "8" ~ "Normal",
        "9" ~ "Time forfeit",
        "10" ~ "Rules infraction"
    )

    node_colors <- case_match(
        all_nodes,
        # eval nodes
        c("0", "1", "5") ~ "#2b83ba",
        c("2", "3", "6") ~ "#DDA0DD",    
        c("4", "7") ~ "#d7191c",
        
        # termination nodes
        "8" ~ "#FF69B4",
        "9" ~ "#20B2AA",
        "10" ~ "#d7191c",
        
        # default fallback
        .default = "#87CEEB"  # sky blue for any unmatched nodes
    )
    
    # Create meaningful colors based on eval->result flow
    link_colors <- case_when(
        df_p$target %in% c("5") ~ alpha("#2b83ba", 0.6),
        df_p$target %in% c("6") ~ alpha("#DDA0DD", 0.6),
        df_p$target %in% c("7") ~ alpha("#d7191c", 0.6),
        
        df_p$source %in% c("0", "1", "5") ~ alpha("#2b83ba", 0.6),
        df_p$source %in% c("2", "3", "6") ~ alpha("#DDA0DD", 0.6),
        df_p$source %in% c("4", "7") ~ alpha("#d7191c", 0.6),
  
        # Default for result->termination flows
        .default = "#87CEEB"
    )
    
    source_indices <- match(df_p$source, all_nodes) - 1
    target_indices <- match(df_p$target, all_nodes) - 1
    
    # Create the Sankey plot
    fig <- plot_ly(
      type = "sankey",
      domain = list(
        x = c(0, 1),
        y = c(0, 1)
      ),
      orientation = "h",
      valueformat = ".0f",
      valuesuffix = " positions",
      node = list(
        label = node_labels,
        color = node_colors,
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = source_indices,
        target = target_indices,
        value = df_p$num_positions,
        color = link_colors,
        label = paste0(df_p$source, df_p$target, df_p$num_positions)
      )
    )

    fig <- fig %>% layout(
      font = list(size = 16),
      margin = list(t = 60),
      annotations = list(
          list(
              text = paste0(
                  format(num_position, big.mark = ","), " positions out of ",
                  format(total_positions, big.mark = ","), 
                  " total endgame positions (", 
                  round(100*num_position/total_positions, 1), "%)"
                 ),
              x = 0.5, y = 1.05,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "bottom",
              showarrow = FALSE,
              font = list(size = 14, color = "black")
          ),
          list(
              text = paste0(
                  format(num_game, big.mark = ","), " unique games ",
                  "out of ", format(total_games, big.mark = ","),
                  " (", round(100*num_game/total_games, 1), "%)"
                  
                ),
              x = 0.5, y = 1.02,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "bottom",
              showarrow = FALSE,
              font = list(size = 14, color = "black")
          )
      )
    )

    fig
  })
}

shinyApp(ui, server)
