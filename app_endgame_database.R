library(arrow)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(stringr)

dat <- open_dataset("data") %>%
    mutate(eval = ifelse(eval == 0, 9, eval))

# store total games and observations

total_positions <- nrow(dat)

total_games <- dat %>%
    summarise(total_games = n_distinct(id)) %>%
    collect() %>%
    pull(total_games)

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
      color: white !important;
      text-shadow: 1px 1px 1px black, -1px -1px 1px black, 1px -1px 1px black, -1px 1px 1px black;
      font-weight: bold;
    }
    
    .black-pieces .control-label {
      color: #333;
      font-weight: bold;
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
        min = 0,
        max = 3500,
        value = c(0, 3500),
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
        input$white_bishop + input$black_queen + input$white_pawn + input$black_pawn
        input$black_rook + input$black_knight + input$black_bishop
    
    # Validate piece count
    validate(
        need(total_pieces <= 3, "Too many pieces selected. Maximum is 3 pieces.")
    )
    
    # impose material
    if (total_pieces > 0) {
        dat <- dat %>%
            filter(
                str_count(fens, "Q") == input$white_queen, 
                str_count(fens, "q") == input$black_queen,
                str_count(fens, "R") == input$white_rook,
                str_count(fens, "r") == input$black_rook,
                str_count(fens, "N") == input$white_knight,
                str_count(fens, "n") == input$black_knight,
                str_count(fens, "B") == input$white_bishop,
                str_count(fens, "b") == input$black_bishop,
                str_count(fens, "P") == input$white_pawn, 
                str_count(fens, "p") == input$black_pawn,
            )
    }
    
    num_position <<- nrow(dat)
    
    num_game <<- dat %>%
        summarise(total_games = n_distinct(id)) %>%
        collect() %>%
        pull(total_games)
    
    # aggregate results

    dat <- dat %>%
      group_by(eval, result, termination) %>%
      summarise(
        num_positions = n(),
        num_games = n_distinct(id),
      )

    dat <- dat %>%
      collect()

    dat
  })

  output$plot <- renderPlotly({
    df_p <- filtered_dat()

    # Create node labels
    eval_nodes <- unique(df_p$eval)
    result_nodes <- unique(df_p$result)
    termination_nodes <- unique(df_p$termination)

    # color palette

    node_colors <- c(
      rep("lightblue", length(eval_nodes)), # eval nodes
      rep("lightgreen", length(result_nodes)), # result nodes
      rep("lightcoral", length(termination_nodes)) # termination nodes
    )

    all_nodes <- c(eval_nodes, result_nodes, termination_nodes)

    eval_labels <- case_match(
      eval_nodes,
      2 ~ "Winning",
      1 ~ "Cursed win",
      9 ~ "Drawn",
      -1 ~ "Blessed draw",
      -2 ~ "Losing"
    )

    result_labels <- case_match(
        result_nodes,
        1 ~ "Win",
        0 ~ "Draw",
        -1 ~ "Loss"
    )

    termination_labels <- termination_nodes

    all_node_labels <- c(eval_labels, result_labels, termination_labels)

    # Create a mapping of node names to indices (0-based for plotly)
    get_index <- function(x) match(x, all_nodes) - 1

    # Prepare links for eval -> result
    eval_to_result <- df_p %>%
      group_by(eval, result) %>%
      summarise(value = sum(num_positions), .groups = "drop") %>%
      mutate(
        source = get_index(eval),
        target = get_index(result)
      )

    # Prepare links for result -> termination
    result_to_termination <- df_p %>%
      mutate(
        source = get_index(result),
        target = get_index(termination),
        value = num_positions
      )

    # Combine all links
    all_links <- rbind(
      eval_to_result[, c("source", "target", "value")],
      result_to_termination[, c("source", "target", "value")]
    )

    node_colors <- case_match(
        all_nodes,
        # eval nodes
        c("1", "2") ~ "#2b83ba",
        c("0", "9") ~ "#DDA0DD",    
        c("-1", "-2") ~ "#d7191c",
        
        # termination nodes
        "Normal" ~ "#FF69B4",
        "Time forfeit" ~ "#20B2AA",
        
        # default fallback
        .default = "#87CEEB"  # sky blue for any unmatched nodes
    )
    
    # Create meaningful colors based on eval->result flow
    link_colors <- case_when(
        all_links$target %in% c(get_index("1"), get_index("2")) ~ alpha("#2b83ba", 0.6),
        all_links$target %in% c(get_index("0"), get_index("9")) ~ alpha("#DDA0DD", 0.6),
        all_links$target %in% c(get_index("-1"), get_index("-2")) ~ alpha("#d7191c", 0.6),
        
        all_links$source %in% c(get_index("1"), get_index("2")) ~ alpha("#2b83ba", 0.6),
        all_links$source %in% c(get_index("0"), get_index("9")) ~ alpha("#DDA0DD", 0.6),
        all_links$source %in% c(get_index("-1"), get_index("-2")) ~ alpha("#d7191c", 0.6),
  
        # Default for result->termination flows
        .default = "#87CEEB"
    )
    
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
        label = all_node_labels, # Use readable labels
        color = node_colors,
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = all_links$source,
        target = all_links$target,
        value = all_links$value,
        color = link_colors
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
