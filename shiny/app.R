library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(stringr)
library(bslib)
library(shinyjs)
library(shinyBS)
library(bigrquery)
library(dbplyr)

bq_auth(path = "absolute-text-417919-b869e01f2c6d.json")

con <- dbConnect(
    bigrquery::bigquery(),
    project = "absolute-text-417919",
    dataset = "chess_endgames",
    billing = "absolute-text-417919"
)

dat <- tbl(con, "chess-endgames")

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css"),
        tags$script(src = "chess-editor.js"),
        # chessboard.js
        tags$script(src = "https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.js",
                    integrity = "sha384-8Vi8VHwn3vjQ9eUHUxex3JSN/NFqUg3QbPyX8kWyb93+8AC/pPWTzj+nHtbC5bxD",
                    crossorigin = "anonymous"),
        # Include chessboard.js default stylesheet
        tags$link(rel = "stylesheet", href = "https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.css")
    ),
    textOutput("plot_title"),
    textOutput("plot_subtitle"),
    br(),
    bsCollapse(
        id = "panels", open = "Filter games",
        bsCollapsePanel(
            HTML("Filter games <i class='fa fa-chevron-down pull-right' style='margin-top: 2px;'></i>"),
            value = "Filter games",
            fluidRow(div(
                           h4("Select time controls:"),
                           checkboxGroupButtons(
                               inputId = "time_control",
                               label = NULL,
                               choices = c(
                                   "UltraBullet", "Bullet", "Blitz", "Rapid", "Classical"
                               ),
                               selected = c(
                                   "UltraBullet", "Bullet", "Blitz", "Rapid", "Classical"
                               )
                       )
                ),
                div(
                           style = "margin: auto; text-align: center; display: flex; justify-content: center; align-items: center; flex-direction: column;",
                           h4("Select rating range:"),
                           sliderInput(
                               "rating",
                               label = NULL,
                               min = 400,
                               max = 3400,
                               value = c(400, 3400),
                               step = 100
                           )
                )
            ),
            # Chess board and buttons
            h4("Set up a position and search for games with the same material imbalance (Kings ignored) or the same exact position. Leave empty to see all games."),
            div(
                class = "chessboard-container",
                tags$div(id = "myBoard", style = "width: 400px;")
            ),
            div(
                style = "text-align: center; margin: 20px; display: flex; justify-content: center; gap: 10px;",
                actionBttn(
                    "clearBtn", 
                    label = "Clear board",
                    style = "bordered",
                    color = "primary",
                    icon = icon("trash"),
                    size = "sm"
                ),
                actionBttn(
                    "white_to_move", 
                    label = "White to move",
                    style = "bordered",
                    color = "primary",
                    size = "sm"
                )
            ),
            div(
                style = "text-align: center; margin: 20px; display: flex; justify-content: center; gap: 10px;",
                actionBttn(
                    inputId = "search_material",
                    label = "Search material",
                    icon = icon("play"),
                    style = "bordered",
                    color = "primary"
                ),
                actionBttn(
                    inputId = "search_position",
                    label = "Search position",
                    icon = icon("play"),
                    style = "bordered",
                    color = "primary"
                )
            )
        )
    ),
    uiOutput("plot")
)

server <- function(input, output, session) {
  # plot title

  output$plot_title <- renderText({
    "Lichess games: tablebase evals vs. results"
  })

  # define the subtitle of the graph

  output$plot_subtitle <- renderText({
    paste0(
      paste0(input$time_control, collapse = ", "),
      " games with average rating ",
      input$rating[1], "-", input$rating[2],
      " that reached a tablebase position"
    )
  })

  # making the buttons antagonistic
  
  counts <- reactiveValues()

  observeEvent(input$search_material, {
      updateCollapse(session, "panels", close = "Filter games")
    reset("search_position")
    
      counts$white_queen  <- str_count(input$current_fen, "Q")
      counts$black_queen  <- str_count(input$current_fen, "q")
      counts$white_rook   <- str_count(input$current_fen, "R")
      counts$black_rook   <- str_count(input$current_fen, "r")
      counts$white_knight <- str_count(input$current_fen, "N")
      counts$black_knight <- str_count(input$current_fen, "n")
      counts$white_bishop <- str_count(input$current_fen, "B")
      counts$black_bishop <- str_count(input$current_fen, "b")
      counts$white_pawn   <- str_count(input$current_fen, "P")
      counts$black_pawn   <- str_count(input$current_fen, "p")
  })
  
  position <- reactiveValues()
  
  observeEvent(input$search_position, {
      updateCollapse(session, "panels", close = "Filter games")
    reset("search_material")
    position$fen <- paste(input$current_fen, substr(move_state(), 1, 1))
    
    # if empty board
    position$empty <- input$current_fen == "8/8/8/8/8/8/8/8"
    
    print(input$current_fen)
    
    # detecting if kings are wrong
    position$invalid <- str_count(input$current_fen, "K") != 1 |
        str_count(input$current_fen, "k") != 1
    
    position$invalid <- position$invalid & !position$empty
  })

  # keep only the time control and rating ranges chosen

  filtered_dat <- reactive({
    dat <- dat %>%
      filter(time_control %in% !!input$time_control)

    if (input$rating[1] > 400) {
      dat <- dat %>%
        filter(avg_elo >= !!input$rating[1])
    }

    if (input$rating[2] < 3400) {
      dat <- dat %>%
        filter(avg_elo <= !!input$rating[2])
    }

    # store total games and observations
    
    total_positions <<- dat %>%
        summarise(total_games = n()) %>%
        collect() %>%
        pull(total_games)

    total_games <<- dat %>%
      summarise(total_games = n_distinct(id)) %>%
      collect() %>%
      pull(total_games)
    
    # filter specific fen
    if (input$search_position && !position$empty) {
        
        # Validate FEN
        shiny::validate(
            need(!position$invalid, "Invalid position.")
        )    
        
      dat <- dat %>%
        filter(
          stringr::str_detect(fens, !!position$fen)
        )
    }

    if (input$search_material) {
      # count material
      white_total <- counts$white_queen + counts$white_rook + 
          counts$white_knight + counts$white_bishop + counts$white_pawn
      black_total <- counts$black_queen + counts$black_pawn +
          counts$black_rook + counts$black_knight + counts$black_bishop
      total_pieces <- white_total + black_total
      
      # Validate piece count
      shiny::validate(
        need(total_pieces <= 3, "Too many pieces selected. Maximum is 3 pieces.")
      )

      # impose material
      if (total_pieces > 0) {
        dat <- dat %>%
          filter(
              white_material == white_total,
              black_material == black_total
        ) %>%
            filter(
                white_queen  == !!counts$white_queen,
                black_queen  == !!counts$black_queen,
                white_rook   == !!counts$white_rook,
                black_rook   == !!counts$black_rook,
                white_knight == !!counts$white_knight,
                black_knight == !!counts$black_knight,
                white_bishop == !!counts$white_bishop,
                black_bishop == !!counts$black_bishop,
                white_pawn   == !!counts$white_pawn,
                black_pawn   == !!counts$black_pawn
            )
      }
    }

    num_position <<- dat %>%
        summarise(total_games = n()) %>%
        collect() %>%
        pull(total_games)

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
          "Abandoned" ~ "10",
          "Rules infraction" ~ "11",
        )
      )

    # altogether

    dat <- bind_rows(
      eval_to_result, result_to_termination
    ) %>%
      arrange(source, target)

    dat
  })

  output$plot <- renderUI({
    if (input$search_material == 0 && input$search_position == 0) {
      # Show card instead of plot
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 80vh;",
        card(
          style = "text-align: center; padding: 40px;",
          h3("No Data Selected", style = "color: #666;"),
          p("Please click 'Search material' or 'Search position' to begin analyzing games."),
          icon("search", style = "font-size: 48px; color: #ccc; margin-top: 20px;")
        )
      )
    } else {
      # Show the actual plot
      withSpinner(
        plotlyOutput("actual_plot", width = "100%", height = "80vh")
      )
    }
  })

  output$actual_plot <- renderPlotly({
      
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
      "10" ~ "Abandoned",
      "11" ~ "Rules infraction"
    )
    
    node_type <- case_match(
        all_nodes,
        "0" ~ "Tablebase eval",
        "1" ~ "Tablebase eval",
        "2" ~ "Tablebase eval",
        "3" ~ "Tablebase eval",
        "4" ~ "Tablebase eval",
        "5" ~ "Result",
        "6" ~ "Result",
        "7" ~ "Result",
        "8" ~ "Termination",
        "9" ~ "Termination",
        "10" ~ "Termination",
        "11" ~ "Termination"
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
      "10" ~ "purple",
      "11" ~ "#d7191c",
      .default = "#87CEEB"
    )

    link_colors <- case_when(
      df_p$target %in% c("5") ~ alpha("#2b83ba", 0.6),
      df_p$target %in% c("6") ~ alpha("#DDA0DD", 0.6),
      df_p$target %in% c("7") ~ alpha("#d7191c", 0.6),
      df_p$source %in% c("0", "1", "5") ~ alpha("#2b83ba", 0.6),
      df_p$source %in% c("2", "3", "6") ~ alpha("#DDA0DD", 0.6),
      df_p$source %in% c("4", "7") ~ alpha("#d7191c", 0.6),
      .default = "#87CEEB"
    )
    
    # link types
    source_types <- case_match(
        df_p$source,
        as.character(0:4) ~ "Tablebase eval",
        as.character(5:7) ~ "Result",
        as.character(8:11) ~ "Termination"
    )
    
    target_types <- case_match(
        df_p$target,
        as.character(1:4) ~ "Tablebase eval",
        as.character(5:7) ~ "Result",
        as.character(8:11) ~ "Termination"
    )
    
    source_labels <- case_match(
        df_p$source,
        "0" ~ "Winning",
        "1" ~ "Cursed Win",
        "2" ~ "Drawn",
        "3" ~ "Blessed draw",
        "4" ~ "Losing",
        "5" ~ "Win",
        "6" ~ "Draw",
        "7" ~ "Loss",
    )
    
    target_labels <- case_match(
        df_p$target,
        "5" ~ "Win",
        "6" ~ "Draw",
        "7" ~ "Loss",
        "8" ~ "Normal",
        "9" ~ "Time forfeit",
        "10" ~ "Abandoned",
        "11" ~ "Rules infraction"
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
      valuesuffix = "",
      node = list(
        label = node_labels,
        customdata = node_type,
        hovertemplate = HTML(
            " <b>%{customdata}:</b> %{label}<br>",
            "<b> Positions:</b> %{value}<extra></extra>"
        ),
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
        label = paste0(df_p$num_games),
        customdata = cbind(source_types, source_labels,
                           target_types, target_labels,
                           round(100*df_p$num_positions/num_position, 1),
                           round(100*df_p$num_games/num_game, 1)),
        hovertemplate = HTML(
            " <b>%{customdata[0]}:</b> %{customdata[1]}<br>",
            "<b>%{customdata[2]}:</b> %{customdata[3]}<br>",
            "<b>Positions:</b> %{value} (%{customdata[4]}%) <br>",
            "<b>Games: </b> %{label} (%{customdata[5]}%)<extra></extra>"
        )
      )
    )

    fig <- fig %>% layout(
      font = list(size = 16, family = "JetBrains Mono"),
      margin = list(t = 60),
      annotations = list(
        list(
          text = paste0(
            format(num_position, big.mark = ","), " positions out of ",
            format(total_positions, big.mark = ","),
            " total endgame positions (",
            round(100 * num_position / total_positions, 1), "%)"
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
            " (", round(100 * num_game / total_games, 1), "%)"
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
  
  # Track move state
  move_state <- reactiveVal("white")
  
  observeEvent(input$white_to_move, {
      if (move_state() == "white") {
          # Switch to black
          move_state("black")
          updateActionButton(
              session, 
              "white_to_move", 
              label = "Black to move"
          )
          # You can also add custom CSS class
          runjs("$('#white_to_move').removeClass('btn-primary').addClass('btn-dark');")
      } else {
          # Switch to white  
          move_state("white")
          updateActionButton(
              session, 
              "white_to_move", 
              label = "White to move"
          )
          runjs("$('#white_to_move').removeClass('btn-dark').addClass('btn-primary');")
      }
  })
  
  # Access current state with: move_state()
}

shinyApp(ui, server)
