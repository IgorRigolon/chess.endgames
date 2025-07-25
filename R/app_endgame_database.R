library(arrow)
library(dplyr)
library(plotly)

dat <- read_dataset("data")

node_colors <- c(
    rep("lightblue", length(eval_nodes)),      # eval nodes
    rep("lightgreen", length(result_nodes)),   # result nodes  
    rep("lightcoral", length(termination_nodes)) # termination nodes
)

# store total games and observations

total_positions <- nrow(dat)

total_games <- length(unique(dat$id))

ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("Telephones by region"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("region", "Region:", 
                        choices=colnames(WorldPhones)),
            hr(),
            helpText("Data from AT&T (1961) The World's Telephones.")
        ),
        
        # Create a spot for the barplot
        mainPanel(
            plotOutput("phonePlot")  
        )
        
    )
)

server <- function(input, output) {
    
    # keep only the time control and rating ranges chosen
    
    filtered_dat <- reactive({
        dat %>%
            filter(time_control %in% input$time_control) %>%
            filter(avg_elo >= input$rating[1], avg_elo <= input$rating[2])
    
    # separate cases for material count, set positions, and FEN search
    
    if (input$type == "material") {
        
    }
    
    if (input$type == "preset") {
        
    }
    
    if (input$type == "fen") {
        
    }
    
    # aggregate results
    
    dat <- dat %>%
        group_by(eval, result, termination) %>%
        summarise(
            num_positions = n(),
            num_games = n_distinct(id),
        )
    
    collect(dat)
    
    })
    
    output$plot <- renderPlotly({
        
        # Create node labels
        eval_nodes <- unique(filtered_dat$eval)
        result_nodes <- paste0(unique(filtered_dat$result), "_result")
        termination_nodes <- paste0(unique(filtered_dat$termination), "_term")
        
        all_nodes <- c(eval_nodes, result_nodes, termination_nodes)
        
        eval_labels <- case_when(
            eval_nodes == "1-0" ~ "Position favors White",
            eval_nodes == "0-1" ~ "Position favors Black", 
            eval_nodes == "1/2-1/2" ~ "Position is equal",
            TRUE ~ eval_nodes
        )
        
        termination_labels <- case_when(
            termination_nodes == "Normal" ~ "Checkmate or resignation",
            termination_nodes == "Time forfeit" ~ "Time Out",
            TRUE ~ termination_nodes
        )
        
        all_node_labels <- c(eval_labels, result_labels, termination_labels)
        
        # Create a mapping of node names to indices (0-based for plotly)
        node_indices <- setNames(0:(length(all_nodes)-1), all_nodes)
        
        # Prepare links for eval -> result
        eval_to_result <- filtered_dat %>%
            group_by(eval, result) %>%
            summarise(value = sum(num_positions), .groups = 'drop') %>%
            mutate(
                source = node_indices[eval],
                target = node_indices[paste0(result, "_result")]
            )
        
        # Prepare links for result -> termination
        result_to_termination <- filtered_dat %>%
            mutate(
                source = node_indices[paste0(result, "_result")],
                target = node_indices[paste0(termination, "_term")],
                value = num_positions
            )
        
        # Combine all links
        all_links <- rbind(
            eval_to_result[, c("source", "target", "value")],
            result_to_termination[, c("source", "target", "value")]
        )
        
        # Create the Sankey plot
        fig <- plot_ly(
            type = "sankey",
            domain = list(
                x = c(0,1),
                y = c(0,1)
            ),
            orientation = "h",
            valueformat = ".0f",
            valuesuffix = " positions",
            
            node = list(
                label = all_node_labels,  # Use readable labels
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
                color = "rgba(100,100,100,0.4)"  # semi-transparent gray
            )
        )
        
        fig <- fig %>% layout(
            title = "Chess Position Flow: Evaluation → Result → Termination",
            font = list(size = 10)
        )
        
        fig
    })
}

shinyApp(ui, server)