library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)

stumpage <- read.csv("https://raw.githubusercontent.com/azd169/timber_prices/main/ms_stumpage.csv",
                     header = T)

# Define UI 

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
    :root {
      --background-color: #ffffff;
      --text-color: #000000;
      --link-color: #0072B2;
    }
    
    @media (prefers-color-scheme: dark) {
      :root {
        --background-color: #1e1e1e;
        --text-color: #ffffff;
        --link-color: #8ab4f8;
      }
    }

    body {
      background-color: var(--background-color);
      color: var(--text-color);
    }

    a {
      color: var(--link-color);
    }
  "))
  ),
  
  fluidRow(
    column(12,
           tags$div(style = "display: flex; align-items: center; justify-content: center;",
                    tags$img(src = "MS_logo.jpg", height = "120px", alt = "Logo",
                             style = "margin-right: 20px; margin-top: 10px;"),
                    tags$h1("Mississippi Timber Price Report",
                            style = "font-size: 52px; font-weight: bold;"),
                    
           )
    )
  ),
  
  fluidRow(
    column(12,
          tags$div(style = "display: flex; align-items: flex-start; gap: 10px; margin-top: 20px; font-size: 20px;",
                    tags$div(style = "flex: 1;",
                             radioButtons("price_selector", "Select Price:", 
                                          choices = c("Minimum", "Average", "Maximum"),
                                          selected = "Average"),  # Default selection
                             uiOutput("type_selector"), # Dynamic UI for selecting types
                             checkboxGroupInput("selected_quarter", "Select Quarter(s):", 
                                                choices = c("Q1", "Q2", "Q3", "Q4"), # Specify the desired order here
                                                selected = character(0)), # Dynamic UI for selecting quarters
                             sliderInput("year_slider", "Select Year Range:", 
                                         min = min(stumpage$Year, na.rm = T),
                                         max = max(stumpage$Year, na.rm = T),
                                         value = c(min(stumpage$Year, na.rm = T), 
                                                   max(stumpage$Year, na.rm = T)),
                                         sep = "",
                                         ticks = F,  
                                         animate = F,
                                         step = 1),
                             tags$button(
                               id = "deselect_all",
                               type = "button",
                               class = "btn btn-danger action-button",
                               style = "margin-top: 10px;",
                               `aria-label` = "Clear all selections",
                               "Clear All"
                             )
                    ),
                    tags$div(style = "flex: 55%; text-align: left;",
                             tags$p("The Mississippi Timber Price Report provides a picture of timber market activity
                                     showing statewide stumpage prices for common forest products.
                                     This report should only be used as a guide to help individuals monitor timber market
                                     trends. The average price should not be applied as fair market value for a specific
                                     timber sale because many variables influence actual prices each landowner will receive.
                                     Timber prices are available by contacting your local county Extension office or consulting",
                                    tags$a(href = "http://www.extension.msstate.edu/forestry/forest-economics/timber-prices",
                                    "Mississippi State Forestry Extension.")),
                             tags$p("Timber prices are generated using data from timber sales conducted and reported across
                                    Mississippi. Reporters include forest product companies, logging contractors, consulting
                                    foresters, landowners, and other natural resource professionals. Are you interested in
                                    reporting timber prices or do you want more information about the Mississippi Timber Price
                                    Report? Please contact",
                                    tags$a(href = "mailto:sabhyata.lamichhane@msstate.edu", "Sabhyata Lamichhane"), 
                                           "at 662-325-3550 for more information.")
                    )
           )
    )
  ),
  
  fluidRow(
    column(12,
           uiOutput("plot_ui"),
           style = "text-align: center; margin-top: 20px;"
    )
  ),

fluidRow(
  column(12,
         downloadButton("download_data", "Download Data as CSV", class = "btn-primary"),
         style = "text-align: center; margin-top: 20px;"
  )
),
  
  fluidRow(
    column(12,
           tags$div(style = "text-align: center; margin-top: 40px; font-size: 20px;",
                    tags$p("For further assistance contact ", tags$a(href = "mailto:ads992@msstate.edu",
                                                                     "Andrea De Stefano")
                           )
           )
    )
  ),
  
  # Include JavaScript to get screen dimensions
  tags$script(HTML("
    $(document).on('shiny:connected', function(event) {
      var width = $(window).width();
      var height = $(window).height();
      Shiny.onInputChange('window_width', width);
      Shiny.onInputChange('window_height', height);
    });
    $(window).resize(function(event) {
      var width = $(window).width();
      var height = $(window).height();
      Shiny.onInputChange('window_width', width);
      Shiny.onInputChange('window_height', height);
    });
  ")
  )
)

# Define server 
server <- function(input, output, session) {
  
  # Create the dynamic UI for the type selector
  output$type_selector <- renderUI({
    checkboxGroupInput("selected_type", "Select Type(s):", 
                       choices = unique(stumpage$Type), 
                       selected = character(0))
  })
  
  output$quarter_selector <- renderUI({
    checkboxGroupInput("selected_quarter", "Select Quarter(s):", 
                       choices = unique(stumpage$Quarter), 
                       selected = character(0))
  })
  
  # Update the selected types
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_type", selected = unique(stumpage$Type))
    updateCheckboxGroupInput(session, "selected_type", selected = unique(stumpage$Type))
    updateSliderInput(session, "year_slider", value = c(min(stumpage$Year, na.rm = T), 
                                                       max(stumpage$Year, na.rm = T)))
    updateCheckboxGroupInput(session, "selected_quarter", selected = unique(stumpage$Quarter))
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_type", selected = character(0))
    updateSliderInput(session, "year_slider", value = c(min(stumpage$Year, na.rm = T), 
                                                        max(stumpage$Year, na.rm = T)))
    updateCheckboxGroupInput(session, "selected_quarter", selected = character(0))
  })
  
  # Reactive expression to filter data based on selected types
  filtered_data <- reactive({
    req(input$selected_type, input$year_slider, input$selected_quarter)  # Ensure there is at least one selected type, year, and quarter
    stumpage %>% 
      filter(
        Type %in% input$selected_type,
        Year >= input$year_slider[1],
        Year <= input$year_slider[2],
        Quarter %in% input$selected_quarter
      ) %>%
      mutate(Type = factor(Type, levels = c("Pine Sawtimber",
                                            "Mixed Hardwood Sawtimber",
                                            "Pine Chip-n-Saw",
                                            "Pine Pulpwood",
                                            "Hardwood Pulpwood")))
  })
  
  # Render the plot or a message based on the selected types
  
  output$plot_ui <- renderUI({
    req(input$window_width, input$window_height)  # Ensure we have screen dimensions
    
    plot_height <- input$window_height - 200  # Adjust this value as needed
    plot_width <- input$window_width - 30  # Adjust this value as needed
    
    if (is.null(input$selected_type) || length(input$selected_type) == 0 ||
        is.null(input$year_slider) || length(input$year_slider) == 0 ||
        is.null(input$selected_quarter) || length(input$selected_quarter) == 0) {
      return(tags$div(
        tags$h3("No types, years, or quarters selected. Please select at least one type, one year, and one quarter to display the plot."),
        style = "color: red; text-align: center; margin-top: 250px;"
      ))
    } else {
      plotlyOutput("plot", height = paste0(plot_height, "px"), width = paste0(plot_width, "px"))
    }
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    # Check if there's any data to plot
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Determine the y-axis based on the selected price metric
    price_column <- switch(input$price_selector,
                           "Minimum" = data$Minimum,
                           "Average" = data$Average,
                           "Maximum" = data$Maximum)
    
    p1 <- ggplot(data,
                 aes(x = Time,
                     y = price_column,
                     group = Type,
                     color = Type,
                     shape = Type,
                     text = paste("Type:", Type, "<br>Time:", Time, "<br>Price ($/ton):", price_column)))  +
      geom_line(linewidth = 1) +          
      geom_point(size = 3) +      
      labs(y = "Price ($/ton)") + 
      scale_shape_manual(values = c("Pine Sawtimber" = 21,
                                    "Mixed Hardwood Sawtimber" = 22,
                                    "Pine Chip-n-Saw" = 23,
                                    "Pine Pulpwood" = 24,
                                    "Hardwood Pulpwood" = 25), name = "Type") +
      scale_color_manual(values = c("Pine Sawtimber" = "#D55E00",
                                    "Mixed Hardwood Sawtimber" = "#009E73",
                                    "Pine Chip-n-Saw" = "#E69F00",
                                    "Pine Pulpwood" = "#0072B2",
                                    "Hardwood Pulpwood" = "#CC79A7"), name = "Type") +
      scale_x_discrete(breaks = unique(data$Time)[seq(1,length(unique(data$Time)), by = 4)]) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      theme(strip.text = element_text(size = 14),
            legend.title = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"), 
            panel.background = element_blank(),
            legend.text = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            legend.position = "bottom")
    
    plotly_plot <- ggplotly(p1, tooltip = "text")
    
    # Adjust the layout to move the legend to the bottom in Plotly
    plotly_plot <- plotly_plot %>%
      layout(legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
             margin = list(l = 50, r = 50, t = 50, b = 100))
    
    plotly_plot
  })
  
  # Download data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      data <- filtered_data()
      if (!is.null(data) && nrow(data) > 0) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
