library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

choice_labels <- list(
  "Kalorie",
  "Aktywność",
  "Nauka",
  "Sen",
  "Kroki",
  "Płyny",
  "Zadowolenie"
)

data <- read.csv("/home/antoni/Uni/Semestr-3/TWD/Projekty/Projekt-2/TWD-Project-2/data.csv") #nolint

data <- data %>%
  mutate(Data = as.Date(Data, format = "%Y-%m-%d"))

ui <- fluidPage(

  titlePanel("TWD Projekt 2"),

  sidebarLayout(

    sidebarPanel(

      conditionalPanel(
        condition = "input.tabs != 'Heatmapa'",
        selectInput("y", "Wybierz wartości", choices = choice_labels)
      ),

      sliderInput("date", "Wybierz zakres czasu:",
                  min = min(data$Data), max = max(data$Data),
                  value = c(min(data$Data), max(data$Data)),
                  timeFormat = "%Y-%m-%d"),


      conditionalPanel(
        condition = "input.tabs == 'Wykres punktowy'",
        checkboxInput("trend", "Linia trendu", value = FALSE)
      )
    ),

    mainPanel(

      tabsetPanel(
        id = "tabs",
        tabPanel("Wykres punktowy", plotlyOutput("scatter")),
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Wykres skrzypcowy", plotlyOutput("violin")),
        tabPanel("Heatmapa", plotOutput("heatmap")),
        tabPanel("Wykres słupkowy", plotOutput("col"))
      )
    )
  )
)

server <- function(input, output) {

  data_with_date <- reactive(filter(data, Data >= input$date[1],
                                    Data <= input$date[2]))

  no_of_dates <- reactive(as.integer(input$date[2] - input$date[1]))

  output$scatter <- renderPlotly({

    scatter_plot <- data_with_date() %>%
      ggplot(aes_string(x = "Data", y = input$y, color = "Imię")) +
      geom_point() +
      scale_color_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      theme_minimal()


    if (input$trend & no_of_dates() > 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "gam", aes(group = Imię,
                                        fill = Imię),
                    show.legend = FALSE,
                    se = FALSE,
                    formula = y ~ s(x, k = 1 + no_of_dates()))
    }

    if (input$trend & no_of_dates() == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm", aes(group = Imię),
                    se = FALSE)
    }

    ggplotly(scatter_plot)
  })

  output$boxplot <- renderPlotly({

    boxplot <- data_with_date() %>%
      ggplot(aes_string(x = "Imię", y = input$y, fill = "Imię")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_boxplot() +
      theme_minimal()

    ggplotly(boxplot)
  })

  output$violin <- renderPlotly({
    violin <- data_with_date() %>%
      ggplot(aes_string(x = "Imię", y = input$y, fill = "Imię")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_violin() +
      theme_minimal()

    ggplotly(violin)

  })

  output$heatmap <- renderPlot({
    data_with_date() %>%
      group_by(Imię, Zadowolenie) %>%
      summarise(n = n()) %>%
      group_by(Imię) %>%
      mutate(sum = sum(n)) %>%
      ungroup() %>%
      complete(Imię = unique(Imię),
               Zadowolenie = 1:10,
               fill = list(n = 0, sum = 1)) %>%
      ggplot(aes(x = Imię, y = as.factor(Zadowolenie), fill  = n / sum)) +
      geom_tile(width = 0.95, height = 0.80) +
      theme_minimal() +
      labs(y = "Poziom zadowolenia", fill = "Odsetek") +
      scale_fill_gradient(low = "#6C6EA0", high = "#FF1053")
  })


# TODO
#   output$col <- render_plot({
    
#   })

}

shinyApp(ui = ui, server = server)