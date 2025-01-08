library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(shinythemes)

choice_labels <- list(
  "Kalorie [kcal]" = "Kalorie",
  "Aktywność [min]" = "Aktywnosc",
  "Nauka [h]" = "Nauka",
  "Sen [h]" = "Sen",
  "Kroki [1]" = "Kroki",
  "Płyny [L]" = "Plyny",
  "Zadowolenie [1-10]" = "Zadowolenie"
)

choice_labels_reversed <- setNames(names(choice_labels), choice_labels)


labeltoint <- function(label){
  which(unname(choice_labels) == label);
}

przedzialy <- list(
  seq(0, 1250, 250), # kalorie
  c(0, 15, 30, 45, 60, 90, 120, 150, 180), # aktywnosc
  0:8, # nauka
  5:12, # sen
  c(0, 2500, 5000, 7500, 10000, 12500, 20000, 30000), # kroki
  seq(1, 3, 0.25), # plyny
  1:10 # zadowolenie
)

data <- read.csv("data.csv")

data <- data %>%
  mutate(across(4:9, as.numeric))
  colnames(data) <- c("Data", "Imie", "Kalorie", "Aktywnosc", "Nauka", "Sen", "Kroki", "Plyny", "Zadowolenie") #nolint

data <- data %>%
  mutate(Data = as.Date(Data, format = "%Y-%m-%d"))

data <- data %>%
  mutate(dzienTygodnia = weekdays(Data),
         dzienTygodnia = factor(dzienTygodnia,
                                levels = c("poniedziałek", "wtorek", "środa",
                                           "czwartek", "piątek", "sobota", "niedziela"))) #nolint

ui1 <- fluidPage(

  sidebarLayout(

    sidebarPanel(

      selectInput("y", "Wybierz wartości", choices = choice_labels),

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
        tabPanel("Wykres słupkowy", plotlyOutput("col"))
      )
    )
  )
)

ui2 <- fluidPage()

ui <- navbarPage("TWD Projekt 2",
                 tabPanel("Wykresy interaktywne", ui1),
                 tabPanel("Wnioski", ui2),
                 theme = shinytheme("lumen"))

server <- function(input, output) {

  data_with_date <- reactive(filter(data, Data >= input$date[1],
                                    Data <= input$date[2]))

  no_of_dates <- reactive(as.integer(input$date[2] - input$date[1]))

  przedzial <- reactive(przedzialy[[labeltoint(input$y)]])

  output$scatter <- renderPlotly({

    scatter_plot <- data_with_date() %>%
      ggplot(aes_string(x = "Data", y = input$y, color = "Imie")) +
      geom_point() +
      scale_color_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])


    if (input$trend & no_of_dates() > 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "gam", aes(group = Imie,
                                        fill = Imie),
                    se = FALSE,
                    formula = y ~ s(x, k = 1 + no_of_dates()))
    }

    if (input$trend & no_of_dates() == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm", aes(group = Imie),
                    se = FALSE)
    }

    ggplotly(scatter_plot)
  })

  output$boxplot <- renderPlotly({

    boxplot <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_boxplot() +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])

    ggplotly(boxplot)
  })

  output$violin <- renderPlotly({
    violin <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_violin() +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]])

    ggplotly(violin)
  })

  output$heatmap <- renderPlot({

    data_with_date() %>%
      mutate(quantile_group = sapply(.data[[input$y]],
                                     function(x) przedzial()[which.min(abs(przedzial() - x))])) %>% #nolint
      group_by(Imie, quantile_group) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Imie) %>%
      mutate(sum = sum(n)) %>%
      ungroup() %>%
      complete(Imie = unique(Imie),
               quantile_group = przedzial(),
               fill = list(n = 0, sum = 1)) %>%
      ggplot(aes(x = Imie, y = as.factor(quantile_group), fill = n / sum)) +
      geom_tile(width = 0.95, height = 0.80) +
      theme_minimal() +
      labs(y = choice_labels_reversed[[input$y]], x = "Imię",
           fill = "Odsetek") +
      scale_fill_gradient(low = "#6C6EA0", high = "#FF1053") +
      theme_minimal()
  })


  output$col <- renderPlotly({
    bar_data <- data_with_date() %>% 
      group_by(dzienTygodnia) %>%
      summarise(val = mean(.data[[input$y]], na.rm = TRUE), .groups = "drop")

    line_data <- data_with_date() %>% 
      group_by(Imie, dzienTygodnia) %>%
      summarise(val = mean(.data[[input$y]], na.rm = TRUE), .groups = "drop")

    colplot <- ggplot() +
      geom_col(data = bar_data,
               aes(x = dzienTygodnia, y = val), fill = "lavender") +
      geom_line(data = line_data,
                aes(x = dzienTygodnia, y = val,
                    group = Imie, color = Imie), size = 0.75) +
      scale_color_manual(values = c("Antoni" = "#66C7F4", "Jan" = "#6C6EA0", "Kacper" = "#FF1053")) + #nolint
      labs(y = "Średnia wartość dla wszystkich",
           x = "Dzień tygodnia", color = "Średnia wartość dla osoby",
           title = paste("Średnie wartości: ", choice_labels_reversed[[input$y]])) + #nolint
      theme_minimal()

    ggplotly(colplot)
  })

}

shinyApp(ui = ui, server = server)