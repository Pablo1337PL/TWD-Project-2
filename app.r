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
  c(0, 250, 500, 750, 1000, 1500, 2000), #Kalorie
  c(0, 15, 30, 45, 60, 90, 120, 180, 240, 300), #Aktywnosc
  c(0, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), #Nauka
  c(4, 5, 6, 7, 7.5, 7.75, 8, 8.25, 8.5, 9, 10, 11, 12), #Sen
  c(0, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 30000), #Kroki
  c(1, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.5), #Plyny
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # zadowolenie
)

data <- read.csv("/home/antoni/Uni/Semestr-3/TWD/Projekty/Projekt-2/TWD-Project-2/data.csv") #nolint
# data <- read.csv("data.csv")


data <- data %>%
    mutate(across(4:9, as.numeric))
colnames(data) <- c("Data", "Imie", "Kalorie", "Aktywnosc", "Nauka", "Sen", "Kroki", "Plyny", "Zadowolenie") #nolint

data <- data %>%
  mutate(Data = as.Date(Data, format = "%Y-%m-%d"))

#data <- data %>%
#  mutate(dzienTygodnia = weekdays(Data)),
#         dzienTygodnia = factor(dzienTygodnia, 
#                                levels = c("Monday", "Tuesday", "Wednesday", 
#                                           "Thursday", "Friday", "Saturday", "Sunday")))

# data <- data %>%
#   mutate(
#     dzienTygodnia = {
#       # Set locale to English
#       original_locale <- Sys.getlocale("LC_TIME")
#       Sys.setlocale("LC_TIME", "C")
#       # Get weekdays in English
#       weekdays(Data)
#     },
#     # Restore original locale
#     { Sys.setlocale("LC_TIME", original_locale) },
#     dzienTygodnia = factor(dzienTygodnia, 
#                            levels = c("Monday", "Tuesday", "Wednesday", 
#                                       "Thursday", "Friday", "Saturday", "Sunday"))
#   )




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
                 theme = shinytheme("cyborg"))

server <- function(input, output) {

  data_with_date <- reactive(filter(data, Data >= input$date[1],
                                    Data <= input$date[2]))

  no_of_dates <- reactive(as.integer(input$date[2] - input$date[1]))

  output$scatter <- renderPlotly({

    scatter_plot <- data_with_date() %>%
      ggplot(aes_string(x = "Data", y = input$y, color = "Imie")) +
      geom_point() +
      scale_color_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      theme_minimal()


    if (input$trend & no_of_dates() > 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "gam", aes(group = Imie,
                                        fill = Imie),
                    se = TRUE,
                    formula = y ~ s(x, k = 1 + no_of_dates()))
    }

    if (input$trend & no_of_dates() == 1) {
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm", aes(group = Imie),
                    se = TRUE)
    }

    ggplotly(scatter_plot)  %>%
      layout(
        title = list(
          font = list(color = "white")
        ),
        xaxis = list(
          title = "Data",
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        yaxis = list(
          title = choice_labels_reversed[[input$y]],
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        legend = list(
          title = list(
            text = "Osoba",
            font = list(
              color = "white"
            )
          ),
          font = list(
            color = "white"
          )
        ),
        paper_bgcolor = "black",
        plot_bgcolor = "black"
      )

  })

  output$boxplot <- renderPlotly({

    boxplot <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_boxplot() +
      theme_minimal()

    ggplotly(boxplot) %>%
      layout(
        title = list(
          font = list(color = "white")
        ),
        xaxis = list(
          title = "Data",
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        yaxis = list(
          title = choice_labels_reversed[[input$y]],
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        legend = list(
          title = list(
            text = "Osoba",
            font = list(
              color = "white"
            )
          ),
          font = list(
            color = "white"
          )
        ),
        paper_bgcolor = "black",
        plot_bgcolor = "black"
      )
  })

  output$violin <- renderPlotly({
    violin <- data_with_date() %>%
      ggplot(aes_string(x = "Imie", y = input$y, fill = "Imie")) +
      scale_fill_manual(values = c("#66C7F4", "#6C6EA0", "#FF1053")) +
      geom_violin() +
      theme_minimal()

    ggplotly(violin) %>%
      layout(
        title = list(
          font = list(color = "white")
        ),
        xaxis = list(
          title = "Data",
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        yaxis = list(
          title = choice_labels_reversed[[input$y]],
          color = "white",
          gridcolor = "white",
          tickfont = list(
            color = "white"
          )
        ),
        legend = list(
          title = list(
            text = "Osoba",
            font = list(
              color = "white"
            )
          ),
          font = list(
            color = "white"
          )
        ),
        paper_bgcolor = "black",
        plot_bgcolor = "black"
      )

  })

  output$heatmap <- renderPlot({
    wartosc <- input$y

  # Ensure that the label exists in `choice_labels`
  if (!wartosc %in% choice_labels) {
    stop("Invalid choice for 'wartosc'. Ensure it matches a valid label.")
  }

  # Select the corresponding range from `przedzialy`
  przedzial <- przedzialy[[labeltoint(wartosc)]]

  # Process and plot the data
  data_with_date() %>%
    mutate(quantile_group = sapply(.data[[wartosc]], 
                                   function(x) przedzial[which.min(abs(przedzial - x))])) %>%
    group_by(Imie, quantile_group) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Imie) %>%
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    complete(Imie = unique(Imie),
             quantile_group = przedzial,
             fill = list(n = 0, sum = 1)) %>%
    ggplot(aes(x = Imie, y = as.factor(quantile_group), fill = n / sum)) +
    geom_tile(width = 0.95, height = 0.80) +
    theme_minimal() +
    labs(y = choice_labels_reversed[[input$y]], fill = "Odsetek") +
    scale_fill_gradient(low = "#6C6EA0", high = "#FF1053") +
    theme(
    # Set the background of the plot area (panel) to black
    panel.background = element_rect(fill = "black"),
    
    # Set the background of the entire plot (including outer area) to black
    plot.background = element_rect(fill = "black"),
    
    # Set the background of the legend to black (if applicable)
    legend.background = element_rect(fill = "black"),
    
    # Set axis text color to white for visibility
    axis.text = element_text(color = "white"),
    
    # Set axis title color to white
    axis.title = element_text(color = "white"),
    
    # Set grid line color to gray (for visibility on black background)
    panel.grid = element_line(color = "black"),

    legend.text = element_text(color = "white")
  )
  })


  output$col <- renderPlotly({
    zmienna <- input$y #Wybór kolumny
    
    bar_data <- data_with_date() %>% 
    group_by(dzienTygodnia) %>%
    summarise(val = mean(.data[[zmienna]], na.rm = TRUE), .groups = "drop")

    line_data <- data_with_date() %>% 
        group_by(Imie, dzienTygodnia) %>%
        summarise(val = mean(.data[[zmienna]], na.rm = TRUE), .groups = "drop")

    plotCol <- ggplot() +
    geom_col(data = bar_data, aes(x = dzienTygodnia, y = val), fill = "lightgreen") +
    geom_line(data = line_data, aes(x = dzienTygodnia, y = val, group = Imie, color = Imie), size = 0.75) +
    scale_color_manual(values = c("Antoni" = "#66C7F4", "Jan" = "#6C6EA0", "Kacper" = "#FF1053")) +
    labs(y = zmienna, x = "Day of the Week", 
         title = paste("Values of Variable:", zmienna)) +
    theme_minimal()



    ggplotly(plotCol)
  })

}

shinyApp(ui = ui, server = server)