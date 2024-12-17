library(shiny)
library(dplyr)
library(ggplot2)

choice_labels <- list(
  "Kalorie",
  "Aktywność",
  "Nauka",
  "Sen",
  "Kroki",
  "Płyny",
  "Zadowolenie"
)

data <- read.csv("/home/antoni/Uni/Semestr-3/TWD/Projekty/Projekt-2/TWD-Project-2/data.csv")

ui <- fluidPage(

    titlePanel("very very s3xy project adasdasdasd"),

    sidebarLayout(

        sidebarPanel(

            selectInput("y", "Wybierz wartości na osi OY", choices = choice_labels),

            sliderInput("date", "Wybierz zakres czasu:", min = 1, max = 4, value = 3)
        ),

        mainPanel(

            tabsetPanel(
                id = "tabs",
                tabPanel("Wykres punktowy", plotOutput("scatter"))
            )
        )
    )
)

server <- function(input, output) {
    
  values <- reactive({

    }
  })

}


shinyApp(ui = ui, server = server)