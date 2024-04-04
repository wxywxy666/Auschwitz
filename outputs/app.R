library(shiny)
library(ggplot2)
library(DT)
library(rsconnect)

# Load data
data <- read.csv("Auschwitz_data.csv", stringsAsFactors = FALSE)
data
# Convert numeric columns to numeric type
data$Number.of.victims <- gsub("~", "", data$Number.of.victims)
data$Number.of.victims <- gsub(" thousand", "e3", data$Number.of.victims)
data$Number.of.victims <- gsub(" million", "e6", data$Number.of.victims)
data$Number.of.victims <- as.numeric(data$Number.of.victims)





ui <- fluidPage(
  titlePanel("Auschwitz Victims by Nationality/Category"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("nationality", "Select Nationality/Category:",
                         choices = data$Nationality.Category,
                         selected = data$Nationality.Category)
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data[data$Nationality.Category %in% input$nationality,]
  })
  
  output$plot <- renderPlot({
    gg <- ggplot(filtered_data(), aes_string(x="`Nationality/Category`", y="`Number of victims`", fill="`Nationality/Category`")) +
      geom_bar(stat="identity") +
      theme_minimal() +
      labs(title="Number of Victims by Nationality/Category", x=NULL, y="Number of Victims") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    print(gg)
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)
