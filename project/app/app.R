library(shiny)
library(h2o)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Banking app"),
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file", "Ikelti csv faila")
      
    ),
    mainPanel(
      
      tableOutput("table"),
      tableOutput("predictions")
    )
  )
)

server <- function(input, output) {
  h2o.init()
  dl_model <- h2o.loadModel("../4-model/final_grid_model_4")
  
  output$table <- renderTable({
    
    # test <- read_csv("../1-data/test_data.csv")
    req(input$file)
    
    test <- read_csv(input$file$datapath) %>%
      group_by(credit_score) %>%
      summarise(n = n())
    
    test
  })
  
  output$predictions <- renderTable({
    # dl_model <- h2o.loadModel("../4-model/DeepLearning_model_R_1636734668821_6")
    req(input$file)
    df_test <- h2o.importFile(input$file$datapath)
    p <- h2o.predict(dl_model, df_test)
    
    p %>%
      as_tibble() %>%
      mutate(y = predict) %>%
      select(y) %>%
      rownames_to_column("id") %>%
      head(10)
  })
  
}
shinyApp(ui = ui, server = server)
