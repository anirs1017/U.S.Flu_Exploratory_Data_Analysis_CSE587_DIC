
library(shiny)
library(shinythemes)

trend_type <- list("#All", "#flu, #Vaccine")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Twitter trends in Flu season"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Twitter Keywords"),
                                choices = unique(trend_type),
                                selected = "#All"),
                    
                    htmlOutput(outputId = "desc")
                     
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    imageOutput("CDC_HeatMap"),
                    imageOutput("TwitterheatMap")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$CDC_HeatMap <- renderImage({
    return(list(
      src = "images/CDCHeatMap.png",
      contentType = "image/png",
      alt = "Trends with all keywords"
    ))
  }, deleteFile = FALSE)
  
  # Create scatterplot object the plotOutput function is expecting
  output$TwitterheatMap <- renderImage({
    if (input$type == "#All") {
      return(list(
        src = "images/Rplot.png",
        contentType = "image/png",
        alt = "Trends with all keywords"
      ))
    } else if (input$type == "#flu, #Vaccine") {
      return(list(
        src = "images/Rplot01.png",
        filetype = "image/png",
        alt = "Trends with keywords - 'flu and Vaccine'"
      ))
    }
  }, deleteFile = FALSE)
  
  # Pull in description of trend
  output$desc <- renderUI({
    str1 <- "<strong>Keywords : </strong>"
    if (input$type == "#All") {
      str2 <- "<i>flu, Influenza, influenzas, grippe, Epidemic, Epidemics, Pandemics, contagious, outbreaks, Antiviral, Antivirals, arbidol, orthomyxovirus</i>"
    } else if (input$type == "#flu, #Vaccine") {
      str2 <- "<i>flu, Vaccine</i>"
    }
    HTML(paste(str1, str2, sep = '<br/><br/>'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

