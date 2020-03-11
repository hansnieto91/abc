#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

setwd("C:/Users/hnoco/Desktop")
gap <- read.csv("gap.csv",dec=",")

gap$year <- sapply(gap$year, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gap$VoltageV <- sapply(gap$lifeExp, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gap$pop <- sapply(gap$pop, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gap$times <- sapply(gap$gdpPercap, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})

#########################################
##########################################
#Alles box


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            textInput("title", "Title", "time vs Voltage"),
            numericInput("size", "Point size", 1, 1),
            checkboxInput("fit", "Add line of best fit", FALSE),
            radioButtons("color", "Point color",
                         choices = c("blue","black")),
            selectInput("continents", "Smart Meter",
                        choices = levels(gap$continent),
                        multiple = TRUE,
                        selected = "S2"
            ),
            # Add a slider selector for years to filter
            sliderInput("years", "Time",
                        min(gap$year), max(gap$year),
                        value = c(50, 1002))
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Define the server logic
server <- function(input, output) {
    output$plot <- renderPlot({
        # Subset the gapminder data by the chosen years
        data <- subset(gap,
                       continent %in% input$continents &
                           year >= input$years[1] & year <= input$years[2])
        p <- ggplot(data   , aes(times, VoltageV)) +
            # geom_point(size = input$size )  +
            #   geom_point(size = input$size, col = ifelse(input$continents=="S1",input$color=="black" )+  
            geom_point(size = input$size, col =ifelse (input$continents=="S2","blue","black"))
        
        #   geom_point(size = input$size, col =ifelse (continents=="S2","blue",ifelse(continents=="S1" or "S2",abline(Time, subset(gap$Voltage,gap$continent=="S1"),col="red")
        
        #subset(gap$Voltage,gap$continent=="S2")=="blue", subset(gap$Voltage,gap$continent=="S1")=="red")) 
        
        ggtitle(input$title)
        
        
        
        
        if (input$fit) {
            p <- p + geom_smooth(method = "lm")
        }
        p
    })
}

shinyApp(ui = ui, server = server)

