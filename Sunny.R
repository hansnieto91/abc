# Load the shiny package
library(shiny)
library(Rcpp)
library(backports)
install.packages("devtools")
devtools::install_github("rstudio/shiny")
install.packages("shiny")
library(ggplot2)
#install.packages("shinythemes")
#library(shinythemes)
#library(gapminder)
#install.packages("gapminder")
#devtools::install_github("jennybc/gapminder")
#library(gapminder)
library(shiny)


#
library(openxlsx)
wb1<- createWorkbook("xlsx")


#add worksheets
addWorksheet(wb1, "country")
addWorksheet(wb1, "continent")
addWorksheet(wb1, "year")
addWorksheet(wb1, "lifeExp")
addWorksheet(wb1, "pop")
addWorksheet(wb1, "gdpPercap")

#
writeData(wb1, sheet=1, gapminder$country )
writeData(wb1, sheet=2, gapminder$continent)
writeData(wb1, sheet=3, gapminder$year)
writeData(wb1, sheet=4, gapminder$lifeExp)
writeData(wb1, sheet=5, gapminder$pop)
writeData(wb1, sheet=6, gapminder$gdpPercap)

#save workbook
saveWorkbook(wb1,"C:/Users/hnoco/Desktop/gapminder.xlsx", overwrite =TRUE)






setwd("C:/Users/hnoco/Desktop")
gapminder <- read.csv("gapminder.csv",dec=",")

gapminder$year <- sapply(gapminder$year, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gapminder$lifeExp <- sapply(gapminder$lifeExp, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gapminder$pop <- sapply(gapminder$pop, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})
gapminder$gdpPercap <- sapply(gapminder$gdpPercap, function(x) if(is.factor(x)) {as.numeric(as.character(x))} else {x})

#########################################
##########################################
#Alles box


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("color", "Point color",
                   choices = c("blue", "red", "green", "black")),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      # Add a slider selector for years to filter
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
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
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)


#########################################
#########################################
#The previous with colours
devtools::install_github("daattali/colourpicker")
install.packages("colourpicker")
library(shiny)
library(devtools)
library(DT)
library(colourpicker)

install.packages("devtools")
devtools::install_github("daattali/colourpicker")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      colourInput("color", "Point color", value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      # Make the plot 600 pixels wide and 600 pixels tall
      plotOutput("plot", width = 600, height = 600)
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)

#################################################
################################################
#Table

ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  # Add a download button
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Create a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "gapminder_data.csv"
    filename = "gapminder_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      data <- gapminder
      data <- subset(
        data,
        lifeExp >= input$life[1] & lifeExp <= input$life[2]
      )
      if (input$continent != "All") {
        data <- subset(
          data,
          continent == input$continent
        )
      }
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)

##########################################
########################################
####Tensor

# From last exercise
total <- tf$add(female,male)

# Multiply your allemps by growth projections
growth <- tf$constant(1.32, name = "EmpGrowth")
EmpGrowth <- tf$math$multiply(total, growth)
print(EmployeeSession$run(EmpGrowth))

# Write to file
towrite <- tf$summary$FileWriter('./graphs', EmployeeSession$graph)

# Open Tensorboard
tensorboard(log_dir = './graphs')




# Define UI for the application
ui <- fluidPage(
  # "DataCamp" as a primary header
  h1("Data Camp"),
  # "Shiny use cases course" as a secondary header
  h2("Shiny use cases course"),
  # "Shiny" in italics
  em("Shiny"),
  # "is fun" as bold text
  strong("is fun")
)

# Define the server logic
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)

##########
#########
########

# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h4("Plot parameters"),
      textInput("title", "Plot title", "Car speed vs distance to stop"),
      numericInput("num", "Number of cars to show", 30, 1, nrow(cars)),
      sliderInput("size", "Point size", 1, 5, 2, 0.5)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(cars[1:input$num, ], main = input$title, cex = input$size)
  })
  output$table <- renderTable({
    cars[1:input$num, ]
  })
}

# Run the application
shinyApp(ui = ui, server = server)


###### sUNNY PLOT

# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("name", "What is your name?", "Dean"),
      numericInput("num", "Number of flowers to show data for",
                   10, 1, nrow(iris))
    ),
    mainPanel(
      textOutput("greeting"),
      plotOutput("cars_plot"),
      tableOutput("iris_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Create a plot of the "cars" dataset 
  output$cars_plot <- renderPlot({
    plot(cars)
  })
  
  # Render a text greeting as "Hello <name>"
  output$greeting <- renderText({
    paste("Hello", input$name)
  })
  
  # Show a table of the first n rows of the "iris" data
  output$iris_table <- renderTable({
    data <- iris[1:input$num, ]
    data
  })
}

# Run the application
shinyApp(ui = ui, server = server)


################################
####################################
#Best fit


# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      # Add a checkbox for line of best fit
      checkboxInput("fit", "Add line of best fit", FALSE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size) +
      scale_x_log10() +
      #   scale_x_log2() +
      ggtitle(input$title)
    
    # When the "fit" checkbox is checked, add a line
    # of best fit
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)

