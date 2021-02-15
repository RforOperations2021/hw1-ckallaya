library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(htmltools)

# load COVID-19 and unemployment data
rawdata <- read.csv("D:/Spring_2021/RShiny/HW1/hw1-ckallaya/rawdata.csv")

# rechange the county column name
colnames(rawdata)[1] <- "County"

# rearrange month Jan -> Dec
rawdata <- rawdata %>% mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))

# mutate year to a factor variable
rawdata <- rawdata %>% mutate(year = factor(year))

# change JH_cases to numeric
rawdata$JH_cases <- as.numeric(gsub(",", "",rawdata$JH_cases))
rawdata$Employed <- as.numeric(gsub(",", "",rawdata$Employed))
rawdata$Labor_Force <- as.numeric(gsub(",", "",rawdata$Labor_Force))
rawdata$Unemployed <- as.numeric(gsub(",", "",rawdata$Unemployed))
rawdata$Unemployment_Rate <- as.numeric(gsub("%", "",rawdata$Unemployment_Rate))


# Define UI for application that plots features of rawdata -----------
ui <- fluidPage(
  
  # Theme selector --------------------------------------------------
  shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("simplex"),
  
  # Application title -----------------------------------------------
  titlePanel("Local Area Unemployment Statistics and Covid-19 Data Browser"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Labor Force (Persons)" = "Labor_Force", 
                              "Employed (Persons)" = "Employed",
                              "Unemployed (Persons)" = "Unemployed",
                              "Unemployment Rate (%)" = "Unemployment_Rate",
                              "COVID-19 New Cases" = "JH_cases"), 
                  selected = "JH_cases"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("COVID-19 New Cases" = "JH_cases", 
                              "Month" = "month",
                              "County" = "County"), 
                  selected = "month"),
      
      # Select variable for color -----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("County" = "County", 
                              "Region" = "dcedregion",
                              "Month" = "month",
                              "Year" = "year"
                              ),
                  selected = "year"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.8),
      
      # Set point size ----------------------------------------------
      sliderInput(inputId = "size", 
                  label = "Size:", 
                  min = 0, max = 5, 
                  value = 4),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title1", 
                label = "Title: Plot 1", 
                placeholder = "Enter text to be used as plot title"),
      
      textInput(inputId = "plot_title2", 
                label = "Title: Plot 2", 
                placeholder = "Enter text to be used as plot title"),
      
      textInput(inputId = "plot_title3", 
                label = "Title: Plot 3", 
                placeholder = "Enter text to be used as plot title"),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Select which county/counties to plot ------------------------
      selectInput(inputId = "selected_county", 
                  label = "Select county/counties:",
                  choices = c("Adams", "Allegheny", "Armstrong", "Beaver", "Bedford", "Berks", "Blair", "Bradford", "Bucks", "Butler","Cambria", "Cameron", "Carbon","Centre","Chester", "Clarion", "Clearfield", "Clinton", "Columbia", "Crawford", "Cumberland", "Dauphin", "Delaware", "Elk", "Erie", "Fayette", "Forest", "Franklin", "Fulton", "Greene", "Huntingdon", "Indiana", "Jefferson","Juniata", "Lackawanna", "Lancaster", "Lawrence",  "Lebanon", "Lehigh", "Luzerne", "Lycoming", "McKean", "Mercer","Mifflin", "Monroe", "Montgomery", "Montour", "Northampton", "Northumberland", "Perry", "Philadelphia", "Pike", "Potter", "Schuylkill", "Snyder", "Somerset", "Sullivan", "Susquehanna", "Tioga", "Union",
                              "Venango", "Warren", "Washington", "Wayne", "Westmoreland", "Wyoming", "York"),
                  multiple = TRUE,
                  selected = c("Adams", "Allegheny")),
      
     ),

    # Output: -------------------------------------------------------
    mainPanel(
      
      # Show all the plots --------------------------------------------
      
      plotOutput(outputId = "scatterplot"),
      br(),        
      
      plotOutput(outputId = "barplot"),
      br(),

      plotOutput(outputId = "boxplot"),
      br(),
      
      # Print number of obs plotted ---------------------------------
      uiOutput(outputId = "n"),
      br(), 

      # Show download button ---------------------------------------------
      downloadButton("download1","Download data table as csv"),
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "rawdatatable")
    )
  ),
  
  # Show links to the data sources
  mainPanel(
    HTML(
      paste(
        h5("Data Sources:"),
        a(href = "https//:paworkstats.geosolinc.com", "Local Area Unemployment Statistics (LAUS) - Latest data: December, 2020"),  # can put this in ui or renderui only
        br(),
        a(href = "https//:https://coronavirus.jhu.edu/map.html", "Johns Hopkins' Covid-19 New Cases - Latest data: January, 2021")  # can put this in ui or renderui only
      )
    )
  )
  
)

# Define server function required to create the plots ---------
server <- function(input, output, session) {

  # Create a subset of data filtering for selected counties ------
  rawdata_final <- reactive({ 
    req(input$selected_county) 
    filter(rawdata, County %in% input$selected_county)
  })
  
  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title1 <- reactive({ toTitleCase(input$plot_title1) })
  pretty_plot_title2 <- reactive({ toTitleCase(input$plot_title2) })
  pretty_plot_title3 <- reactive({ toTitleCase(input$plot_title3) })
  
  # Create plot objects, the plotOutput function is expecting --

    output$scatterplot <- renderPlot({
    ggplot(data = rawdata_final(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title2()
      ) 
  })
    
    output$barplot <- renderPlot({
      ggplot(data = rawdata_final(), aes_string(x = input$x, y = input$y, color = input$z)) +
        geom_bar(stat = "identity") +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " ")),
             color = toTitleCase(str_replace_all(input$z, "_", " ")),
             title = pretty_plot_title1()
        ) 
    })
  
  output$boxplot <- renderPlot({
    ggplot(data = rawdata_final(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_boxplot() +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title3()
      ) 
  })

  
  # Print number of data points plotted ----------------------------------
  output$n <- renderUI({
    types <- rawdata_final()$County %>% 
      factor(levels = input$selected_county) 
    counts <- table(types)
    
    HTML(paste("There are", counts, "data points for ", input$selected_county, " county in this dataset. <br>"))
  })

  # Print data table if checked -------------------------------------
  output$rawdatatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = rawdata_final(),
                    callback = JS("$('div.dwnld').append($('#download1'));"),
                    extensions = 'Buttons',
                    options = list(pageLength = 10, dom = 'B<"dwnld">frtip',
                                   buttons = list(
                                     "copy"
                                   )), 
                    rownames = FALSE)
    }
  )
  
  # Download data if button is clicked -------------------------------------
  output$download1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rawdata_final(), file)
    }
  )
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)