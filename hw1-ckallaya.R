library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(htmltools)
library(rsconnect)

# load COVID-19 and unemployment data
rawdata <- read.csv("./rawdata.csv")

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
      
      # Select which county/counties to plot ------------------------
      selectInput(inputId = "selected_county", 
                  label = "Select county/counties:",
                  choices = c("Adams", "Allegheny", "Armstrong", "Beaver", "Bedford", "Berks", "Blair", "Bradford", "Bucks", "Butler","Cambria", "Cameron", "Carbon","Centre","Chester", "Clarion", "Clearfield", "Clinton", "Columbia", "Crawford", "Cumberland", "Dauphin", "Delaware", "Elk", "Erie", "Fayette", "Forest", "Franklin", "Fulton", "Greene", "Huntingdon", "Indiana", "Jefferson","Juniata", "Lackawanna", "Lancaster", "Lawrence",  "Lebanon", "Lehigh", "Luzerne", "Lycoming", "McKean", "Mercer","Mifflin", "Monroe", "Montgomery", "Montour", "Northampton", "Northumberland", "Perry", "Philadelphia", "Pike", "Potter", "Schuylkill", "Snyder", "Somerset", "Sullivan", "Susquehanna", "Tioga", "Union",
                              "Venango", "Warren", "Washington", "Wayne", "Westmoreland", "Wyoming", "York"),
                  multiple = TRUE,
                  selected = c("Adams", "Allegheny")),
      
      selectInput(inputId = "selected_year", 
                  label = "Select year/years:",
                  choices = c(2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1994,1993,1992,1991,1990,1989,1987,1986,1985,1984,1983,1982,1981,1980,1979,1978,1977,1976),
                  multiple = TRUE,
                  selected = c(2021,2020)),
      
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
                              "Year" = "year"),
                  selected = "year"),
      
      # Select variable for label -----------------------------------
      selectInput(inputId = "m", 
                  label = "Label by:",
                  choices = c("County" = "County", 
                              "Region" = "dcedregion",
                              "Month" = "month",
                              "Year" = "year"),
                  selected = "County"),
      
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
                label = "Title: Scatter plot", 
                placeholder = "Enter text to be used as plot title"),
      
      textInput(inputId = "plot_title2", 
                label = "Title: Bar plot", 
                placeholder = "Enter text to be used as plot title"),
      
      textInput(inputId = "plot_title3", 
                label = "Title: Box plot", 
                placeholder = "Enter text to be used as plot title"),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
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
    filter(rawdata, County %in% input$selected_county & year %in% input$selected_year)
  })
  
  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title1 <- reactive({ toTitleCase(input$plot_title1) })
  pretty_plot_title2 <- reactive({ toTitleCase(input$plot_title2) })
  pretty_plot_title3 <- reactive({ toTitleCase(input$plot_title3) })
  
  # Create plot objects, the plotOutput function is expecting --

    output$scatterplot <- renderPlot({
    ggplot(data = rawdata_final(), aes_string(x = input$x, y = input$y, color = input$z, label = input$m)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title2()
      ) + geom_text(size = 3, hjust = 0, nudge_x = 0.1)
  })
    
    output$barplot <- renderPlot({
      ggplot(data = rawdata_final(), aes_string(x = input$x, y = input$y, color = input$z, label = input$m)) +
        geom_bar(stat = "identity",fill="white", width=0.5) +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " ")),
             color = toTitleCase(str_replace_all(input$z, "_", " ")),
             title = pretty_plot_title1()
        ) + geom_text(size = 3, hjust = 0, nudge_x = 0.1)
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