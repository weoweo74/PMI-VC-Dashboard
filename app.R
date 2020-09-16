## Add all the packages required 
install.packages("shiny")
install.packages("shinydashboard")
install.packages("magrittr")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyr")
install.packages("shinydashboardPlus")
install.packages("DT")
install.packages("wordcloud")
install.packages("rAmCharts")
install.packages("plyr")
install.packages("dplyr")
install.packages("sp")
install.packages("pipeR")
install.packages("ggplot2")
install.packages("BROOM")
install.packages("kableExtra")
install.packages("plotly")
install.packages("shinyjs")

library(shiny)
library(shinydashboard)
library(magrittr)
library(readxl)
library(highcharter)
require(tidyr)
library(shinydashboardPlus)
library(DT)
library(wordcloud)
library(rAmCharts)
library(plyr)
library(dplyr)
library(pipeR)
library(ggplot2)
library(broom)
library(kableExtra)
library(plotly)
library(shinyjs)


# Load dataset required for the analysis
data_gbar1<-read.csv("C:\\VC_Indicator\\Sept_10\\data_gbar.csv")



#data_gbar1$month <- as.character(
 # format(
  #  as.Date(paste0("01/",data_gbar1$month), "%d/%m/%Y"), 
   # "%B %Y"))

# Define UI for application

ui <- dashboardPage( skin = "blue", dashboardHeader(title= "Vector Control" , dropdownMenu()),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Info", tabName = "menu_1"),         
      menuItem("By District", tabName = "sub_2"),
      
               selectInput("v_genre", "Select Uganda District ", choices = data_gbar1 %>%
                             select(Genre)%>%
                             distinct()%>%
                             arrange(Genre)%>%
                             drop_na()
                           )
      )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "menu_1", 
              box(
                width = 7, status = "danger", solidHeader = TRUE,
                h1((icon("info")), "VECTOR CONTROL MONITORING", hr()),
                # VC INDICATOR TITLE
                h2("IRS timing selection based on rainfall season and malaria cases "), hr(),
                
                div(
                  "",
                  p("Dashboard provides analytical trend of IRS, ITN and residual efficacy of treated net in relaton to seasonal rainfall and spike in malaria cases,"),
                  p(strong(""))
                ),
                
                # Using the app
                h2(icon("users"), "Key Questions Addressed"), hr(), 
                
                div(
                  "How can we determine the optimal period for IRS campaign activity guided by residual efficacy and seasonal rainfall data ? ",
                  #tags$ul(
                  #  tags$li("Navigate across different tabs by clicking on the options in the left-hand black vertical box."),
                  #  tags$li("The ", strong("title"), " tab generic information.")
                  # )
                ),
                
                h2(icon(""), ""), hr(),
                div(
                  "", 
                  a(),
                  "", a()
                ), hr()
                
              ), #box
              
              box(
                width = 5, status = "danger", solidHeader = TRUE,
                
                # Data Sources
                h2(icon("database"), "Data Sources"), hr(),
                
                div(
                  "This Dashboard analytics uses data from: ", br(),
                  tags$ul(
                    tags$li("VectorLink Collect-DHIS2 "),
                    tags$li("PMI Quarterly Report in MDIVE"),
                    tags$li("National Malaria Control Report")
                  )
                ), hr(),
                #  Indicators Covered from Country and Admin-level Perspective
                h2(icon("cogs"), "Key Indicators Covered"), hr(),
                
                div(
                  " Visualizing Indicators on : ", br(),
                  tags$ul(
                    tags$li("Mosquito mortality / Residual Efficacy "),
                    tags$li("Cases of malaria by mosquito mortality of ITN use "),
                    tags$li("Seasonal rainfall by mosquito mortality "),
                    tags$li("IRS Spray-Pregnant women population protected"),
                    tags$li("Population protected based on IRS and ITN used"),
                    tags$li("Insecticided used and its residual efficacy"),
                    tags$li("IRS and ITS timing tracker")
                  )
                ), hr()
                
              )
      ),  ### end of tabname dashboard info
      
      
      ##### Plots   ### Use the syntax below to begin with TabBox and embedd all the width, height and
      ##### wrap the tabPanel inside in other to see Tab per indicator################################
      
      tabItem(tabName = "sub_2",   
              fluidRow(
          
          
          tabBox(
            width = 11,
            height = "47vh",
            tabPanel(amChartsOutput("top_imdb_rainfall"), title = " Mosquito mortality by rainfall season"),
            tabPanel(amChartsOutput("top_imdb_malaria_cases"), title = "Mosquito mortality by malaria cases")
            )),
          
       ## This is where the output is rendered when from the function of the server logic
      
                h1(" ")
              
              )
      )
    )
  )


# Define server logic
# using this output function to render in tabitem in UI side inside fluidRow/Box


server <- function(input, output)
  
  {
  
  output$top_imdb_rainfall <- renderAmCharts({
    #data_select_genre%>%
    data_gbar1 %>%
      
    #select(data_gbar1$Genre,data_gbar1$month, data_gbar1$mosquito_mortality,data_gbar1$cases)%>%
    filter(Genre== input$v_genre)%>%
    group_by(month)
    #drop_na()
 
      
    
    # Ploting the graphs using months as the x-axis with armchart packages    
    pipeR::pipeline(
      amBarplot(x = "month", y = c("mosquito_mortality", "rainfall"), data = data_gbar1,labelRotation = 0,
                 dataDateFormat = "MM/YYYY", minPeriod = "MM"),
     amOptions(main="", legend = TRUE,legendPosition ="bottom",
     show_values = FALSE, labelRotation = 0, depth = 0.1, creditsPosition = "top-left"),
      setChartCursor()
    ) 
    
    # using this output function to render in tabitem in UI side inside fluidRow/Box 
  })
  output$top_imdb_malaria_cases<- renderAmCharts({
    data_gbar1 %>%
      filter(Genre== input$v_genre)%>%
      group_by(month)
      #drop_na()
      
      
    # Ploting the graphs using months as the x-axis with armchart packages  
    pipeR::pipeline(
      amBarplot(x = "month", y = c("mosquito_mortality", "cases"), data = data_gbar1,labelRotation = 0,
                dataDateFormat = "MM/YYYY", minPeriod = "MM"),
      amOptions(main="", legend = TRUE,legendPosition ="bottom",
                show_values = FALSE, labelRotation = 0, depth = 0.1, creditsPosition = "top-left"),
      setChartCursor()
    )
  })
      
}

# Run the application 
shinyApp(ui = ui, server = server)