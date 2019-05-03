library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(plotly)
library(shinycssloaders)
okc <-fread("profiles.csv", stringsAsFactors = TRUE)


##############ui########################
ui <- 
  
  
  dashboardPage(
    
    
    dashboardHeader(title = "Exploring OKCupid"),
    dashboardSidebar(
          sidebarMenu(
                       menuItem("Data", tabName = "Data", icon = icon("table")),
                       menuItem("Pies", icon = icon("signal"), tabName = "Pies",
                                badgeLabel = "new", badgeColor = "green")
                     ),
          textInput("text", "Text", value = "enter text here")
    ,     radioButtons("sex", "Sex:",
                       choiceNames = list(
                         "Female",
                         "Male",
                         "Any"
                       ),
                       choiceValues = list(
                         "f", "m", "Any"
                       ))
      ,sliderInput("age", "Age:",
                   min = 18, max = 80, value = c(18,80))
      
      ,selectInput("location", "Location:",c("Any",levels(okc$location)), selected = "Any")
    
    ,selectInput("religion", "Religion:",c("Any", levels(okc$religion)[!grepl("serious",levels(okc$religion)) & !grepl("laughing",levels(okc$religion)) & levels(okc$religion) != "" ]
), selected = "Any") #don't include the "seriousness" as an option in religion
      ,sliderInput("height", "Height:",
                   min = 40, max = 85, value = c(40,85)
      )
      ,selectInput("ethnicity", "Ethnicity:",c("Any",
                                               c("asian", "black", "hispanic / latin", "white", "other", "indian", "native american", "pacific islander", "middle eastern")), selected = "Any")
      ,selectInput("orientation", "Orientation:",c("Any",levels(okc$orientation)), selected = "Any")
      
    ),
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "Data",
                h2("Raw Data"),
                fluidRow(
                  dataTableOutput("result")
                ),
                fluidRow(
                  column(3, valueBoxOutput("exp2021", width=NULL) %>% withSpinner(color="lightblue")),
                  column(3, valueBoxOutput("exp2022", width=NULL) %>% withSpinner(color="lightblue"))
                )
        ),
        tabItem(tabName = "Pies",
                h2("Overview of Data"),
                fluidRow(
                  plotlyOutput("pie")
                  )
                ,
                fluidRow(
                 plotlyOutput("pie2")
                )
                ,
                fluidRow(
                  plotlyOutput("pie3")
                )
                ) # end tabItem
        )
      )
      
     
     
      #,valueBox(100, "Basic example")
      
    )
 # )

######################################server#############
server <- function(input, output) {

 
  
  output$result <-DT::renderDT({
    return(okc %>%   select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE))
 
  }) # end data table
  
  #validate( need(nrow(okc) > 0) "Selection returned no results. Please widen your search criteria")
     output$pie <- renderPlotly({
       okcfilter <-okc %>%  select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE)
       
       #merge the various "seriousness" religion factors to make the pie chart less crowded
       okcfilter <-okcfilter %>% mutate(religion=case_when(grepl("judaism", religion) ~"judaism", grepl("islam", religion) ~"islam", grepl("christianity", religion) ~ "christianity", grepl("catholicism", religion) ~ "catholicism", grepl("agnosticism", religion) ~ "agnosticism", grepl("atheism", religion) ~"atheism", grepl("hinduism", religion) ~"hinduism", grepl("buddhism", religion) ~"buddhism",grepl("other", religion) | "" == religion ~"other/no response" ))
       
    #   okcfilter <-okfilter %>% 
        p <- plot_ly() %>%
         add_pie(data = count(okcfilter, sex), labels = ~sex, values = ~n,
                 name = "Sex", domain = list(row = 0, column = 0)) %>% 
         add_pie(data = count(okcfilter, religion), labels = ~religion, values = ~n,
                 name = "Religion", domain = list(row = 0, column = 1)) %>%

      
       
         layout(title = "Sex & Religion", showlegend = F,
                grid=list(rows=1, columns=2),
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
        
        config( displayModeBar = F) 
       
      
       return(p)
    
  })#end plotly Pie
     
     output$pie2 <- renderPlotly({
       okcfilter2 <-okc %>%  select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE)
       
       p <- plot_ly() %>%
         add_pie(data = count(okcfilter2, drugs), labels = ~drugs, values = ~n,
                 name = "Drugs", domain = list(row = 2, column = 0)) %>%
         add_pie(data = count(okcfilter2, smokes), labels = ~smokes, values = ~n,
                 name = "Smokes", domain = list(row = 2, column = 1)) %>%


         layout(title = "Drugs & Smoking", showlegend = F,
                grid=list(rows=1, columns=2),
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
         
         config( displayModeBar = F) 

       return(p)

     })#end plotly Pie2
    
     
     output$pie3 <- renderPlotly({
       okcfilter3 <-okc %>%  select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE)
       
       #merge the various "seriousness" religion factors to make the pie chart less crowded
       okcfilter3 <-okcfilter3 %>% mutate(religion=case_when(grepl("judaism", religion) ~"judaism", grepl("islam", religion) ~"islam", grepl("christianity", religion) ~ "christianity", grepl("catholicism", religion) ~ "catholicism", grepl("agnosticism", religion) ~ "agnosticism", grepl("atheism", religion) ~"atheism", grepl("hinduism", religion) ~"hinduism", grepl("buddhism", religion) ~"buddhism",grepl("other", religion) | "" == religion ~"other/no response" ))
       
       #   okcfilter <-okfilter %>% 
       p <- plot_ly() %>%
         
         add_pie(data = count(okcfilter3, status), labels = ~status, values = ~n,
                 name = "Status", domain = list(row = 1, column = 0)) %>%
         add_pie(data = count(okcfilter3, orientation), labels = ~orientation, values = ~n,
                 name = "Orientation", domain = list(row = 1, column = 1)) %>%
         
         layout(title = "Relationship Status & Sexual Orientation", showlegend = F,
                grid=list(rows=1, columns=2),
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
         
         config( displayModeBar = F) 
       
       return(p)
       
     })#end plotly Pie3
}
shinyApp(ui, server)
#todo: fix the pie chart so that a useful error message is displayed when the search criteria return 0 rows
#to fix the issue of too many of these "lines" with percents, let's actually fix the issue of the fact that there are too many tiny categories
#if a category contains less than 2%, lump it into another category called "other". you can do this in the data preprocessing stage.
##todo: (only after everything else): allow "seriousness" options for religion, and allow multiple selections of ethnicity.