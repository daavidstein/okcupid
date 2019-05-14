library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(plotly)
library(shinycssloaders)

setwd("~/Desktop/NCF_Data_Science/Spring_2019/Viz/final_project/app/okcupid")
source("wordcloud.R")
okc <-fread("profiles.csv", stringsAsFactors = FALSE)
okc$location <- as.factor(okc$location)
okc$religion <- as.factor(okc$religion)
okc$orientation <- as.factor(okc$orientation)
okc <- okc %>% mutate(smokes =ifelse(smokes == "", "no response",smokes), drugs =ifelse(drugs == "", "no response", drugs))
okc <-okc %>% mutate(sign =case_when(grepl("aries", sign) ~"aries",grepl("aquarius", sign) ~"aquarius"
                                    , grepl("cancer", sign) ~"cancer",grepl("capricorn", sign) ~"capricorn"
                                     ,grepl("leo", sign) ~"leo",grepl("libra", sign) ~"libra"
                                    ,grepl("pisces", sign) ~"pisces",grepl("gemini", sign) ~"gemini"
                                    ,grepl("scorpio", sign) ~"scorpio",grepl("sagittarius", sign) ~"sagittarius"
                                    ,  grepl("taurus", sign) ~"taurs",grepl("virgo", sign) ~"virgo"
                                    , sign == "" ~ "no response"))


##############ui########################
ui <- 
  
  
  dashboardPage(
    
    
    dashboardHeader(title = "Exploring OKCupid"),
    dashboardSidebar(
          sidebarMenu(
                       menuItem("I Never Kiss On The First (Data)", tabName = "Data", icon = icon("table"))
                      , menuItem("Pie (Charts) in the Sky", icon = icon("signal"), tabName = "Pies",
                                badgeColor = "green")
                      , menuItem("We Met At A Bar (Chart)", icon = icon("signal"), tabName = "Bar",
                                 badgeColor = "green")
                      , menuItem("Love Is A 4-Letter Word (Cloud)", icon = icon("cloud"), tabName = "Cloud",
                                 badgeColor = "green")
                     ),
          textInput("text", "Text", value = "enter text here")
          # ,selectInput("essay_choice", "Search Essay:",c("Any", "My self summary", "What I’m doing with my life", "I’m really good at",
          #                                            "The first thing people usually notice about me",
          #                                            "Favorite books, movies, show, music, and food",
          #                                            "The six things I could never do without",
          #                                            "I spend a lot of time thinking about",
          #                                            "On a typical Friday night I am", selected = "My self summary"))
          
          # ,selectInput("essay_choice", "Search Essay:",c( "My self summary","What I’m doing with my life"
          #                                                ))
          #you need to make it so that "My self summary" correspons to essay0, etc
    ,     radioButtons("sex", "Sex:",
                       choiceNames = list(
                         "Female",
                         "Male",
                         "Any"
                       ),
                       choiceValues = list(
                         "f", "m", "Any"
                       ), selected = "Any")
      ,sliderInput("age", "Age:",
                   min = 18, max = 80, value = c(18,80))
      
      ,selectInput("location", "Location:",c("Any",levels(okc$location)), selected = "Any")
    ,selectInput("sign", "Sign:",c("Any",c("Aries" ="aries","Aquarius" = "aquarius", "Cancer" ="cancer", "Capricorn" ="cancer", "Leo" = "leo",
                                           "Libra" = "libra", "Pisces" = "pisces","Gemini" = "gemini", "Scorpio" ="scorpio", "Sagittarius" ="sagittarius", "Taurus" ="taurus","Virgo" ="virgo")), selected = "Any")
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
                )#,
                # fluidRow(
                #   column(3, valueBoxOutput("exp2021", width=NULL) %>% withSpinner(color="lightblue")),
                #   column(3, valueBoxOutput("exp2022", width=NULL) %>% withSpinner(color="lightblue"))
                # )
        ) , #end tabitem
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
                ,
                fluidRow(
                  plotlyOutput("pie4")
                )
                
                ), # end TabItem
        tabItem(tabName = "Cloud",
                h2("What are people talking about?"),
                fluidRow(
                  plotOutput("my_wordcloud")
                )
               
        )#end map tabItem
        
        ,tabItem(tabName = "Bar",
                h2("Grouped Bar Chart"),
                fluidRow(
                  selectInput("cat1", "Select Category 1:", c("sex", "orientation", "smokes", "status"), selected = "sex")
                ,selectInput("cat2", "Select Category 2:", c("sex", "orientation", "smokes", "status"), selected = "orientation")
        
                 
                  
                  ,plotlyOutput("bar")
                  
                  
                  
                )
                
        )#end Bar tabItem
        
        )
      )
      
     
     
      #,valueBox(100, "Basic example")
      
    )
 # )

######################################server#############
server <- function(input, output) {

  
  essay_selector <- function(input){
    
    return(case_when(input == "My self summary" ~"essay0", input == "What I’m doing with my life" ~"essay1"))
    
  }#end essay_selector
  
 
  search_any <-function(search_term, my_df){
    return(grepl(search_term,my_df$essay0)|  grepl(search_term,my_df$essay1) |  grepl(search_term,my_df$essay2) | grepl(search_term,my_df$essay3)
           |  grepl(search_term,my_df$essay4) |  grepl(search_term,my_df$essay5) |  grepl(search_term,my_df$essay6) |  grepl(search_term,my_df$essay7) 
           |  grepl(search_term,my_df$essay8) |  grepl(search_term,my_df$essay9))
    
    
  }#end search_any
  
  search_any("travel", okc)
  output$result <-DT::renderDT({
   # print(input$text)
    okcfilter <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) %>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
    # dt_filter <- dt_filter %>%mutate(essay_selection=case_when(input$essay_choice== "My self summary" ~essay0
    #                                                            ,input$essay_choice== "What I’m doing with my life" ~essay1
    #                                                            , input$essay_choice== "I’m really good at" ~essay2
    #                                                            ,input$essay_choice== "The first thing people usually notice about me" ~essay3
    #                                                            , input$essay_choice== "Favorite books, movies, show, music, and food" ~essay4
    #                                                            ,input$essay_choice== "The six things I could never do without" ~essay5
    #                                                            , input$essay_choice== "I spend a lot of time thinking about" ~essay6
    #                                                            , input$essay_choice== "On a typical Friday night I am" ~essay7
    #                                                            ,input$essay_choice== "The most private thing I am willing to admit" ~essay8
    #                                                            ,input$essay_choice== "You should message me if..." ~essay9)) %>% filter(if(input$text != "enter text here") grepl(input$text, essay_selection) else TRUE)  
    # 
    
    okcfilter <- okcfilter %>% filter(if(input$text != "enter text here") search_any(input$text,dt_filter) else TRUE)
  
    
    return(okcfilter %>%  select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height, sign)) #remove essay_choice column
  }) # end data table
  
 
     output$pie <- renderPlotly({
       okcfilter <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) 
       okcfilter <- okcfilter %>% filter(if(input$text != "enter text here") search_any(input$text,okcfilter) else TRUE)
     
     
       if(nrow(okcfilter) <1 ){return(NULL)} #showmodal inside
          #validate( need(nrow(okcfilter) > 0), "Selection returned no results. Please widen your search criteria")
       #merge the various "seriousness" religion factors to make the pie chart less crowded
       okcfilter <-okcfilter %>% mutate(religion=case_when(grepl("judaism", religion) ~"judaism", grepl("islam", religion) ~"islam", grepl("christianity", religion) ~ "christianity", grepl("catholicism", religion) ~ "catholicism", grepl("agnosticism", religion) ~ "agnosticism", grepl("atheism", religion) ~"atheism", grepl("hinduism", religion) ~"hinduism", grepl("buddhism", religion) ~"buddhism",grepl("other", religion) | "" == religion ~"other/no response" ))%>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
       
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
      # okcfilter2 <-okc %>%  select(sex, age, location, religion, height, ethnicity, orientation, drugs, smokes, status, height) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE)
      # okcfilter2 <- okcfilter2%>% mutate(drugs = case_when(drugs == "" ~ "no response", drugs !="" ~drugs)) #fix 0 factor level
       okcfilter2 <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) %>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
       okcfilter2 <- okcfilter2 %>% filter(if(input$text != "enter text here") search_any(input$text,okcfilter2) else TRUE)
       
       
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
       okcfilter3 <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) 
       okcfilter3 <- okcfilter3 %>% filter(if(input$text != "enter text here") search_any(input$text,okcfilter3) else TRUE)
       
       #merge the various "seriousness" religion factors to make the pie chart less crowded
       okcfilter3 <-okcfilter3 %>% mutate(religion=case_when(grepl("judaism", religion) ~"judaism", grepl("islam", religion) ~"islam", grepl("christianity", religion) ~ "christianity", grepl("catholicism", religion) ~ "catholicism", grepl("agnosticism", religion) ~ "agnosticism", grepl("atheism", religion) ~"atheism", grepl("hinduism", religion) ~"hinduism", grepl("buddhism", religion) ~"buddhism",grepl("other", religion) | "" == religion ~"other/no response" )) %>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
      # print(colnames(okcfilter3))
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
     
     output$pie4 <- renderPlotly({
       okcfilter4 <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) %>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
       okcfilter4 <- okcfilter4 %>% filter(if(input$text != "enter text here") search_any(input$text,okcfilter4) else TRUE)
      
       
       p <- plot_ly() %>%
         add_pie(data = count(okcfilter4, sign), labels = ~sign, values = ~n,
                 name = "Sign", domain = list(row = 2, column = 0)) %>%
         add_pie(data = count(okcfilter4, pets), labels = ~pets, values = ~n,
                 name = "Pets", domain = list(row = 2, column = 1)) %>%
         
         
         layout(title = "Sign & Pets", showlegend = F,
                grid=list(rows=1, columns=2),
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
         
         config( displayModeBar = F) 
       
       return(p)
       
     })#end plotly Pie4
     
    
     
     ########begin grouped bar
   output$bar <-renderPlotly({

     cat1 <- okc %>% select(input$cat1)
     cat2 <-okc %>% select(input$cat2)
     df <- data.frame(cat1,cat2)
     names(df) <-c("cat1", "cat2")
  
          grouped_barr <- function(df){
       my_sum <- df %>% select(cat1, cat2) %>% group_by(cat1,cat2) %>% summarize(n=n())                                                                                    
     
     okcbar<- plot_ly(my_sum, type="bar", x = ~cat1, y =~n, color =~cat2)
    
       return(okcbar)
     }#end grouped barr function
     
     return(grouped_barr(df))
     
   })#end grouped bar
   
   output$my_wordcloud <-renderPlot({
     okcfilter <-okc %>% filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(between(height,input$height[1], input$height[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE) 
     okcfilter <- okcfilter %>% filter(if(input$text != "enter text here") search_any(input$text,okcfilter) else TRUE)%>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)%>% filter(if(input$sign  != "Any") grepl(input$sign, sign) else TRUE)
     
      wc <-make_wc(okcfilter)
     return(wc)
     
   })#end grouped bar
}
shinyApp(ui, server)

#todo: fix the pie chart so that a useful error message is displayed when the search criteria return 0 rows
#todo "invalid cex value" for word cloud. possible solution: add an "action"/submit button and only call the word cloud function (again) when it is pressed.
#if a category contains less than 2%, lump it into another category called "other". you can do this in the data preprocessing stage.
##todo: (only after everything else): allow "seriousness" options for religion, and allow multiple selections of ethnicity.

##DONE TODOs
#todo: fix 0's on pie charts [DONE]
#7 minute presentation


#observeEvent 
#whenever selectinput changes, update choices

