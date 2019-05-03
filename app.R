library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(data.table)

okc <-fread("profiles.csv", stringsAsFactors = TRUE)


##############ui########################
ui <- 
  
  
  dashboardPage(
    
    
    dashboardHeader(title = "Exploring OKCupid"),
    dashboardSidebar(textInput("text", "Text", value = "enter text here")
      #selectInput("variable", "Variable:",colnames(okc), multiple = TRUE)
      
     # selectInput("sex", "Sex:",levels(okc$sex), selected ="m")
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
      
      dataTableOutput("result")
      #,valueBox(100, "Basic example")
      
    )
  )

######################################server#############
server <- function(input, output) {
 
  myvar <-reactive(input$sex)
  print(myvar)

  output$result <-DT::renderDT({
    return(okc %>%  select(sex, age, location, religion, height, ethnicity, orientation)) %>%filter(if(input$sex  != "Any") sex == input$sex else TRUE) %>%  filter(between(age,input$age[1], input$age[2])) %>%  filter(if(input$religion  != "Any") grepl(input$religion, religion) else TRUE) %>%  filter(if(input$location  != "Any") location == input$location else TRUE) %>% filter(if(input$ethnicity  != "Any") grepl(input$ethnicity, ethnicity) else TRUE) %>% filter(if(input$orientation  != "Any") orientation == input$orientation else TRUE)
    
  })
  
}
shinyApp(ui, server)


##todo: clean up ethnicity and religion drop-downs.

##for religion column,  the easiest thing to do would be to ignore the "seriousness" and just have a drop-down with the religions.
#then a selection of judaism will return all users who mentioned judaism. use grepl(string, column)

#this works:
#okc %>% select(religion) %>% filter(grepl("judaism", religion)) %>% head


#c("asian", "black", "hispanic / latin", "white", "other", "indian", "native american", "pacific islander", "middle eastern")