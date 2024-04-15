library(shiny)
library(bslib)
library(tidyverse)
library(ggridges)
library(ggplot2)

compilation = read.csv("compilation.csv")

# Define UI ----

ui <- page_navbar(
  title = "HO Review from M18 to M23",
  bg = "#2D89C8",
  inverse = T,
  fillable = F, #allow scroll instead of fill 
  
  #First tab -- General impression tab
  nav_panel(
    title = "General Impression", 
    
    navset_card_underline(
      title = "General impression",
      
      #Sidebar options and formatting
      sidebar = sidebar(
        helpText(
          "General rule of thumb: the more data points the better - in order to
            avoid inadequate data for misleading interpretation, try not to narrow down 
            too much into a specific cohort + rotation AND pay attention to the actual
            number of entries shown in the boxplot."
        ),
        
        #Parameter selection
        selectInput(
          inputId = "parameter", #your variable in the server part
          label = "Parameter",
          choices = list("Overall","Workload", "Stress","Independence","Education",
                         "Support","Daytime_workload","Call_workload"),
          selected = "Overall"
        ),
        
        #Cohort selection
        selectInput(
          inputId = "cohort",
          label = "Cohort",
          choices = list("All cohorts","M18","M19","M20","M21","M22","M23"),
          selected = "All cohorts"
        ),
        
        #Rotation selection
        selectInput(
          inputId = "rotation",
          label = "Rotation",
          choices = list("All rotations","R1" = 1,"R2" = 2,"R3" = 3,"R4" = 4),
          selected = "All rotations"
        )),
      
      #Medicine
      nav_panel("MED", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("med_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("med_ridgeplot"))),
      
      #Surgery
      nav_panel("SUR", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("sur_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("sur_ridgeplot"))),
      
      #Paediatrics
      nav_panel("PAED", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("ped_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("ped_ridgeplot"))),
      
      #Ortho
      nav_panel("ORTHO", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("ortho_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("ortho_ridgeplot"))),
      
      #OG
      nav_panel("OG", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("og_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("og_ridgeplot"))),
      #Bundle
      nav_panel("Bundle", 
                card(card_header("Boxplot"),
                     helpText("Actual number of entries from the HO review dataset"),
                     #full_screen = T, #allows the card to expand into full screen
                     plotOutput("bundle_boxplot")),
                card(card_header("Ridgeplot"),
                     helpText("Approximation of the distribution of scores for
                              more intuitive visualization (but PLEASE NOTE that it
                              is only an estimation as you will see values extend to
                              > 5 in the plot which are NOT actually present in the dataset"),
                     plotOutput("bundle_ridgeplot")))
    )
  ),
  
  #Second tab -- Workload breakdown
  nav_panel(
    title = "Workload Breakdown", 
    
    nav_panel(
      title = "Workload Breakdown",
      
      #Sidebar options and formatting
      layout_sidebar(
        sidebar = sidebar(
          #Specialty selection
          selectInput(
            inputId = "specialty",
            label = "Specialty",
            choices = list("MED","SUR","PAED","OG","ORT"),
            selected = "MED"
          ),
          
          #Cohort selection
          selectInput(
            inputId = "cohort",
            label = "Cohort",
            choices = list("All cohorts","M18","M19","M20","M21","M22","M23"),
            selected = "All cohorts"
          ),
          
          #Rotation selection
          selectInput(
            inputId = "rotation",
            label = "Rotation",
            choices = list("All rotations","R1" = 1,"R2" = 2,"R3" = 3,"R4" = 4),
            selected = "All rotations"
          )),
        
        card(card_header("Lone call"),
             plotOutput("lone")),
        card(card_header("Post call half day"),
             plotOutput("postcall")),
        card(card_header("Long short call"),
             plotOutput("longshort")),
        card(card_header("Call room available"),
             plotOutput("callroom"))
        #Daytime, Call, the next 5 columns
      )
      
      
    )
  ),
  
  #Third tab --  Leave arrangement
  nav_panel(
    title = "Leave Arrangement", 
    
    nav_panel(
      title = "Leave Arrangement",
      
      #Sidebar options and formatting
      layout_sidebar(
        sidebar = sidebar(
          #Specialty selection
          selectInput(
            inputId = "specialty",
            label = "Specialty",
            choices = list("MED","SUR","PAED","OG","ORT"),
            selected = "MED"
          ),
          
          #Cohort selection
          selectInput(
            inputId = "cohort",
            label = "Cohort",
            choices = list("All cohorts","M18","M19","M20","M21","M22","M23"),
            selected = "All cohorts"
          ),
          
          #Rotation selection
          selectInput(
            inputId = "rotation",
            label = "Rotation",
            choices = list("All rotations","R1" = 1,"R2" = 2,"R3" = 3,"R4" = 4),
            selected = "All rotations"
          )),
        
        card(card_header("Annual leave"),
             plotOutput("al")),
        card(card_header("Compensatory leave"),
             plotOutput("compleave")),
        card(card_header("Interview"),
             plotOutput("interview")),
      )
      
      
    )
  ),
  
  #Fourth tab
  nav_panel(
    title = "Comments",
    
    #Sidebar options and formatting
    layout_sidebar(
      sidebar = sidebar(
        #Specialty selection
        selectInput(
          inputId = "specialty",
          label = "Specialty",
          choices = list("MED","SUR","PAED","OG","ORT"),
          selected = "MED"
        ),
        
        #Hospital selection
        selectInput(
          inputId = "hospital",
          label = "Hospital",
          choices = list("ANNH","CMC","KWH","NDH","PMH","POH","PWH","PYNEH","QEH","QMH",
                         "RH","TKOH","TMH","UCH","YCH"),
          selected = "QMH"
        )),
      
      card(card_header("Comments"),
           helpText("Arranged in order of rotation and then descending overall score"),
           tableOutput("comments"))
    )
    
  ),
  
  
  #Fifth tab
  nav_panel(
    title = "General Duties", 
    p("To be updated.")
  )
)
