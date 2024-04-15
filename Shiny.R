library(shiny)
library(bslib)
library(shinythemes)
library(tidyverse)
library(ggridges)
library(ggplot2)

compilation = read.csv("D:/Careers/HO Review/Web app/compilation.csv")

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



# Define server logic ----
server <- function(input, output) {
  
  output$parameter_name = renderText({
    paste(input$parameter)
  })
  
  test_df = reactive({ #must put this wrapper otherwise can't access reactive values
    
    if(input$cohort == "All cohorts"){
      test_df = compilation
    } else if (input$cohort != "All cohorts") {
      test_df = compilation %>%
        filter(class == input$cohort)
    }
    
    if(input$rotation == "All rotations"){
      test_df = test_df
    } else if (input$rotation != "All rotations"){
      test_df = test_df %>%
        filter(Rotation == input$rotation)
    }
    
  })

  output$med_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(Specialty == "MED") %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$sur_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(Specialty == "SUR") %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$ped_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(str_detect(Specialty,"PAED")) %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$ortho_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(Specialty == "ORT") %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$og_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(Specialty == "OG") %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$bundle_ridgeplot = renderPlot({
    test_df() %>% #must add () next to the reactive expression "object"
      filter(str_detect(Specialty,"\\-")) %>%
      #ggplot(aes(y = ro, fill = ..x..)) + aes_string(x = input$parameter) +
      ggplot(aes(x = get(input$parameter), y = ro, fill = ..x..)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1),limits = c(0,7)) + 
      geom_density_ridges_gradient(rel_min_height = 0.05,scale=2) + 
      theme_ridges() + theme(legend.position="none",
                             axis.title = element_blank()) +
      scale_fill_gradientn(colours = c("red","yellow","green"))
  })
  
  output$med_boxplot = renderPlot({
    test_df() %>%
      filter(Specialty == "MED") %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$sur_boxplot = renderPlot({
    test_df() %>%
      filter(Specialty == "SUR") %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$ortho_boxplot = renderPlot({
    test_df() %>%
      filter(Specialty == "ORT") %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$ped_boxplot = renderPlot({
    test_df() %>%
      filter(str_detect(Specialty,"PAED")) %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$og_boxplot = renderPlot({
    test_df() %>%
      filter(Specialty == "OG") %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$bundle_boxplot = renderPlot({
    test_df() %>%
      filter(str_detect(Specialty,"\\-")) %>%
      ggplot(aes(y = get(input$parameter), x = ro)) + 
      #geom_boxplot(outlier.shape = NA) +
      geom_violin() + theme_minimal() +
      geom_point(aes(color = Specialty),size=1.75,
                 position = position_jitterdodge(jitter.width = 0.5,
                                                 jitter.height = 0.1)) +
      ggtitle(paste(input$parameter)) +
      theme(axis.text.x = element_text(size=11,angle=90),
            axis.title = element_blank(),
            legend.position = "none")
  })
  
  output$lone = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Lone_call)) %>%
      ggplot(aes(x = Lone_call,fill = Lone_call)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("No","Yes"),
                        values = c("#ca0000","#4d9116")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$postcall = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Post_call_half)) %>%
      ggplot(aes(x = Post_call_half,fill = Post_call_half)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("No","Yes"),
                        values = c("#ca0000","#4d9116")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
 
  output$longshort = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Long_short_call)) %>%
      ggplot(aes(x = Long_short_call,fill = Long_short_call)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("No","Yes"),
                        values = c("#ca0000","#4d9116")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$callroom = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Call_room)) %>%
      ggplot(aes(x = Call_room,fill = Call_room)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("No","Yes"),
                        values = c("#ca0000","#4d9116")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$al = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(AL)) %>%
      ggplot(aes(x = AL,fill = AL)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("Allow arrangement among HO","Assigned by Department"),
                        values = c("#2297e6","#f5c710")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$compleave = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Compensatory_leave)) %>%
      ggplot(aes(x = Compensatory_leave,fill = Compensatory_leave)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("Allow arrangement among HO","Assigned by Department"),
                        values = c("#2297e6","#f5c710")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$interview = renderPlot({
    test_df() %>%
      filter(Specialty == input$specialty) %>%
      filter(!is.na(Interview)) %>%
      ggplot(aes(x = Interview,fill = Interview)) + 
      geom_bar() +  theme_minimal() + ylab("Count") +
      facet_wrap(~ ro,strip.position = "bottom") +
      scale_fill_manual(labels = c("Allow arrangement among HO","Need to take official leave"),
                        values = c("#2297e6","#f5c710")) + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(-10,0,0,0),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_line(size = .5),
            axis.line.x = element_line(size = .5),
            strip.placement = "outside")
  })
  
  output$comments = renderTable({
    compilation %>% 
      filter(Specialty == input$specialty) %>%
      filter(Hospital == input$hospital) %>%
      select(Hospital,Specialty,Rotation,Overall,class,Comments1,Comments2) %>%
      arrange(Rotation,desc(Overall),desc(class))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)





