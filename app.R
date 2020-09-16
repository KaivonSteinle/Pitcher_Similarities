#Import packages
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(magrittr)
library(shiny)
library(rsconnect)
library(methods)
library(devtools)
library(datasets)
library(broom)
library(stats)
library(stringr)
library(tibble)
library(tidyr)
library(caret)
library(pROC)
library(mlbench)
library(lubridate)
library(readxl)
library(ggridges)
library(shiny)
library(shinyauthr)
library(shinyjs)
library(ggExtra)
library(fmsb)
library(plotly)
library(knitr)
library(shinyWidgets)
library(gridExtra)
library(shinyjqui)
library(highcharter)
library(plotly)
library(animation)
library(magrittr)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(purrr)
library("nleqslv")
kable <- knitr::kable

#Import Data
Pitches <- read_excel("Pitches.xlsx", col_types = c("numeric", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "text", "text", "text", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "text", 
                                                    "numeric", "numeric", "text", "text"))
Pitches <- Pitches %>% filter(!is.na(Rel_X) & !is.na(Rel_Z) & !is.na(Ext))
Pitches$Hor_Break <- 12 * Pitches$Hor_Break
Pitches$Ver_Break <- 12 * Pitches$Ver_Break
#Make a non FB Data Set
Non_FB <- Pitches %>% filter(Pitch_Type_2 == "CB" | Pitch_Type_2 == "CH" | Pitch_Type_2 == "SL")




#Begin creating the ui
ui <- fluidPage(
  titlePanel(h1("Pitcher Similarities", align = "center")),
  fluidRow(
    column(4, actionButton("go", "Apply Filters"))
    ),
  fluidRow(column(11, offset = 1, br(strong("Enter Release Profile:", align = "center")))),
  br(),
  fluidRow(column(2, offset = 1, numericInput("RelH", "Release Height (ft)", 6.0)),
           column(2, numericInput("RelS", "Release Side (ft)", 2.0)),
           column(2, numericInput("Ext", "Extension (ft)", 6.15)),       
           column(2, numericInput("Height", "Height (in)", 73)),
           column(2, textInput("Throws", "Handedness", "R"))),
  fluidRow(column(3, br(strong("Enter Fastball Characteristics:", align = "center"))),
           column(3, offset = 3, br(strong("Enter Filters:", align = "center")))),
  br(),
  fluidRow(column(2, numericInput("Velo", "Velocity", 93)),       
           column(2, numericInput("Spin", "Spin Rate", 2240)),
           column(2, numericInput("H_Break", "Horizontal Break (in)", 20)),
           column(2, numericInput("V_Break", "Vertical Break (in)", 20)),
           column(2, selectInput(inputId = "Type", label = "Pitch Type", choices = sort(unique(Non_FB$Pitch_Type_2)), selected = sort(unique(Non_FB$Pitch_Type_2)), multiple = TRUE)),
           column(1, numericInput("SpinRange", "Min Spin", 1200)),
           column(1, numericInput("SpinRange2", "Max Spin", 3200))),
  br(strong("Best Breaking Ball Comps", style = "font-size: 12pt", align = "center")),
  fluidRow(
    column(12, DT::dataTableOutput("BB_Comps"))),
  br(),
  fluidRow(
    column(5, offset = 4, plotlyOutput("Comps_Graph"))),
  br(),
  br()
)



#Server where we calculate everything
server <- (function(input, output) {
  
  
  #Get the data frame
  Data <- eventReactive(input$go, {
    #Create data frame of just the player
    New_Player <- data.frame(000000, "Name", 2020, 100, input$RelH, input$RelS, 1, input$Ext, 1, input$Velo, input$H_Break, 1, input$V_Break, input$Spin, 1, 1, 1, 1, 1, 1, 1, "A", "Fastball", "A", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "kaivon01", 1, input$Height, "R", input$Throws)      
    #Naming the Data Frame
    colnames(New_Player) <- colnames(Pitches)
    #Using rbind() function to insert above observation  
    Pitches <- rbind(New_Player, Pitches)
    Pitches <- as.data.frame(Pitches)
  } )
  
  
  #Create reactive variable called FB Similarities
  Similarities <- eventReactive(input$go, {
    #Filter for Fastballs
    DataFB <- Data()
    DataFB <- DataFB %>% filter(throws == input$Throws)
    DataFB <- DataFB %>% filter(Pitch_Type_2 == "Fastball")
    FB <- Data() %>% filter(Pitch_Type_2 == "Fastball")
    FB <- FB %>% filter(throws == input$Throws)
    #Adjust the weight next to sd(variable) to open and close windows; bigger the number the wider the range
    height_low <- input$Height - 2*sd(FB$height)
    height_high <- input$Height + 2*sd(FB$height)
    
    rs_low <- input$RelS - 2*sd(FB$Rel_X)
    rs_high <- input$RelS + 2*sd(FB$Rel_X)
    
    rh_low <- input$RelH - 1.5*sd(FB$Rel_Z)
    rh_high <- input$RelH + 1.5*sd(FB$Rel_Z)
    
    ex_low <- input$Ext - 2*sd(FB$Ext)
    ex_high <- input$Ext + 2*sd(FB$Ext)
    
    #Take pitchers with releases in these parameters
    rel_comp <- subset(DataFB, 
                       DataFB$height > height_low & 
                         DataFB$height < height_high &
                         DataFB$Rel_X > rs_low &
                         DataFB$Rel_X < rs_high &
                         DataFB$Rel_Z > rh_low &
                         DataFB$Rel_Z < rh_high &
                         DataFB$Ext > ex_low &
                         DataFB$Ext < ex_high)
    rel_comp <- as.data.frame(rel_comp)
    
    
    #FB Similarity:
    RL_Top_50 <- rel_comp 
    #Getting Inputs
    DataFB_2 <- RL_Top_50[, c("Mean_Velo", "Spin")]
    #Scale and center data
    DataFB_2 <- scale(DataFB_2, center = TRUE)
    #Find the distance for every player to every other one
    DataFB_2 <- dist(DataFB_2, method = "euclidean")
    DataFB_2 <- as.matrix(DataFB_2)
    #Take the first column, becuase we want the first player's similarity
    DataFB_2 <- DataFB_2[,1]
    #Add that similarity to data frame as a column
    RL_Top_50$FB_Similarity <- DataFB_2
    RL_Top_50 <- RL_Top_50 %>% arrange(FB_Similarity)
    #Order players by Release similarity (50 is a arbitrary number)
    FB_Top_50 <- head(RL_Top_50, 50)
    FB_Top_50 <- as.data.frame(FB_Top_50)
    
    
    #Get the final data frame of players we want to return as options
    #Filter fastball top 50 for just these 2 columns on a player
    FB_Top_50 <- FB_Top_50[, c('Name', 'Year')]
    #Create a data frame of just breaking balls
    Breaking_Balls <- Data()
    #Filter Breaking balls for min and max spin, pitch types and handedness
    Breaking_Balls <- Breaking_Balls %>% filter(throws == input$Throws)
    Breaking_Balls <- Breaking_Balls %>% filter(Spin >= input$SpinRange & Spin <= input$SpinRange2)
    Breaking_Balls <- Breaking_Balls %>% filter(Pitch_Type_2 %in% input$Type)
    #Left join the data. So take all the names from fastball top 50 and also those in breaking balls.
    Rejoined <- inner_join(FB_Top_50, Breaking_Balls, by = c('Name', 'Year'))
    Rejoined <- unique(Rejoined)
    #Remove rows where there maybe isn't data
    Rejoined <- Rejoined %>% filter(!is.na(height)) 
    #Order by grade and return let's say top 20
    Rejoined <- Rejoined %>% arrange(desc(SM))
    Rejoined <- head(Rejoined, 20)
    
    #Taking certain columns
    Comps <- Rejoined
    Comps <- Comps[, c('Name', "Year", "Pitch_Type_2", "Pitch_Count", "Mean_Velo", "Spin", "Hor_Break", "Ver_Break", "SM", "CSW", "EV", "xwOBA", "xFIP", "SIERA")]
    Comps <- as.data.frame(Comps)
  })
  
  #Find Comps
  Comps <- eventReactive(input$go, {
    dt <- Similarities()
    #Round Numbers
    dt$SM <- round(dt$SM * 100, 0)
    dt$CSW <- round(dt$CSW * 100, 0)
    dt$EV <- round(dt$EV, 0)
    dt$xwOBA <- round(dt$xwOBA, 3)
    dt$Mean_Velo <- round(dt$Mean_Velo, 1)
    dt$Spin <- round(dt$Spin, 0)
    dt$Ver_Break <- round(dt$Ver_Break, 1)
    dt$Hor_Break <- round(dt$Hor_Break, 1)
    #Rename Columns
    colnames(dt) <- c('Name', "Year", "Type", "N", "Velocity", "Spin", "Horizontal Break", "Vertical Break", "SwStr%", "CSW%", "EV", "xwOBA", "xFIP", "SIERA")
    #Order by Grade again to be safe
    dt <- dt %>% arrange(desc(`SwStr%`))
    dt <- as.data.frame(dt)
  })
  
  #Now make that data frame presentable
  output$BB_Comps <- DT::renderDataTable({
    dt <- Comps()
    #Present with numbers centered, 10 chosen by default and also choices
    datatable(dt,  options = list(
      columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))),
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20)))
  })
  
  #Make the graph a reactive ggplot graph
  Graph <- eventReactive(input$go, {
    k <- Comps()
    fig <- ggplot(k, aes(x = `Horizontal Break`, y = `Vertical Break`, colour = `SwStr%`, size = 1.75))  + geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black") + geom_point(aes(text=map(paste('<b>Name:</b>', Name, '<br>', '<b>Year:</b>', Year, '<br>', '<b>Type:</b>', Type, '<br>', '<b>SwStr%:</b>', `SwStr%`), HTML))) + ggtitle("Secondary Pitch Movement for Fastball Comps") + labs(y = "Vertical Break (in)", x = "Horizontal Break (in)") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"))  + scale_color_gradient("SwStr%",low="grey", high="red", space ="Lab") + theme(legend.title = element_text(face = "bold")) + scale_x_continuous(breaks=seq(-30, 30, 10), limits=c(-30, 30)) + scale_y_continuous(breaks=seq(-30, 30, 10), limits=c(-30, 30))
    fig <- ggplotly(fig, tooltip="text") %>% layout(height = 550, width = 600)
    fig
  })
  
  #Output a plotly graph
  output$Comps_Graph <- renderPlotly(
    Graph()
  )
  
  
  
})



## Run the application 
shinyApp(ui = ui, server = server)
## End of the app
