library(shiny)
library(plyr)
library(tidyverse)
library(stringr)
library(emojifont)
library(janitor)
library(tidyr)
library(ggplot2)
library(tools)
library(readr)
library(rmarkdown)
library(dplyr)
library(shinythemes)

# Load data and perform data manipulation
# Read the CSV file
setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc/Updated_data")

# Read the CSV file
Oldyak_23 <- read.csv('Moeller_2024_Season.csv')

# Data manipulation and cleaning
yak_23 <- Oldyak_23 %>%
  mutate(PitchType = recode(PitchType,
                            "Fast Ball" = "FastBall",
                            "Breaking Ball" = "BreakingBall",
                            "Two Seam Fast Ball" = "TwoSeamFastBall",
                            "Change Up" = "ChangeUp")) %>%
  mutate(PitchResult = recode(PitchResult,
                              "Strike Looking" = "StrikeLooking",
                              "StrikeIn Play" = "StrikeInPlay",
                              "Strike Swing and Miss" = "Strike_Swing_Miss",
                              "Strike Foul" = "StrikeFoul")) %>%
  mutate(AtBatResult = recode(AtBatResult,
                              "Ground Out" = "GroundOut",
                              "Fly Out" = "FlyOut",
                              "Strike Out" = "StrikeOut",
                              "Line Out" = "LineOut",
                              "Double Play" = "DoublePlay",
                              "Infield Fly" = "InfieldFly",
                              "Strike Out" = "StrikeOut",
                              "Fielders Choice" = "FieldersChoice"
  ))

# Change data types
yak_23 <- yak_23 %>%
  mutate(PitchNo = as.integer(PitchNo),
         Date = as.Date(Date, format = "%m/%d/%Y"),
         Time = as.character(Time),
         PAofInning = as.integer(PAofInning),
         PAofInning.1 = as.integer(PAofInning.1),
         Pitcher = as.factor(Pitcher),
         PitcherHand = as.factor(PitcherHand),
         PitcherTeam = as.factor(PitcherTeam),
         Batter = as.factor(Batter),
         Batter.Hand = as.factor(Batter.Hand),
         BatterTeam = as.factor(BatterTeam),
         Inning = as.integer(Inning),
         Top.Bottom = as.factor(Top.Bottom),
         Outs = as.integer(Outs),
         Balls = as.integer(Balls),
         Strikes = as.integer(Strikes),
         PitchType = as.factor(PitchType),
         PitchResult = as.factor(PitchResult),
         AtBatResult = as.factor(AtBatResult),
         PitchVelo = as.numeric(PitchVelo),
         Location = as.factor(Location),
         AttackZone = as.factor(AttackZone))

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # Background and text styles
  tags$style(HTML("
    body {
      background-image: url('https://s.yimg.com/ny/api/res/1.2/Roftf5p.A7GW6V9yIqrFLg--/YXBwaWQ9aGlnaGxhbmRlcjt3PTk2MDtoPTUzOTtjZj13ZWJw/https://media.zenfs.com/en/cincinnati-com-the-enquirer/af73ed7d27851595e327ef9fb6422e7a');
      background-attachment: fixed;
      background-size: cover;
    }
    .tab-content .active h3, .tab-content .active p {
        color: #E0E0E0;
    }
    .tab-content .active {
        background-color: rgba(0, 0, 0, 0.7);
        padding: 15px;
        border-radius: 5px;
    }
  ")),
  
  titlePanel("Baseball Pitch Analysis"),
  navbarPage(
    title = "Baseball Dashboard",
    
    tabPanel("Introduction",
             h3("Overview of Dashboard"),
             p("The Dashboard details..."),
             p("<Assumptions Made> For the purpose of our analysis, we have made the assumption that the influence of these additional factors is negligible. This allows us to concentrate 
                  on the data available and identify the most influential factors within our chosen areas of focus."),
             p("<Business Benefits>"),
             h3("Keys towards app"),
             p("<Drivers>"),
             p("<Key Assumptions>"),
             p("H0: Manufacturing process has no significant influence on breakage rate | H1: Manufacturing process has significant influence on breakage rate"),
             p("H0: Supplier variability has no significant influence on breakage rate | H1: Supplier variability has significant influence on breakage rate"),
             p("<Key metrics of success>"),
             tableOutput("dataDictTable")
    ),
    
    tabPanel("Game Summary",
             sidebarPanel(
               selectInput("pitcherTeam1", "Select Pitcher Team:", choices = unique(yak_23$PitcherTeam)),
               selectInput("pitcher1", "Select Pitcher:", choices = unique(yak_23$Pitcher)),
               dateRangeInput("dateRange1", "Select Date Range:",
                              start = min(yak_23$Date),
                              end = max(yak_23$Date)),
               tableOutput("gameSummaryTable")
             )
    ),
    
    tabPanel("Pitch Usage",
             sidebarPanel(
               selectInput("pitcherTeam2", "Select Pitcher Team:", choices = unique(yak_23$PitcherTeam)),
               selectInput("pitcher2", "Select Pitcher:", choices = unique(yak_23$Pitcher)),
               dateRangeInput("dateRange2", "Select Date Range:",
                              start = min(yak_23$Date),
                              end = max(yak_23$Date)),
               tableOutput("pitchUsageTable")
             )
    ),
    
    tabPanel("Righty vs Lefty",
             sidebarPanel(
               selectInput("pitcherTeam3", "Select Pitcher Team:", choices = unique(yak_23$PitcherTeam)),
               selectInput("pitcher3", "Select Pitcher:", choices = unique(yak_23$Pitcher)),
               dateRangeInput("dateRange3", "Select Date Range:",
                              start = min(yak_23$Date),
                              end = max(yak_23$Date))
             ),
             mainPanel(
               h4("Usage vs Right-Handed Hitters"),
               tableOutput("usageRTable"),
               h4("Stats vs Right-Handed Hitters"),
               tableOutput("statsVsRTable"),
               h4("Usage vs Left-Handed Hitters"),
               tableOutput("usageLTable"),
               h4("Stats vs Left-Handed Hitters"),
               tableOutput("statsVsLTable")
             )
    ),
    
    tabPanel("Batted Ball",
             sidebarPanel(
               selectInput("pitcherTeam4", "Select Pitcher Team:", choices = unique(yak_23$PitcherTeam)),
               selectInput("pitcher4", "Select Pitcher:", choices = unique(yak_23$Pitcher)),
               dateRangeInput("dateRange4", "Select Date Range:",
                              start = min(yak_23$Date),
                              end = max(yak_23$Date)),
               tableOutput("battedBallTable")
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update the pitcher dropdown based on the selected team
  observe({
    updateSelectInput(session, "pitcher1", choices = unique(yak_23 %>% filter(PitcherTeam == input$pitcherTeam1) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher2", choices = unique(yak_23 %>% filter(PitcherTeam == input$pitcherTeam2) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher3", choices = unique(yak_23 %>% filter(PitcherTeam == input$pitcherTeam3) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher4", choices = unique(yak_23 %>% filter(PitcherTeam == input$pitcherTeam4) %>% pull(Pitcher)))
  })
  
  filteredData1 <- reactive({
    yak_23 %>%
      filter(PitcherTeam == input$pitcherTeam1 & 
               Pitcher == input$pitcher1 &
               Date >= input$dateRange1[1] &
               Date <= input$dateRange1[2])
  })
  
  filteredData2 <- reactive({
    yak_23 %>%
      filter(PitcherTeam == input$pitcherTeam2 & 
               Pitcher == input$pitcher2 &
               Date >= input$dateRange2[1] &
               Date <= input$dateRange2[2])
  })
  
  filteredData3 <- reactive({
    yak_23 %>%
      filter(PitcherTeam == input$pitcherTeam3 & 
               Pitcher == input$pitcher3 &
               Date >= input$dateRange3[1] &
               Date <= input$dateRange3[2])
  })
  
  filteredData4 <- reactive({
    yak_23 %>%
      filter(PitcherTeam == input$pitcherTeam4 & 
               Pitcher == input$pitcher4 &
               Date >= input$dateRange4[1] &
               Date <= input$dateRange4[2])
  })
  
  # Define the data dictionary
  data_dictionary <- data.frame(
    variable_name = c("PitchNo", "Date", "Time", "PAofInning", "PAofInning.1", "Pitcher", 
                      "PitcherHand", "PitcherTeam", "Batter", "Batter Hand", "Top/Bottom", 
                      "Outs", "Balls", "Strikes", "PitchType", "PitchResult", "AtBatResult", 
                      "PitchVelo", "Location", "AttackZone"),
    R_data_type = c("numeric", "Date", "Time", "numeric", "numeric", "factor", 
                    "factor", "factor", "factor", "factor", "factor", 
                    "numeric", "numeric", "numeric", "factor", "factor", "factor", 
                    "numeric", "numeric", "factor"),
    description = c("Pitch Number", "Date of the game", "Time of the pitch", "Plate appearance of inning", 
                    "Duplicated plate appearance of inning", "Pitcher name", "Pitcher hand (R/L)", 
                    "Pitcher team name", "Batter name", "Batter hand (R/L)", "Top or Bottom of inning", 
                    "Number of outs", "Number of balls", "Number of strikes", "Type of pitch thrown", 
                    "Result of the pitch", "Result of the at-bat", "Velocity of the pitch", "Pitch location", 
                    "Zone of the attack")
  )
  
  # Render the data dictionary as a table
  output$dataDictTable <- renderTable({
    data_dictionary
  })
  
  # Game summary table
  output$gameSummaryTable <- renderTable({
    filteredData1() %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "ChangeUp" = "CH",
                                "Curve" = "CB",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      summarize(
        No. = n(),
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = round(max(PitchVelo, na.rm = TRUE), 1)
      ) %>%
      mutate(`Usage %` = round(No. / sum(No.) * 100, 3))
  })
  
  # Pitch usage table
  output$pitchUsageTable <- renderTable({
    filteredData2() %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "ChangeUp" = "CH",
                                "Curve" = "CB",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      summarize(
        No. = n(),
        `90+` = sum(PitchVelo >= 90, na.rm = TRUE),
        `% FB 90+ mph` = round(sum(PitchVelo >= 90 & PitchType == 'FB', na.rm = TRUE) / n() * 100, 1),
        `2K` = sum(Strikes == 2, na.rm = TRUE),
        `2K%` = round(sum(Strikes == 2, na.rm = TRUE) / n() * 100, 1),
        `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 1),
        `Whiff%` = round(sum(PitchResult %in% c("Strike_Swing_Miss"), na.rm = TRUE) / 
                           sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) * 100, 1)
      ) %>%
      ungroup() %>%
      mutate(
        TotalPitches = sum(No.),
        `Usage %` = round(No. / sum(No.) * 100, 1),
        `90+% vs Total` = round(`90+` / TotalPitches * 100, 1)
      ) %>%
      select(-TotalPitches)
  })
  
  # Usage vs RHH table
  output$usageRTable <- renderTable({
    filteredData3() %>%
      filter(Batter.Hand == 'R') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', Slider = 'SL',
                                TwoSeamFastBall = '2FB', Changeup = 'CH', Splitter = 'SPL')) %>%
      group_by(Pitch = PitchType) %>%
      summarize(No. = n(),
                `Usage %` = n(),
                `2K` = sum(Strikes == 2),
                `2K%` = sum(`2K`),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n(), 3) * 100,
                `Whiff%` = round(sum(PitchResult %in% c("Strike_Swing_Miss"), na.rm = TRUE) / 
                                   sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE), 3) * 100)
  })
  
  # Stats vs RHH table
  output$statsVsRTable <- renderTable({
    filteredData3() %>%
      filter(Batter.Hand == 'R') %>%
      summarize(BF = n_distinct(Inning, Batter),
                K = sum(AtBatResult == "StrikeOut"),
                BIP = sum(AtBatResult == 'DoublePlay'),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
                XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')))
  })
  
  # Usage vs LHH table
  output$usageLTable <- renderTable({
    filteredData3() %>%
      filter(Batter.Hand == 'L') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', Slider = 'SL',
                                TwoSeamFastBall = '2FB', Changeup = 'CH', Splitter = 'SPL')) %>%
      group_by(Pitch = PitchType) %>%
      summarize(No. = n(),
                `Usage %` = n(),
                `2K` = sum(Strikes == 2),
                `2K%` = sum(`2K`),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n(), 3) * 100,
                `Whiff%` = round(sum(PitchResult %in% c("Strike_Swing_Miss"), na.rm = TRUE) / 
                                   sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE), 3) * 100)
  })
  
  # Stats vs LHH table
  output$statsVsLTable <- renderTable({
    filteredData3() %>%
      filter(Batter.Hand == 'L') %>%
      summarize(BF = n_distinct(Inning, Batter),
                K = sum(AtBatResult == "StrikeOut"),
                BIP = sum(AtBatResult == 'DoublePlay'),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
                XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')))
  })
  
  # Batted ball table
  output$battedBallTable <- renderTable({
    filteredData4() %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "Changeup" = "CH",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      summarize(
        No. = n(),
        BIP = sum(AtBatResult %in% c('GroundOut', 'FlyOut', 'LineOut')),
        XBH = sum(AtBatResult %in% c("2B", "3B", "HR"))
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

  
