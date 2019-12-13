library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(tidyverse)

#Read in all of my graphs from the prep shiny so I can display them in the tabs more easily.
#I want to figure out how to do the ggplots on my app.R in the future to save on
#code and space

#Create list of anime for users to scroll through on the first tab

kogenes <- c("ARNTL", "ATF2", "DCTN6", "EIF2AK3/PERK", "GABBR1", "GSK3B", "HIF1A", "hSPB5/BIP/GRP78", "MAPK1", "NRCAM", "P4HA1", "RELB", "STK36", "TRAF6", "XBP1")
kopaths <- c("CMAP Hit", "NFkB", "Oxidative Stress", "ER Stress")

#Provide two scrolls on the front page so people can cross compare
#popularity and rank trends between different popular anime simultaneously,
#instead of clicking back and forth across a faceted graph

#Provided separate statistical modeling combining all the datasets
#on second tab to show overall trends and ues as a baseline to point
#out deviations on the first tab

ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "XDP: Novel Fly Model for A Cure",
    tabPanel(
      "Disease and Background",
      titlePanel("X-linked Dystonia Parkinsonism"),
      br()
    ),
    tabPanel("Eye Scores and Viability"
    ),
    tabPanel(
      "Trends"
    ),
    tabPanel(
      "About",
      htmlOutput("intro"),
      htmlOutput("hitter"),
      uiOutput("ls100"),
      htmlOutput("aboutme")
    )
  )
)

#connected all the menu selection choices with their respective rds files
#I feel like my current formatting is clunky — going to try to figure out how to
#make it more aesthetically pleasing for my final presentation

server <- function(input, output, session) {
  
  output$intro <- renderUI({
    HTML("<b><font size=6> Drosophila Melanogaster and the Race to Find a Cure </font></b>
          <br><br>
          <p> X-linked Dystonia Parkinsonism is a rare, devasting 
         neurodegenerative disorder which affects descendants from
         the Panay Island, Philippines. Its pathology shares similarities
         with a number of other prominent illnesses, including Hungtinton's
         Disease and Parkinson's Disease. These factors, in addition to the
         devastating consequences of the disease on impacted individuals in 
         the Philippines who cannot afford expensive medical treatment, makes
         finding a cost-effective cure crucial.
         In this app, you will be able to explore the background of XDP, and
         understand a new fly model designed to screen for gene targets which,
         when silenced, restores <q>XDP</q> fly survival rates and eye size,
         and thus may also rescue the damage caused by the disease in human
         patients as well. You will be able to see the distribution of eye sizes,
         fly survival rates after knockdown, correlation between sex, time of week,
         and various genetic pathways that may hold keys to a future cure.
         ")})
  output$hitter <- renderUI({
    HTML("<b><font size=4>Acknowledgements </font></b> 
          <br>
          This project is made possible by groundbreaking individuals actively
          researching, teaching, and collecting financial and political support
          for XDP research. Many thanks to Geraldine Acuna-Sunshine, a board member 
          of the Harvard Board of Overseers and the founder of Sunshine Care Foundation
          which funds the Life Sciences 100 course through which these experiments were
          carried out under the supervision of professor Alain Viel. Naoto Ito and 
          Noriko Wakabayashi developed the novel Drosophila screening model. Cris Bragg,
          MD, the director of the Center for XDP Research at MGH, provided direction for the
          use of the CMAP and targeted genes. Finally, the execution of 
          the data gathered here are a cumulation of more than 18,000 hand-recorded fly datapoints
          over 480 hours by six Harvard undergraduates: Matthew Li, Minjue Wu, Paul Doan,
          Toni Scott, Michelle Lara, and Jessica Miller. 
          <br>
          <br>
                             ")
  })
  
  urlls100 <- a("The LS100 Fall 2019 Project One Link", href = "https://drive.google.com/open?id=1FQpT1aZWjhlm-Inn_hrF55AL-AXEOI7F")
  
  output$ls100 <- renderUI({
    tagList("Raw Fly Data can be found at", urlls100)
  })
  
  output$aboutme <- renderUI({
    HTML("<b><font size=4> About Me</font></b> 
          <br>
           My name is Minjue Wu and I am a sophomore at Harvard College. I study History of
           Science and Music with a secondary in Global Health and Health Policy. I'm from
           Anhui, China, and love all things related to anime, cats, and frozen desserts.
          <br>
                             ")
  })
  
}

shinyApp(ui, server)

