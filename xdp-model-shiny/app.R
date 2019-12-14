library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(janitor)
library(animation)
library(magick)
library(readxl)
library(knitr)
library(janitor)
library(reshape2)
library(psych)
library(ggcorrplot)
library(scales)
library(plotly)
library(shinythemes)
library(htmltools)
library(dplyr)
library(gt)
library(tidyr)
library(mapdata)
library(ggcorrplot)
library(png)
library(corrplot)
library(tidyverse)


#Create list of genes for users to scroll through on the second tab, and factors for the 
#overall bar plot distributions on the "What is XDP?" section.
factors <- c("Sex", "Knockdown", "Percent Viability")

gene_names <- c("All", "ARNTL", "ATF2", "DCTN6", "EIF2AK3/PERK", "GABBR1", 
                "GSK3B", "HIF1A", "hSPB5/BIP/GRP78", "MAPK1", "NRCAM", "P4HA1", "RELB", 
                "STK36", "TRAF6", "XBP1")

#Read in all of my graphs from the prep shiny so I can display them in the tabs more easily.
#I initially used the more efficient framework of feeding user inputs into temporary 
#dataframes and transforming them, but a series of crashes last week made it impossible
#to do it in another way. The plots attached here have also encountered trouble with
#errors saying "In gzfile(file, "rb") : cannot open compressed file 'xxx.rds', probable
#reason 'No such file or directory'". Uploading Shiny has been sporadic, and I have tried
#streamlining comments as much as I can to make this process less painful. 

bosploty <- read_rds("boxploty.rds")
arntl1 <- read_rds("arntl1.rds")
atf21 <- read_rds("atf21.rds")
dctn61 <- read_rds("dctn61.rds")
perk1 <- read_rds("perk1.rds")
gaba1 <- read_rds("gaba1.rds")
gsk3b1 <- read_rds("gsk3b1.rds")
hif1a1 <- read_rds("hif1a1.rds")
hspb51 <- read_rds("hspb51.rds")
nrcam1 <- read_rds("nrcam1.rds")
p4ha11 <- read_rds("p4ha11.rds")
relb1 <- read_rds("relb1.rds")
stk361 <- read_rds("stk361.rds")
traf61 <- read_rds("traf61.rds")
xbp11 <- read_rds("xbp11.rds")
all1 <- read_rds("all1.rds")
corplot <- readPNG("corplot.png")
all2 <- read_rds("all2.rds")
all3 <- read_rds("all3.rds")
sex1 <- read_rds("sex1.rds")
knock1 <- read_rds("knock1.rds")
rkc <- read_rds("rkc.rds")
res.df <- read_rds("res.rds")

ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "XDP: Novel Fly Model for A Cure",
    tabPanel(
      "Disease and Background",
      titlePanel("X-linked Dystonia Parkinsonism"),
      br(),
      h4("XDP Pathology"),
      htmlOutput("introduct"),
      plotOutput("boxy"),
      h4("Which metric is better to use?"),
      htmlOutput("metricsan"), 
      sidebarLayout(
        sidebarPanel(
          selectInput("somanyfactors", "Factors affecting fly recovery", factors)
        ),
        mainPanel(
      htmlOutput("p1analysis"),
      plotOutput("factorsPlot")
      )
    )),
    tabPanel("Viability Scores",
             h2("Analysis of viability across months"),
             h4("Click to see the viability for each vial of the knockdown
                genes, pathway trends, and linear correlation across time!"),
    sidebarLayout(
      sidebarPanel(
        selectInput("gene1", "Knockdown Gene", gene_names)
                    ),
    mainPanel(
      plotOutput("viabilityPlot"),
      htmlOutput("viabilitytrends"),
      plotOutput("eyes"),
      htmlOutput("eyecorrelation")
    )
    )),
    tabPanel(
      "Trends",
      titlePanel("Cross Section Patterns"),
      br(),
      mainPanel(
        htmlOutput("takeaway1"),
        plotOutput("weekday"),
        htmlOutput("takeaway2"),
        plotOutput("compPlot"),
        htmlOutput("comeplay")
      )
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
  
  data_input <- reactive({
    switch(input$gene1,
           "All" = all1,
           "ARNTL" = arntl1, 
           "ATF2" = atf21, 
           "DCTN6"= dctn61, 
           "EIF2AK3/PERK" =  perk1, 
           "GABBR1" = gaba1, 
           "GSK3B" = gsk3b1, 
           "HIF1A" = hif1a1, 
           "hSPB5/BIP/GRP78" = hspb51, 
           "MAPK1" = mapk11, 
           "NRCAM" = nrcam1, 
           "P4HA1" = p4ha11,
           "RELB" = relb1, 
           "STK36" =  stk361, 
           "TRAF6" = traf61, 
           "XBP1"= xbp11
    )
  })
  
  output$viabilityPlot <- renderPlot(data_input())
  
  data_input2 <- reactive({
    switch(input$somanyfactors,
           "Sex" = sex1,
           "Knockdown" = knock1,
           "Percent Viability" = rkc
    )
  })
  output$factorsPlot <- renderPlot(data_input2())
  
  output$introduct <- renderUI({
    HTML("<p> X-linked Dystonia Parkinsonism (XDP) is a rare, fatal neurodegenerative disease 
         native to Panay, Philippines, characterized physically by dystonic and parkinsonism 
         behaviors and striatal cell death. Patient pathology resembles aspects of Huntington’s 
         Disease (HD) and Parkinson’s Disease (PD), and is most likely caused by lowered levels 
         of TATA-Box Binding Protein Associated Factor 1(TAF1) due to an intron retrotransposon 
         insertion. Because TAF1’s diverse functions are not well explored, it has been difficult
         to determine which other gene pathways it interacts with to produce pathologic mechanisms. 
         A tool called the Connectivity MAP (CMAP) identified significant overlap between XDP 
         signatures, and a strong theory is that by corrupting some of these genes, the effects
         of XDP will be stopped. A big boundary in testing if silencing these genes will cure the disease
         is the lack of an animal model. Three of the most prominent pathways that overlap with
         XDP symptoms are the ER Stress response, Oxidative Stress Response, and NFkB pathways.
         This project demonstrates the findings generated from a novel, fruit fly-based model
         which disrupts TAF1 in the flies eyes and allows one to easily observe whether drugs or
         other gene knockouts has rescued symptoms by looking at how big the eye has grown.")
  })
  
  output$p1analysis <- renderUI({
    HTML("<p>While both sexes are represented in the sample, if one simply looks at the first
    factor bar graph, when comparing it to the knockout bar graph and the combined
    stacked graphs, there is a clear imbalanced between sexes in TAF1 affected flies — namely,
    a higher percentage of knockdown survivors are female. In humans, the XDP gene is 
    carried on the X chromosome, and thus males are predominantly represented in the patient
    population, whereas two copies of the gene is required for females to show symptoms. The 
    fact that this is also noted in flies, whose chromosomes depend not on a binary schematic
    but rather a ratio of sex chromosomes, lends question to if there is something evolutionarily
    preserved in the sex chromosomes that is being affected by the TAF1 deficiency. <p>")
  })
  
  output$metricsan <- renderUI({
    HTML("After the first rounds of experiments, researchers quickly noticed not only were 
    TAF1-deficient flies losing eyes, they were also not hatching. In a typical Mendelian
    cross, 50% of flies are supposed to be normal, and 50% have the TAF1 knockout and 
    are expected to have no eyes. However, data shown in the boxplot below demonstrates
    this is far from the case. This boxplot plots 10 samples of 15-150 flies per week
    and maps the range of the viabilities. Most viabilities (% of TAF1 knockdown flies that survive)
    are far below 50%. Thus, viability becane a strong contender for measuring how well
    the target gene knockout. However, analyzing this boxplot also demonstrates that some
    genes which show strong rescue have far too large a standard error to be relied upon.
    One example is the STK36 gene, which seems to show a high viability range, but has an enormous 
    third quartile pulled by several outliers.<p>")
  })
  
  
  output$viabilitytrends <- renderUI({
    HTML("<p>Something I found interesting here is that the viability seems
         to be increasing each week, even though most of the data is at the
         bottom, and the positive linear correlation seems to be coming from
         the outliers at the top. It also looks like within each week,
         viability rises at the end of the week. Since it is difficult to asssign
         a meaningful numeric value to the days in this range, this will be analyzed 
         further from a different angle in Trends.<p>")
  })
  
  output$eyecorrelation <- renderUI({
    HTML("<p>When determining levels of phenotypi rescue, the the idea of using eye size
    led to a qualitative metric of eye size, ranging from 0-4, with 0 being nonexistent eyes 
    and 4 being almost or fully restored. One possibility to consider with data collection is 
    hat given the limited range of outcomes possible for eye size, how much randomness is there when
         the eyes are restored by gene knockdowns? In order to test if there is a consistent
         favoring on one eye, I plotted all the eye data from the surviving TAF1 flies and 
         used a linear model to graph a line of best fit. Assuming a Gaussian distribution,
         since the eye values tend to cluster around a size between 2 to 3 on average,
         performing a stan_glm test on this correlation revealed a median left eye size intercept
         of 1 and a median right eye size intercept of 0.8. This model suggests that for
         every increase in the right eye, both eyes increase almost the same amount, 
         even though the intercept of size 1 in the left eye indicates a fly with size 0
         right eye is likely to have a larger left eye comparatively. The 95th percent confidence
         interval for the standard deviation of both intercepts is 0, indicating there is a strong
         likelihood these correlations are in the correct value ranges, and there is a consistent
         positive correlation between left and right eye size.<p>")
  })
  
  output$takeaway1 <- renderUI({
    HTML("<p>One macroscopic pattern trend which has biologically suspicions is that
         flies that hatch later in the week, the last days of the adult life cycle, may
         have <q>broken</q> chromosome structures that were too unstable to maintain the
         TAF1 knockdown, or are products of crosses between surviving adults, and thus
         would not bear the same TAF1 deficiency we are trying to measure. Thus, this graph
         combines all the viability data derived from the same vials of the same gene
         throughout the semester, and categorized them by day of the week.")
  })
  
  output$takeaway2 <- renderUI({
    HTML("<p> Converting
         Monday-Friday to a numerical scale, and performing a stan_glm model on the data,
         we obtain a median viability intercept of 2.4, meaning that on day 0, so to speak,
         the survival rate of TAF1 flies is expected to be 2.4% of the entire vial of flies,
         which is close to the viabilities that control samples exhibit (see tab 1 for 
         refrence). The 95th percent confidence interval on the standard deviation of this
         intercept is 1.1, indicating there is a 95% chance that a guess between 1.3% and
         3.6% is likely in the right ballpark. The intercept for viability per increased 
         weekday is 2.0%, with a much smaller SD range of 0.3. This suggests that as flies
         are measured later in the week, the improvement in viability increases the original
         viability by close to 100%. The distribution of this data was not
         entirely gaussian and the majority of the data is condensed around the predicted
         viability intercept, so perhaps the data is partially biased due to a increased
         anomality of outlier flies with rapid chromosomal breakdown.")
  })
  
  output$comeplay <- renderUI({
    HTML("<p> A correlation analysis was performed between the survival rate of knockouts
         (a positive correlation indicates increased viability), eye size, day of the week,
         and whether one of the three specially identified pathways in the CMAP tool 
         predicted higher fly survival rate. Since there has been no quantitivate test
         performed to verify how much of the targeted genes in the pathways were silenced,
         the numbers used for the pathway correlation was simply 1 for a gene belonging to
         the pathway, and 0 for not. Algining with earlier statistical
         analysis, there is a high correlation between right and left eye sizes.
         A negative strong correlation bewteen survival (higher knockdown) also correlated
         with a large decrease in the left and right eyes, which may be 
         attributed to the fact that a larger population of surviving
         knockdown flies have more defective eyes than a smaller population
         of knockdown flies simply due to quantity. Of the three pathways,
         only NFkB had a positive correlation with survival, and surprisingly
         weekday had a stronger effect on surviva than any other indicator, 
         including sex, which still shows a slight increase in female flies.
         Most of these values, barring the left/right eye and eye/survival
         correlations, are significant; however, as shown by linear analysis,
         even weekday, which is predicted to double viability, only has a correlation
         value of 0.28, suggesting that the other values which don't appear
         to be statisticaly significant may be imparting more impact than
         first meets the eye. ")
  })
  
  output$correlation <- renderImage(corplot)
  
  output$weekday <- renderPlot(all2)
  
  output$eyes <- renderPlot(all3)
  
  output$sexy <- renderPlot(sex1)
  
  output$knockout <- renderPlot(knock1)
  

  output$sknockout <- renderPlot(rkc)
  
  output$compPlot <- renderPlot({
    cor_all <- corr.test(res.df, adjust = "none")
    cor_plot <- ggcorrplot(cor_all[["r"]], 
                           hc.order = TRUE, 
                           type = "lower",
                           method = "square",
                           lab = TRUE,
                           lab_size = 2.5) +
      labs(title = "Correlation between Pathways, Eye Size, Weekday, and Survival")
    
    cor_plot
  })
    
  output$boxy <- renderPlot(bosploty)

  
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
    HTML("<br><b><font size=4> About Me</font></b> 
          <br>
           My name is Minjue Wu and I am a sophomore at Harvard College. I study History of
           Science and Music with a secondary in Global Health and Health Policy. I'm from
           Anhui, China, and love all things related to anime, cats, and frozen desserts.
          <br>
                             ")
  })
  
}

shinyApp(ui, server)

