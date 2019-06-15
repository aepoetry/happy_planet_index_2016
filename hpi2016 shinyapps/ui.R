library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(stringr)
library(ggthemes)
library(RColorBrewer)
library(data.table)
library(maps)
library(cluster)
library(FactoMineR)
library(factoextra)


# Define UI
ui <- fluidPage(
    theme = shinytheme("united"),
    
    navbarPage("Happy Planet Index",
               
               # Home
               tabPanel(
                   "Home",
                   fluidRow(
                       column(12,
                              h2(strong("About Happy Planet Index")),
                              hr(),
                              br(),
                              p("Happy Planet Index is an index created by Nic Marks, 
                         a statistician and a happiness researcher, from New Economics Foundation (NEF).
                         The index measures what matters: ", strong("sustainable wellbeing for all"),
                                ". It tells us how well nations are doing at achieving long, happy, sustainable lives."
                              ),
                              br(),
                              h2(strong("How to calculate Happy Planet Index")),
                              hr(),
                              br(),
                              column(12, align = "center",
                                  tags$img(src = "calcHPI.png", height = "70%", width = "70%")),
                              
                              br(),
                              fluidRow(
                              column(
                              2, align = "center",
                              tags$img(src = "wellbeing.png", height = "50px", width = "50px")
                              ),
                              column(
                              9,
                              p(strong("Wellbeing"), " : How satisfied the residents of each country
                                say they feel with life overall, on a scale from zero to ten, based on data collected as part of 
                                the Gallup World Poll."
                                )
                                )
                              ),
                              
                              br(),
                              fluidRow(
                              column(
                              2, align = "center",
                              tags$img(src = "life.png", height = "50px", width = "50px")
                              ),
                              column(
                              9,
                              p(strong("Life expectancy"), " : The average number of years a person is expected to 
                               live in each country based on data collected by the United Nations."
                                )
                                )
                              ),
                              
                              br(),
                              fluidRow(
                              column(
                              2, align = "center",
                              tags$img(src = "inequality.png", height = "50px", width = "50px")
                              ),
                              column(
                              9,
                              p(strong("Inequality of outcome"), " : The inequalities between people within a country, 
                               in terms of how long they live, and how happy they feel, based on the distribution in 
                               each country’s life expectancy and wellbeing data. 
                               Inequality of outcomes is expressed as a percentage."
                                )
                                )
                              ),
                              
                              br(),
                              fluidRow(
                              column(
                              2, align = "center",
                              tags$img(src = "footprint.png", height = "50px", width = "50px")
                              ),
                              column(
                              9,
                              p(strong("Ecological footprint"), " : The sum of the global hectares needed 
                              to support the resource consumption and waste generation of the person gives / 
                              biologically productive area needed to provide for everything people use: 
                              fruits and vegetables, fish, wood, fibers, absorption of carbon dioxide from 
                              fossil fuel use, and space for buildings and roads, 
                              based on data prepared by the Global Footprint Network."
                                )
                                )
                              ),
                              
                              br(),
                              h2(strong("Why do we need the Happy Planet Index?")),
                              hr(),
                              br(),
                              
                              p("We have lived with the belief that the world is becoming a better place. 
                       Unstable global economy, rising inequalities, and climate change have begun to shatter that belief. 
                       Recent surveys reveal that majorities in both the USA and Europe have said 
                       they no longer think life is getting better."
                              ),
                              p("One cause of these crises is the stubborn prioritisation of economic growth as the central 
                         objective of government above all other objectives. People vote for political parties that they perceive 
                         to be most capable of delivering a strong economy, and policy makers prioritise policies that increase in GDP 
                         as a result. Doing so has led to short-termism, deteriorating social conditions, and paralysis in the 
                         face of climate change."
                              ),
                              p("In fact, GDP growth on its own does not mean a better life for everyone, 
                         particularly in countries that are already wealthy. It does not reflect inequalities in material 
                         conditions between people in a country. It does not properly value the things that really matter 
                         to people like social relations, health, or how they spend their free time. And crucially, ever-more 
                         economic growth is incompatible with the planetary limits we are up against."
                              ),
                              
                              br(),
                              h2(strong("Happy Planet Index Research Insight")),
                              hr(),
                              br(),
                              p("From his research, Wealthy Western countries, often seen as the standard of success, 
                         do not rank highly on the Happy Planet Index. Instead, several countries in Latin America 
                         and the Asia Pacific region lead the way by achieving high life 
                         expectancy and wellbeing with much smaller Ecological Footprints."
                              ),
                              p("The Happy Planet Index provides a compass to guide nations, 
                         and shows that it is possible to live good lives without costing the Earth."
                              ),
                              br(),
                              br(),
                              br()
                       )
                   )
               ),
               
               # Tab Panel 1 HPI Map
               tabPanel(
                   "HPI Map",
                   fluidRow(
                       column(12,
                              h2("Happy Planet Index Map"),
                              hr(),
                              br(),
                              wellPanel(span("Explore how countries compare on each of the four elements of the HPI")),
                              selectInput(inputId = "Var",
                                          label = "Choose Variable",
                                          choices = c(
                                              "Life Expectancy",
                                              "Wellbeing",
                                              "Inequality",
                                              "Ecological Footprint",
                                              "Happy Planet Index"
                                          ),
                                          selected = "Happy Planet Index")
                       ),
                       column(12,
                              br(),
                              plotlyOutput("HPI_Map", width = "100%", height = "100%", inline = TRUE),
                              wellPanel(span("Based on data from: ", a(href = "", "http://happyplanetindex.org/")))
                       )
                   )
               ),
               
               # Tab Panel 2 Heat Map
               tabPanel(
                   "Clustering Analysis",
                   fluidRow(
                       column(12,
                              h2("A Simple Correlation Heatmap"),
                              br(),
                              plotlyOutput("Heatmap"),
                              wellPanel(span("Apparently there's a strong correlation between Average Life Expectancy, Average Wellbeing, and Inequality. GDP is not giving strong correlations to the other variables and so we can say that, money can't (always) buy long life and happiness, eh?"))
                       )
                   ),
                   fluidRow(
                       column(12,
                              h2("Contribution Value"),
                              br(),
                              plotOutput("PCA_contrib"),
                              wellPanel(span("This graph highlights the most important variables in explaining the variations retained by the principal components. The larger the value of the contribution, the more the variable contributes to the component."))
                       )
                   ),
                   fluidRow(
                       column(12,
                              h2("Cluster Plot"),
                              br(),
                              plotOutput("ClusterPlot"),
                              wellPanel(span("It's easy to understand, but which country belong to which cluster? Well, I think we'll get the idea better if we use world map to visualize our cluster."))
                       )
                   )
               ),
               
               # Tab Panel 3 Cluster Analysis
               tabPanel(
                   "Visualization",
                   fluidRow(
                       column(12,
                              h2("Happy Planet Index Cluster Map"),
                              br(),
                              wellPanel(span(
                                  "Using principal component analysis to separate 140 countries into 3 different clusters with similar characteristics.")),
                              selectInput(inputId = "Cluster_input",
                                          label = "Choose Cluster",
                                          choices = c("All", "Cluster 1", "Cluster 2", "Cluster 3"),
                                          selected = "All"),
                              plotlyOutput(outputId = "ClusterMap", width = "100%", height = "100%", inline = TRUE),
                              wellPanel(span(
                                "Notes :",
                                br(),
                                textOutput(outputId = "InsightCluster")
                                )
                                )
                              )
                       )
                   ),
               
               # Tab Panel 4 Conclusion
               navbarMenu(
                   "More",
                   tabPanel(
                       "Top 5 and Bottom 5",
                       fluidRow(
                           column(12,
                                  h2("Happy Planet Index Country Rankings"),
                                  br(),
                                  wellPanel(span("Explore the top 5 and bottom 5 countries on the Happy Planet Index, and on each of the components used to calculate Happy Planet Index scores.")),
                                  selectInput(inputId = "Var2",
                                              label = "Choose Variable",
                                              choices = c(
                                                  "Life Expectancy",
                                                  "Wellbeing",
                                                  "Inequality",
                                                  "Ecological Footprint",
                                                  "Happy Planet Index"
                                              ),
                                              selected = "Happy Planet Index"),
                                  tableOutput("Table")
                           )
                       )
                   ),
                   tabPanel(
                       "140 Country Data",
                       fluidRow(
                           column(12,
                                  h2("Happy Planet Index Country Rankings"),
                                  br(),
                                  wellPanel(span("Explore the countries on the Happy Planet Index, and on each of the components used to calculate Happy Planet Index scores.")),
                                  column(4,
                                         sliderInput(inputId = "HPI_sld",
                                                     label = "Happy Planet Index",
                                                     min = 10,
                                                     max = 50,
                                                     value = c(20, 30))
                                  ),
                                  column(4,
                                         sliderInput(inputId = "Life_sld",
                                                     label = "Average Life Expectancy (years)",
                                                     min = 45,
                                                     max = 85,
                                                     value = c(60, 75))
                                  ),
                                  column(4,
                                         sliderInput(inputId = "Well_sld",
                                                     label = "Average Wellbeing",
                                                     min = 2.5,
                                                     max = 8,
                                                     value = c(4.5, 6))
                                  )
                           ),
                           fluidRow(
                               column(12,
                                      column(4,
                                             sliderInput(inputId = "Ineq_sld",
                                                         label = "Inequality",
                                                         min = 0,
                                                         max = 0.6,
                                                         value = c(0.2, 0.4))),
                                      column(4,
                                             sliderInput(inputId = "CarbF_sld",
                                                         label = "Ecological Footprint (gha)",
                                                         min = 0,
                                                         max = 16,
                                                         value = c(2, 10))),
                                      column(4,
                                             textInput(inputId = "Country_tI",
                                                       label = "Type a name of a country"))
                               )
                           ),
                           fluidRow(
                               column(12,
                                      dataTableOutput("FullTable")
                               )
                           )
                       )
                   )
               )
    )
)

