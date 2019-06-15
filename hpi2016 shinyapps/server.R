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

source("helper.R")

# Define server logic
server <- function(input, output) {
    
    # Home
    
    # Panel 1
    # Map1 <- reactive({
    #     if (input$Var == "Happy Planet Index") {
    #         HPImap
    #     } else if (input$Var == "Life Expectancy") {
    #         LEmap
    #     } else if (input$Var == "Wellbeing") {
    #         WBmap
    #     } else if (input$Var == "Inequality") {
    #         Imap
    #     } else {
    #         Fmap
    #     }
    # })
    # 
    output$HPI_Map <- renderPlotly({
        if (input$Var == "Happy Planet Index") {
                    HPImap
                } else if (input$Var == "Life Expectancy") {
                    LEmap
                } else if (input$Var == "Wellbeing") {
                    WBmap
                } else if (input$Var == "Inequality") {
                    Imap
                } else {
                    Fmap
                }
    })
    
    # Panel 2
    output$Heatmap <- renderPlotly(heatmap_HPI)
    output$PCA_contrib <- renderPlot(PCA_viz)
    output$ClusterPlot <- renderPlot(Cluster_viz)
    
    # Panel 3
    
    # Cluster_Map <- reactive({
    #     if (input$Cluster_input == "All") {
    #         cluster_map_v
    #     } else if (input$Cluster_input == "Cluster 1") {
    #         cluster_map_1
    #     } else if (input$Cluster_input == "Cluster 2") {
    #         cluster_map_2
    #     } else {
    #         cluster_map_3
    #     }
    # })
    
    output$ClusterMap <- renderPlotly(
        {
            if (input$Cluster_input == "All") {
                cluster_map_v
            } else if (input$Cluster_input == "Cluster 1") {
                cluster_map_1
            } else if (input$Cluster_input == "Cluster 2") {
                cluster_map_2
            } else {
                cluster_map_3
            }
        }
    )
        
    
    # Insight
    
    Insight_text <- reactive({
        if (input$Cluster_input == "All") {
            ClustText
        } else if (input$Cluster_input == "Cluster 1") {
            Clust1Text
        } else if (input$Cluster_input == "Cluster 2") {
            Clust2Text
        } else {
            Clust3Text
        }
    })

    output$InsightCluster <- renderText({
        Insight_text()
        })
    
    # Panel 4
    
    # Top 5 Bottom 5
    # InputTable1 <- reactive({
    #     if (input$Var2 == "Happy Planet Index") {
    #         HPI_table
    #     } else if (input$Var2 == "Life Expectancy") {
    #         LE_table
    #     } else if (input$Var2 == "Wellbeing") {
    #         WB_table
    #     } else if (input$Var2 == "Inequality") {
    #         I_table
    #     } else {
    #         F_table
    #     }
    # })
    
    output$Table <- renderTable(
        {
            if (input$Var2 == "Happy Planet Index") {
                HPI_table
            } else if (input$Var2 == "Life Expectancy") {
                LE_table
            } else if (input$Var2 == "Wellbeing") {
                WB_table
            } else if (input$Var2 == "Inequality") {
                I_table
            } else {
                F_table
            }
        }
    )
    
    # Full Table
    InputFullTable <- reactive({
        
        # Filter by HPI
        min_HPI <- input$HPI_sld[1]
        max_HPI <- input$HPI_sld[2]
        
        hpifull <- hpi %>%
            filter(
                HPI >= min_HPI,
                HPI <= max_HPI
            )
        
        # Filter by Life Expectancy
        min_LE <- input$Life_sld[1]
        max_LE <- input$Life_sld[2]
        
        hpifull <- hpi %>%
            filter(
                Avg.Life.Expectancy >= min_LE,
                Avg.Life.Expectancy <= max_LE
            )
        
        # Filter by Wellbeing
        min_WB <- input$Well_sld[1]
        max_WB <- input$Well_sld[2]
        
        hpifull <- hpi %>%
            filter(Avg.Wellbeing >= min_WB,
                   Avg.Wellbeing <= max_WB
            )
        
        # Filter by Inequality
        min_I <- input$Ineq_sld[1]
        max_I <- input$Ineq_sld[2]
        
        hpifull <- hpi %>%
            filter(Inequality >= min_I,
                   Inequality <= max_I
            )
        
        # Filter by Footprint
        min_F <- input$CarbF_sld[1]
        max_F <- input$CarbF_sld[2]
        
        hpifull <- hpi %>%
            filter(Footprint.gha >= min_F,
                   Footprint.gha <= max_F
            )
        
        # Filter by Country
        if (!is.null(input$Country_tI) && input$Country_tI != "") {
            hpifull <- hpi %>%
                filter(Country %like% input$Country_tI)
        }
        
        hpifull <- data.table(as.data.frame(hpifull))
        hpifull <- hpifull[ , c("Rank", "Country", "Avg.Life.Expectancy", 
                                "Avg.Wellbeing", "Inequality", "Footprint.gha", "HPI", "GDP")]
        hpifull$Avg.Life.Expectancy <- round(hpifull$Avg.Life.Expectancy, 2)
        hpifull$Avg.Wellbeing <- round(hpifull$Avg.Wellbeing, 2)
        hpifull$Inequality <- round(hpifull$Inequality, 4) * 100
        hpifull$Footprint.gha <- round(hpifull$Footprint.gha, 2)
        hpifull$HPI <- round(hpifull$HPI, 2)
        hpifull$GDP <- round(hpifull$GDP, 0)
        names(hpifull) <- c("Rank", "Country", "Life Expectancy (years)", 
                            "Wellbeing", "Inequality (%)", "Footprint (gha)", "HPI", 
                            "GDP (USD)")
        
        hpifull
        
    }
    )
    
    output$FullTable <- renderDataTable(InputFullTable())
}