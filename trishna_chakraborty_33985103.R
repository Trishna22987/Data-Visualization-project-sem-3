library(shiny)
library(gridExtra)
library(viridis)
library(scales)
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
library(sf)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(tidyverse)
library(networkD3)
library(jsonlite)
library(igraph)
library(ggraph)
library(plotly)

# Loading the main dataset
geojson_file_path <- "Local_Authority_Districts_December_2021_UK_BFC_2022_-6196036569057273538.geojson"
cleaned_accident_data <- "Final_Clean_accident_rcrds.xlsx"

ui <- navbarPage(
  title = "UK Accident Data Analysis ", inverse = TRUE,
  
  tabPanel("Introduction", 
           fluidPage(
             h3("Population Density and Road Accidents: A District-Level Analysis for UK", align = "center",  style = "font-size: 24px; color: blue; font-weight: bold;"),
             h3("Name: Trishna Chakraborty |  Student ID: 33985103  |  Tutor: Jeffery Liu", align = "center",  style = "font-size: 20px; font-weight: bold;"),
             tags$p("In the media, news of road accidents is frequently observed, highlighting the need to understand the root causes behind them. This project seeks to explore the dynamics of various factors contributing to road accidents from the year 2021 to 2022 at the district level. By examining data on road accidents alongside district-wise population statistics, the aim is to shed light on how these factors influence the frequency and severity of road accidents. This analysis holds significance for urban planners, policymakers, and road safety advocates seeking to address road safety challenges in densely populated areas.", align = "center", style = "font-size: 18px;"),
             tags$p("The focused parameters for the districts include:", align = "center", style = "font-size: 18px;"),
             tags$p(
               "i. Accident Count", tags$br(),
               "ii. Population Density", tags$br(),
               "iii. Weather Conditions", tags$br(),
               "iv. Light Conditions", tags$br(),
               "v. Day, Month, Year", align = "center", style = "font-size: 18px;"
             )
           )
  ),
  
  
  tabPanel("Population Density and Accident Counts", 
           fluidPage(
             h3("Study of relation in Population Density and Accident Count of Districts", align = "center", style = "color: blue; font-weight: bold;"),
             fluidRow(
               tags$p("Note: the areas in Grey are not being considered, as they have been removed during the cleaning of the data.",
                      align = "center",style = "font-size: 12px; font-weight: bold;"),
               column(width = 6, 
                      plotOutput("mapPlotdensity", height = "600px", width = "100%"),
                      tags$p("This map illustrates the population density across UK districts. Notably, higher population densities are concentrated in urban areas such as Greater London, parts of the West Midlands, and the Manchester region, depicted by the darker blue shades. Conversely, rural areas in Scotland, Wales, and parts of Northern England exhibit lower population densities, shown in lighter blue. This spatial distribution highlights the urban-rural divide in population concentration, which is crucial for understanding and analyzing regional variations in infrastructure demands and road accident occurrences.",
                             align = "center", style = "font-size: 14px;")
               ),
               column(width = 6, 
                      plotOutput("mapPlotaccident", height = "600px", width = "100%"),
                      tags$p("This map displays the accident counts across UK districts, highlighting areas with varying levels of road accidents. The legend indicates that districts with higher accident counts are shown in red, while those with lower counts are in yellow and orange. Notably, regions with high accident counts are predominantly found in urban and densely populated areas such as parts of Greater London, West Midlands, and Manchester. In contrast, rural areas and districts in Scotland, Wales, and parts of Northern England exhibit lower accident counts. This distribution suggests a correlation between higher population densities and increased road accidents, underscoring the need for targeted road safety measures in urban centers.",
                             align = "center", style = "font-size: 14px;")
               ),
               h3("Correlation of the population density and Accident count:", style = "font-weight: bold;"),
               tags$p("There is a clear correlation between higher population densities and increased road accidents. Districts with darker shades of blue in the population density map align with regions displaying red and dark orange in the accident count map. Urban areas with higher population densities experience more road accidents, underscoring the impact of population density on road safety.",
                      style = "font-size: 14px;"),
               tags$p("Southern and central England (towards the bottom and middle of the map) have more districts with high accident counts compared to the northern regions and Scotland (top part of the map), which correlates with their higher population densities. In contrast, Scotland (top) and Wales (left side) show a predominance of low population density and correspondingly lower accident counts, highlighting regional disparities.",
                      align = "center", style = "font-size: 14px;"), tags$br(),
               tags$p("However, just comparing on the basis of the geographical distribution does not help to analyze the situation in depth and stand with the conclusion. Thus checking the top population density and the accident count districts in zoomed view.",
                      align = "center", style = "font-size: 14px;"), tags$br(),
                div(
                 actionButton("zoom_in", "Zoom in to Districts", style = "background-color: #343a40; color: white; border-color: #343a40;"),
                 style = "text-align: center; margin-bottom: 20px;"
               ),
               tags$br(),
               column(width = 6,
                      uiOutput("enlargedPlotOutput")),
               tags$br(),
               column(width = 6,
                      uiOutput("enlargedPlotAccidentOutput")),
               column(width = 12, "As visible from the above enlarged plot of the top three accident and population density districts. The districts are clearly different, and the districts with highest population densities shows a comparatively less accident counts for Hackney, Tower Hamlets, Islington
to that of Birmingham and Manchester. However the count from Westminster do not vary drastically." , tags$br(),align = "center", style = "font-size: 14px;" ,
                      "When the population densities for the highest accident districts are compared then Westminster is still not vary far.", tags$br(),align = "center", style = "font-size: 14px;" ,
                      "However the population density of the Manchester and Birmingham is comparatively less then that of Tower Hamlets, Islington.", tags$br(),align = "center", style = "font-size: 14px;" ,
                      "With the plots it can be said that, the population densities can influence the count of accidents, however the urban and rural aspects of the districts can cause more accidents than the population density of the region."
               )
             )
           )
  ),
  
  
  tabPanel("Time impact on Accident Count",
           fluidPage(
             h3("Trend in year 2021 between the High Accident, High Population Density Districts", align = "center", style = "color: blue; font-weight: bold;"),tags$br(),
             tags$p(
               "In order to understand is the population density has a strong influence on the Accident count of the Top accident prone and the densly populated areas, let's explore the impact of various other parameters on the Top 3 Population Density Districts and the top 3 Accident Prone Districts.",tags$br(),
               "The detailed analysis of the accidents in selected districts through out the year 2021 and 2022 can be visualized here: ",
               style = "font-size: 15px;"),
             column(width = 6, 
                    plotOutput("lineChart", height = "400px", width = "100%")),
             column(width = 6, 
                    plotOutput("lineChart_top3", height = "400px", width = "100%")),
             column(width = 6,
                    tags$p("The plot for the highest accident count districts shows a relatively stable trend in accident counts across the months, with no significant fluctuations. This suggests that in these districts, the factors contributing to high accident counts are consistently present throughout the year.", tags$br(), 
                           style = "font-size: 14px;")
             ),
             column(width = 6,
                    # tags$p("### Analysis of Accidents by Day of the Week for Highest Accident Count Districts", style = "font-size: 16px; font-weight: bold;"),
                    tags$p("Plot for the highest population density districts exhibits a steady trend with accident counts remaining flat across the months. This indicates that the accident risk in densely populated districts remains uniformly high throughout the year.", tags$br(), 
                           style = "font-size: 14px;"),
             ),tags$br(),
             column(width = 12,
                    tags$p("Correlation:",  align = "center",style = "font-size: 18px; font-weight: bold;"),
                    tags$p(
                           "The relation observed from the chloropleth map between the highest Accident Count and the Highest Population Density districts, is supported by the monthly trend analysis. Both highest accident count and highest density districts show consistent accident counts across different months, highlighting the persistent nature of the underlying risk factors. Te densly populated districts have a increased accident count in the Start of year where as the high accident prone districts like Manchester, Birmingham experince in the middle of the year span. However they both do not vary vary largely and also coincide for the month of Feburary and March.", tags$br(),
                           "Overall, High-risk districts, whether defined by accident counts or population density, experience steady accident rates in the early half of year 2021.",
                           style = "font-size: 14px;")
             ),
             column(width = 6, 
                    plotOutput("polarPlot_all", height = "400px", width = "100%")),
             column(width = 6, 
                    plotOutput("polarPlot", height = "400px", width = "100%")),
             column(width = 6,
                    
                    "Above Polar plot describes the accident counts in different day of the week. Here, Friday shows the highest number of accidents, followed by Thursday. This pattern suggests that towards the end of the workweek, these districts experience a spike in road accidents, potentially due to increased traffic volumes ", style = "font-size: 14px;"),
             
             column(width = 6,
                   
                    "Above Polar plot describes the accident counts in different day of the week. Here, Thursday exhibits the highest accident count, with Friday still showing a significant number of accidents but less than Thursday. This indicates that these densely populated areas face a higher risk of accidents towards the latter part of the week.", style = "font-size: 14px;"),
             tags$br(),tags$p("Correlation:",  align = "center",style = "font-size: 18px; font-weight: bold;"),
             column(width = 12,
                  
                  tags$p(
                    "Correlation between high population density and higher accident counts is reflected in the consistent trend of increased accidents towards the end of the workweek in both high accident count and high density districts making the parameter Day of the Week in sync with the monthly trend above and the chloropleth map.", tags$br(),
                    "This continuation from spatial to temporal analysis highlights the persistent nature of risk factors in these districts. Both types of districts see an uptick in accidents on Thursday and Friday, suggesting a need for targeted safety measures during these peak periods. Also, the parameter day of week shows that the districts on either category are prone equally to acacidents on the week ends.",
                    style = "font-size: 14px;"))
           
           )
      ),
  
  
  tabPanel("Weather and Light impact on Accident Count",
           fluidPage(
              h3("Impact of Weather and Light on High-Density, High-Accident Districts", align = "center", style = "color: blue; font-weight: bold;"),tags$br(),
              h4("Impact of Weather on High-Density, High-Accident Districts", align = "center", style = "color: steelblue; font-weight: bold;"),tags$br(),
              tags$p(
                "On checking the accidents over the year 2021 it can be seen that both the Highest Accident Count and Highest Population Density districst have the similar trend for both month and year. Let us furthur analyze the very strong considerable parameters Light and Weather conditiosn to see if they share the similar trend.",tags$br(),
                "The detailed analysis of the accidents in selected districts through out the year 2021 and 2022 fro different light and weather conditions can be visualized here: ",
                style = "font-size: 15px;"),
             column(width = 6, 
                    plotOutput("radialBarChart", height = "400px", width = "100%")),
             column(width = 6, 
                    plotOutput("radialBarChart_density", height = "400px", width = "100%")),
             column(width = 6,
                    # tags$p("### Analysis of Accidents by Day of the Week for Highest Accident Count Districts", style = "font-size: 16px; font-weight: bold;"),
                    "Above Bubble plot describes the influence of weather conditions on the accident count. Clearly indicating that most of the accidents occur under normal weather conditons. However, other adverse conditions also have a significant impact on accidents.", style = "font-size: 14px;"),
             column(width = 6,
                    # tags$p("### Analysis of Accidents by Day of the Week for Highest Accident Count Districts", style = "font-size: 16px; font-weight: bold;"),
                    "Above Bubble plot describes the influence of weather conditions on the accident count for the most populated districts. Clearly indicating that most of the accidents occur under normal weather conditons, which strongly indicates that incresed traffic volume due to population can be a major cause.", style = "font-size: 14px;"),
             tags$br(), tags$p("Correlation", style = "font-size: 18px; font-weight: bold;"),
             column(width = 12,
                    # tags$p("### Analysis of Accidents by Day of the Week for Highest Accident Count Districts", style = "font-size: 16px; font-weight: bold;"),
                    "Overall, both the high population and high density districts have the most number of cases under the normal weather conditions, however the districts with high accident counts also have a influnce of unknown possible adverse weather conditions. Which still shows the similar trend of the maps that the influence of density is moderate on the districts with higher number of accidents and other parameters here like weather conditions play a significant role.", style = "font-size: 14px;"),
             tags$br(),h4("Impact of Light on High-Density, High-Accident Districts", align = "center", style = "color: steelblue; font-weight: bold;"),tags$br(),
             column(width = 6, 
                    plotOutput("barPlot", height = "400px", width = "100%")),
             column(width = 6, 
                    plotOutput("barPlot_density", height = "400px", width = "100%")),
             tags$p("Correlation",  style = "font-size: 18px; font-weight: bold;"),
           column(width = 12,
                  # tags$p("### Analysis of Accidents by Day of the Week for Highest Accident Count Districts", style = "font-size: 16px; font-weight: bold;"),
                  "The Bar plots above depict the influence of light conditions on accident counts in districts with the highest accident counts and highest population density. In the highest accident count districts, accidents are more frequent during darkness with lights lit, while daylight conditions see fewer accidents and vice-versa in districts with the highest population density.", style = "font-size: 14px;",
                    tags$br(),
                  "Here, though reverse in the count by figures but both the plots have the same light conditions responsible for high accident count, supporting the trend of the moderate proportional relation of the population density and the Accident Count.", style = "font-size: 14px;"),
             
                  )
        ),
  
  tabPanel("All Parameters impact on Accident",
           fluidPage(
             h3("All parameters vision on Accident Severity", align = "center", style = "color: blue; font-weight: bold;"),
             column(width = 12,
                    "The Sankey diagram illustrates road accident data flow from the year through to accident severity. It starts with the year (2021 and 2022), progresses through months, days of the week, weather conditions, light conditions, and road surface conditions, and ends with accident severity (slight to fatal). This visual highlights how these factors collectively influence accident outcomes.", style = "font-size: 14px;"),
             column(width = 12, 
                    "Lets us now understand the influnce of all the possible parameters as listed above on the over all data including all the districts. Also let us visualize the trend on the distrists with the highest accident count and highest population density.")
             ,tags$br(),
             div(
             
             actionButton("show_density", "Highest Density", style = "background-color: #343a40; color: white; border-color: #343a40;"),
             actionButton("show_accidents", "Highest Accidents", style = "background-color: #343a40; color: white; border-color: #343a40;"),
             actionButton("Combinedresult", "Clubbed Analysis", style = "background-color: #343a40; color: white; border-color: #343a40;"),
           ),tags$br(),
             uiOutput("sankey_output"),
             # column(width = 4, plotOutput("shankeylegend", height = "100px", width = "100%")),
             column(width = 12, sankeyNetworkOutput("sankeyPlot", height = "700px", width = "100%")),
             column(width = 12,"
              The Sankey diagram above addresses question 2 of the analysis, which investigates whether various parameters contribute to increased accident counts. The diagram visualizes the flow and relationship between different features that can lead to accidents. A key observation is that the majority of accidents occurred in 2021, with a noticeably lower count in 2022. Examining monthly trends, accidents are more frequent at the beginning (January) and end (December) of the year, with a relatively consistent accident count from February to July. November and September appear to be safer months with fewer recorded accidents, likely due to stable weather conditions.",
              style = "font-size: 14px;", tags$br(),
              "When analyzing the days of the week, Friday has the highest accident count, possibly due to increased traffic volume as people prepare for the weekend. Sunday and Thursday also show higher accident counts, while other days have a more consistent count. Regarding weather conditions, most accidents occur on days with 'Fine no high winds,' suggesting that increased traffic density on clear days might be a significant factor. Rainy days also contribute significantly to accident counts.",
              style = "font-size: 14px;", tags$br(),
              "
              In terms of light conditions, accidents are most frequent during daylight hours, followed by periods of darkness with lit roads. Road conditions reveal that dry roads have the highest accident count, but wet and damp roads also show a considerable number of accidents. Regarding accident severity, the majority are classified as slight, followed by serious and fatal accidents. This indicates that while accidents are frequent, their severity has been mitigated, possibly due to improved road safety measures and effective handling of population density.
              ", style = "font-size: 14px;", tags$br(),
              "
              Overall, the Sankey diagram effectively illustrates the interplay between various parameters and accident occurrences, highlighting that while population density is a significant factor, other conditions like weather, light, and road conditions also play crucial roles in influencing accident rates.
              "
           )
             
          )
          
  ),
  tabPanel("Network Diagram",
           h3("Network Analysis of Accident Risk Classification by Population Density", align = "center", style = "color: blue; font-weight: bold;"),
           fluidRow(
             column(width = 12, "The network plot, here shows a clear and direct relation of the accident and the population density by forming clusters of the districts(green), population density wise, and gives a clubbed analysis of the accident severity(maroon) which are most frequent for those districts and the number of accident counts(orange) they have. This helps to see the final conclusive visualization of the data and analyze the final result which is impact of population density and accident count.", 
              style = "font-size: 14px;", tags$br(),
              "The most strong relation(the thicker nodes) stands with the slight accident severity type as been evident from the sankey diagram that the the majority of accident cases for the years have the slight severity, which is again very strongly linked to the medium and low density districts. This helps to clearly understand that the population density has a direct influence on the Accident Count for any District.",
              style = "font-size: 14px;", tags$br(),
              "As for the high density districts only they have a link with the Serious Accident Severity Type which is thin but still gives a very strong conclusion of how the population can be a reason for the severe accident cases. Also the relation of the acceptable and the low accident counts have a noticeable link width with the high population districts indicating that the districts are at the highest risk of accident when compared to low and average populated districts.",
              style = "font-size: 14px;", tags$br(),
              "Thus overall, based on the complete analysis it can be concluded that that districts with high population density have a high risk of accident with all types of accident severity (low, acceptable and severe)."),
             column(width = 4, plotOutput("legendPlot", height = "100px", width = "100%")),
             column(width = 12, forceNetworkOutput("networkPlot", height = "800px", width = "100%"))
           )
  )
   

)

# Define server logic
server <- function(input, output, session) {
  
  # Loading the dataset into variables 
  final_data <- read_excel(cleaned_accident_data) # full accident data 
  geo_data <- st_read(geojson_file_path) # all geo spatial data 
  
  # Ensure the columns are correctly identified
  if (!("Weather_Conditions" %in% colnames(final_data)) || !("Accident_Severity" %in% colnames(final_data))) {
    stop("Columns 'Weather_Conditions' or 'Accident_Severity' not found in the data frame.")
  }
  
  # Convert Accident_Severity to a factor if it's not already
  final_data$Accident_Severity <- as.factor(final_data$Accident_Severity)
  
  # Perform the ANOVA test
  anova_result <- aov(as.numeric(Accident_Severity) ~ Weather_Conditions, data = final_data)
  
  # Display the summary of the ANOVA test
  print("before test")
  print(nrow(final_data))
  print("the anova test results:")
  print(summary(anova_result))
  print("after test")
  print(nrow(final_data))
  
  # Clean the District column in final_data
  final_data$District <- tolower(final_data$District)
  
  # Clean the LAD21NM column in geo_data
  geo_data$LAD21NM <- tolower(geo_data$LAD21NM)
  
  # Remove duplicates to ensure distinct districts
  distinct_districts_data <- final_data %>%
    distinct(District, .keep_all = TRUE)
  
  # 
  # print("need check ")
  # print(head(distinct_districts_data))
  
  # Sort distinct_data by population_density
  sorted_data_density_Wise <- distinct_districts_data %>%
    arrange(desc(population_density))
  
  # Sort distinct_data by accident count
  sorted_data_accdnt_cnt_Wise <- distinct_districts_data %>%
    arrange(desc(Accident_Count))
  
  # Merge geo data with accident data
  merged_data <- geo_data %>%
    left_join(distinct_districts_data, by = c("LAD21NM" = "District"))
  # print("checking the merged data ")
  # print(merged_data)
  
  # the plot of density wise chloropleth map 
  output$mapPlotdensity <- renderPlot({
    
    # Improved color gradient
    density_colors <- scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue", midpoint = mean(merged_data$population_density, na.rm = TRUE), name = "Population Density")
    
    
    base_map_density <- ggplot(data = merged_data) +
      geom_sf(aes(fill = population_density), color = "black", size = 0.2) +
      density_colors +
      theme_void() +
      labs(
        title = "Population Density of UK Districts",
        
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1, "cm")
      )
    
    print(base_map_density)
  })
  
  # the plot of accident count wise chloropleth map 
  output$mapPlotaccident <- renderPlot({
    
    # Improved color gradient
    
    accident_colors <- scale_fill_gradient2(low = "lightyellow", mid = "orange", high = "red", midpoint = mean(merged_data$Accident_Count, na.rm = TRUE), name = "Accident Count")
    
    base_map_accident <- ggplot(data = merged_data) +
      geom_sf(aes(fill = Accident_Count), color = "black", size = 0.2) +
      accident_colors +
      theme_void() +
      labs(
        title = "Accident Count in UK Districts",
        
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        # plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1, "cm")
      )
    
    print(base_map_accident)
  })
  
  
  #selecting the top 5 density districts 
  sorted_data_density_Wise_top_3 <- sorted_data_density_Wise %>% head(3)
  sorted_data_accident_Wise_top_3 <- sorted_data_accdnt_cnt_Wise %>% head(3)
  print("checking the features:")
  # print(sorted_data_density_Wise_top_5)
  
  observeEvent(input$zoom_in, {
    output$enlargedPlotOutput <- renderUI({
      tagList(
        tags$br(), h3("Top 3 Highest Population Density Districts", align = "center", style = "color: blue; font-weight: bold;"),
        plotOutput("enlargedPlot", height = "600px", width = "100%")
      )
    })
    
    output$enlargedPlotAccidentOutput <- renderUI({
      tagList(
        h3("Top 3 Highest Accident Count Districts", align = "center", style = "color: red; font-weight: bold;"),
        plotOutput("enlargedPlot_accident", height = "600px", width = "100%")
      )
    })
  })
  
  # #the enlarged view of the districts 
  output$enlargedPlot <- renderPlot({
    enlarged_plots <- lapply(unique(sorted_data_density_Wise_top_3$District), function(district) {
      district_data <- merged_data %>% filter(LAD21NM == district)
      plot_enlarge_density <- ggplot(data = district_data) +
        geom_sf(aes(fill = population_density), color = "black", size = 0.4) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", guide = NULL) +
        theme_void() +
        labs(title = toupper(district)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14)
        ) +
        geom_text(aes(x = st_coordinates(st_centroid(geometry))[1], y = st_coordinates(st_centroid(geometry))[2],
                      label = paste("Pop. Density:", round(population_density, 5), "\nAccidents:", Accident_Count)),
                  size = 4, color = "black", fontface = "bold", nudge_y = 0.03)
      return(plot_enlarge_density)
    })
    grid.arrange(grobs = enlarged_plots, ncol = 2)
  })
  
  output$enlargedPlot_accident <- renderPlot({
    enlarged_plots <- lapply(unique(sorted_data_accident_Wise_top_3$District), function(district) {
      district_data <- merged_data %>% filter(LAD21NM == district)
      plot_enlarge_density <- ggplot(data = district_data) +
        geom_sf(aes(fill = population_density), color = "black", size = 0.4) +
        scale_fill_gradient(low = "lightyellow", high = "red", guide = NULL) +
        theme_void() +
        labs(title = toupper(district)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14)
        ) +
        geom_text(aes(x = st_coordinates(st_centroid(geometry))[1], y = st_coordinates(st_centroid(geometry))[2],
                      label = paste("Pop. Density:", round(population_density, 5), "\nAccidents:", Accident_Count)),
                  size = 4, color = "black", fontface = "bold", nudge_y = 0.03)
      return(plot_enlarge_density)
    })
    grid.arrange(grobs = enlarged_plots, ncol = 2)
  })
  
 
  # For all districts
  output$lineChart <- renderPlot({
    month_year_data <- sorted_data_accident_Wise_top_3 %>%
      group_by(Year, Month) %>%
      summarize(Accident_Count = n())
    
    # Define the correct order for the months
    month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    ggplot(month_year_data, aes(x = factor(Month, levels = month_levels), y = Accident_Count, color = as.factor(Year), group = Year)) +
      geom_line() +
      theme_minimal() +
      scale_color_manual(name = "Year", values = c("2021" = "red", "2022" = "purple")) +
      labs(title = "Monthly Accident Trends by Year for highest accident districts", x = "Month", y = "Accident Count") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(color = "black")
      ) +
      scale_y_continuous(limits = c(0, 10000))
  })
  
  # For highest accident prone districts
  output$lineChart_top3 <- renderPlot({
    month_year_data <- sorted_data_density_Wise_top_3 %>%
      group_by(Year, Month) %>%
      summarize(Accident_Count = n())
    
    # Define the correct order for the months
    month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    ggplot(month_year_data, aes(x = factor(Month, levels = month_levels), y = Accident_Count, color = as.factor(Year), group = Year)) +
      geom_line() +
      theme_minimal() +
      scale_color_manual(name = "Year", values = c("2021" = "red", "2022" = "purple")) +
      labs(title = "Monthly Accident Trends by Year for highest density districts", x = "Month", y = "Accident Count") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(color = "black")
      ) +
      scale_y_continuous(limits = c(0, 10000))
  })
  
  
  # day of week
  # for all districts 
  output$polarPlot_all  <- renderPlot({
    day_of_week_data <- sorted_data_accident_Wise_top_3 %>%
      group_by(Day_of_Week) %>%
      summarize(Accident_Count = n())
    
    ggplot(day_of_week_data, aes(x = Day_of_Week, y = Accident_Count, fill = Day_of_Week)) +
      geom_bar(stat = "identity", color = "black") +
      coord_polar(start = 0) +
      theme_minimal() +
      labs(title = "Day of the Week for highest accident districts") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 50, unit = "pt") 
      )
  })
  
  #for the highest accident prone districts 
  output$polarPlot <- renderPlot({
    day_of_week_data <- sorted_data_density_Wise_top_3 %>%
      group_by(Day_of_Week) %>%
      summarize(Accident_Count = n())
    
    ggplot(day_of_week_data, aes(x = Day_of_Week, y = Accident_Count, fill = Day_of_Week)) +
      geom_bar(stat = "identity", color = "black") +
      coord_polar(start = 0) +
      theme_minimal() +
      labs(title = "Day of the Week for highest density districts") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),plot.margin = margin(t = 10, r = 10, b = 10, l = 50, unit = "pt") 
      )
  })
  
  output$radialBarChart <- renderPlot({
    weather_condition_data <- sorted_data_accident_Wise_top_3 %>%
      filter(!is.na(Weather_Conditions)) %>%
      group_by(Weather_Conditions) %>%
      summarize(Accident_Count = n()) %>%
      arrange(desc(Accident_Count))
    
    ggplot(weather_condition_data, aes(x = Weather_Conditions, y = Accident_Count, size = Accident_Count, color = Weather_Conditions)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 15)) +
      theme_minimal() +
      labs(title = "Weather Condition influence on Highest Accident Count Districts", x = "Weather Conditions", y = "Accident Count") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(1, 2.5))  # Ensure y-axis labels are consistent
  })
  
  output$radialBarChart_density <- renderPlot({
    weather_condition_data <- sorted_data_density_Wise_top_3 %>%
      filter(!is.na(Weather_Conditions)) %>%
      group_by(Weather_Conditions) %>%
      summarize(Accident_Count = n()) %>%
      arrange(desc(Accident_Count))
    
    ggplot(weather_condition_data, aes(x = Weather_Conditions, y = Accident_Count, size = Accident_Count, color = Weather_Conditions)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 15)) +
      theme_minimal() +
      labs(title = "Weather Condition influence on Highest Population Density Districts", x = "Weather Conditions", y = "Accident Count") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5)
      ) +
      scale_y_continuous(limits = c(1, 2.5))  # Ensure y-axis labels are consistent
  })
  
  output$barPlot <- renderPlot({
  light_condition_data <- sorted_data_accident_Wise_top_3 %>%
    group_by(Light_Conditions) %>%
    summarize(Accident_Count = n()) %>%
    arrange(desc(Accident_Count))

  ggplot(light_condition_data, aes(x = reorder(Light_Conditions, -Accident_Count), y = Accident_Count, fill = Light_Conditions)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(discrete = TRUE) +  # Apply viridis color scale
    theme_minimal() +
    labs(title = "Light Condition influence on Highest Accident Count districts", x = "Light Conditions", y = "Accident Count") +
    scale_y_continuous(labels = comma) +  # Use comma format for y-axis labels
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.text.x = element_text( hjust = 1),
      legend.position = "none",
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5)
    )
  })
  output$barPlot_density <- renderPlot({
    light_condition_data <- sorted_data_density_Wise_top_3 %>%
      group_by(Light_Conditions) %>%
      summarize(Accident_Count = n()) %>%
      arrange(desc(Accident_Count))
    
    ggplot(light_condition_data, aes(x = reorder(Light_Conditions, -Accident_Count), y = Accident_Count, fill = Light_Conditions)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis(discrete = TRUE) +  # Apply viridis color scale
      theme_minimal() +
      labs(title = "Light Condition influence on Highest Population Density districts", x = "Light Conditions", y = "Accident Count") +
      scale_y_continuous(labels = comma) +  # Use comma format for y-axis labels
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text( hjust = 1),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5)
      )
  })
  
  #Shankey Diagram to understand the impact of all parameters on Accident count 
  
  final_data_sankey <- final_data %>%
    distinct(District, .keep_all = TRUE)
  
  
  final_data_sankey <- final_data_sankey %>%
    mutate(
      Year = as.integer(Year),
      Month = as.factor(Month),
      # population_density = as.numeric(population_density)
    )
  #to categorize the population density calculating the quantile
  quantiles <- quantile(final_data_sankey$population_density, probs = c(0.33, 0.66), na.rm = TRUE)
  
  # Filter for specified values as they do not have much influence
  final_data_sankey <- final_data_sankey %>%
    filter(
      Road_Surface_Conditions %in% c("Dry", "Frost or ice", "Wet or damp"),
      !Weather_Conditions %in% c("Snowing + high winds", "Other"),
      !Light_Conditions %in% c("Darkness - lights unlit", "Darkness - lighting unknown")
    )
  
  final_data_sankey <- final_data_sankey %>%
    mutate(
      population_density = case_when(
        population_density > quantiles[2] ~ "High",
        population_density > quantiles[1] ~ "Medium",
        TRUE ~ "Low"
      ),
      Accident_Count = case_when(
        Accident_Count > 3500 ~ "High",
        Accident_Count > 1000  ~ "Medium",
        TRUE ~ "Low"
      )
    )
  
  
  
  # Construct nodes and links for Sankey diagram
  nodes <- data.frame(name = unique(c(
    as.character(final_data_sankey$Year),
    as.character(final_data_sankey$Month),
    as.character(final_data_sankey$Day_of_Week),
    as.character(final_data_sankey$Weather_Conditions),
    as.character(final_data_sankey$Light_Conditions),
    as.character(final_data_sankey$Road_Surface_Conditions),
    as.character(final_data_sankey$Accident_Severity)
  )))
  
  # Initialize an empty data frame for links
  links <- data.frame()
  
  # Function to create links between nodes
  add_links <- function(source_col, target_col) {
    links <<- rbind(links, final_data_sankey %>%
                      group_by(across(all_of(c(source_col, target_col)))) %>%
                      summarise(count = n(), .groups = 'drop') %>%
                      transmute(
                        source = match(!!rlang::sym(source_col), nodes$name) - 1,
                        target = match(!!rlang::sym(target_col), nodes$name) - 1,
                        value = count
                      ))
  }
  
  # Add links for each level
  add_links("Year", "Month")
  add_links("Month", "Day_of_Week")
  add_links("Day_of_Week", "Weather_Conditions")
  add_links("Weather_Conditions", "Light_Conditions")
  add_links("Light_Conditions", "Road_Surface_Conditions")
  add_links("Road_Surface_Conditions", "Accident_Severity")
  
  
  # Prepare color scale
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  output$sankeyPlot <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  sinksRight = FALSE, colourScale = ColourScal,
                  nodeWidth = 40, fontSize = 13, nodePadding = 20,
                  width = "100%")
  })
  
  
  #Shankey Diagram to understand the impact of all parameters on Accident count
  
  #Shankey Diagram to understand the impact of all parameters on Accident count for the top population density districts
  
  sorted_data_density_Wise_top_3_shankey <- sorted_data_density_Wise_top_3 %>%
    distinct(District, .keep_all = TRUE)
  
  
  sorted_data_density_Wise_top_3_shankey <- sorted_data_density_Wise_top_3_shankey %>%
    mutate(
      Year = as.integer(Year),
      Month = as.factor(Month),
      # population_density = as.numeric(population_density)
    )
  #to categorize the population density calculating the quantile
  quantiles <- quantile(sorted_data_density_Wise_top_3_shankey$population_density, probs = c(0.33, 0.66), na.rm = TRUE)
  
  # Filter for specified values as they do not have much influence
  sorted_data_density_Wise_top_3_shankey <- sorted_data_density_Wise_top_3_shankey %>%
    filter(
      Road_Surface_Conditions %in% c("Dry", "Frost or ice", "Wet or damp"),
      !Weather_Conditions %in% c("Snowing + high winds", "Other"),
      !Light_Conditions %in% c("Darkness - lights unlit", "Darkness - lighting unknown")
    )
  
  sorted_data_density_Wise_top_3_shankey <- sorted_data_density_Wise_top_3_shankey %>%
    mutate(
      population_density = case_when(
        population_density > quantiles[2] ~ "High",
        population_density > quantiles[1] ~ "Medium",
        TRUE ~ "Low"
      ),
      Accident_Count = case_when(
        Accident_Count > 3500 ~ "High",
        Accident_Count > 1000  ~ "Medium",
        TRUE ~ "Low"
      )
    )
  
  
  
  # Construct nodes and links for Sankey diagram
  nodes_acc <- data.frame(name = unique(c(
    as.character(sorted_data_density_Wise_top_3_shankey$Year),
    as.character(sorted_data_density_Wise_top_3_shankey$Month),
    as.character(sorted_data_density_Wise_top_3_shankey$Day_of_Week),
    as.character(sorted_data_density_Wise_top_3_shankey$Weather_Conditions),
    as.character(sorted_data_density_Wise_top_3_shankey$Light_Conditions),
    as.character(sorted_data_density_Wise_top_3_shankey$Road_Surface_Conditions),
    as.character(sorted_data_density_Wise_top_3_shankey$Accident_Severity)
  )))
  
  # Initialize an empty data frame for links
  links_Acc <- data.frame()
  
  # Function to create links between nodes
  add_links_acc <- function(source_col, target_col) {
    links_Acc <<- rbind(links_Acc, sorted_data_density_Wise_top_3_shankey %>%
                          group_by(across(all_of(c(source_col, target_col)))) %>%
                          summarise(count = n(), .groups = 'drop') %>%
                          transmute(
                            source = match(!!rlang::sym(source_col), nodes_acc$name) - 1,
                            target = match(!!rlang::sym(target_col), nodes_acc$name) - 1,
                            value = count
                          ))
  }
  
  # Add links for each level
  add_links_acc("Year", "Month")
  add_links_acc("Month", "Day_of_Week")
  add_links_acc("Day_of_Week", "Weather_Conditions")
  add_links_acc("Weather_Conditions", "Light_Conditions")
  add_links_acc("Light_Conditions", "Road_Surface_Conditions")
  add_links_acc("Road_Surface_Conditions", "Accident_Severity")
  
  
  # Prepare color scale
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  output$sankeyPlot_top_3_density <- renderSankeyNetwork({
    sankeyNetwork(Links = links_Acc, Nodes = nodes_acc,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  sinksRight = FALSE, colourScale = ColourScal,
                  nodeWidth = 40, fontSize = 13, nodePadding = 20,
                  width = "100%")
  })
  
  
  #top accident districts
  #Shankey Diagram to understand the impact of all parameters on Accident count
  
  sorted_data_accident_Wise_top_3_shankey <- sorted_data_accident_Wise_top_3 %>%
    distinct(District, .keep_all = TRUE)
  
  
  sorted_data_accident_Wise_top_3_shankey <- sorted_data_accident_Wise_top_3_shankey %>%
    mutate(
      Year = as.integer(Year),
      Month = as.factor(Month),
      # population_density = as.numeric(population_density)
    )
  #to categorize the population density calculating the quantile
  quantiles <- quantile(sorted_data_accident_Wise_top_3_shankey$population_density, probs = c(0.33, 0.66), na.rm = TRUE)
  
  # Filter for specified values as they do not have much influence
  sorted_data_accident_Wise_top_3_shankey <- sorted_data_accident_Wise_top_3_shankey %>%
    filter(
      Road_Surface_Conditions %in% c("Dry", "Frost or ice", "Wet or damp"),
      !Weather_Conditions %in% c("Snowing + high winds", "Other"),
      !Light_Conditions %in% c("Darkness - lights unlit", "Darkness - lighting unknown")
    )
  
  sorted_data_accident_Wise_top_3_shankey <- sorted_data_accident_Wise_top_3_shankey %>%
    mutate(
      population_density = case_when(
        population_density > quantiles[2] ~ "High",
        population_density > quantiles[1] ~ "Medium",
        TRUE ~ "Low"
      ),
      Accident_Count = case_when(
        Accident_Count > 3500 ~ "High",
        Accident_Count > 1000  ~ "Medium",
        TRUE ~ "Low"
      )
    )
  
  
  
  # Construct nodes and links for Sankey diagram
  nodes_accident <- data.frame(name = unique(c(
    as.character(sorted_data_accident_Wise_top_3_shankey$Year),
    as.character(sorted_data_accident_Wise_top_3_shankey$Month),
    as.character(sorted_data_accident_Wise_top_3_shankey$Day_of_Week),
    as.character(sorted_data_accident_Wise_top_3_shankey$Weather_Conditions),
    as.character(sorted_data_accident_Wise_top_3_shankey$Light_Conditions),
    as.character(sorted_data_accident_Wise_top_3_shankey$Road_Surface_Conditions),
    as.character(sorted_data_accident_Wise_top_3_shankey$Accident_Severity)
  )))
  
  # Initialize an empty data frame for links
  links_Accident <- data.frame()
  
  # Function to create links between nodes
  add_links_Accident <- function(source_col, target_col) {
    links_Accident <<- rbind(links_Accident, sorted_data_accident_Wise_top_3_shankey %>%
                               group_by(across(all_of(c(source_col, target_col)))) %>%
                               summarise(count = n(), .groups = 'drop') %>%
                               transmute(
                                 source = match(!!rlang::sym(source_col), nodes_accident$name) - 1,
                                 target = match(!!rlang::sym(target_col), nodes_accident$name) - 1,
                                 value = count
                               ))
  }
  
  # Add links for each level
  add_links_Accident("Year", "Month")
  add_links_Accident("Month", "Day_of_Week")
  add_links_Accident("Day_of_Week", "Weather_Conditions")
  add_links_Accident("Weather_Conditions", "Light_Conditions")
  add_links_Accident("Light_Conditions", "Road_Surface_Conditions")
  add_links_Accident("Road_Surface_Conditions", "Accident_Severity")
  
  
  # Prepare color scale
  ColourScal <- 'd3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  output$sankeyPlot_accidents <- renderSankeyNetwork({
    sankeyNetwork(Links = links_Accident, Nodes = nodes_accident,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  sinksRight = FALSE, colourScale = ColourScal,
                  nodeWidth = 40, fontSize = 13, nodePadding = 20,
                  width = "100%")
  })
  
  
  observeEvent(input$show_density, {
    output$sankey_output <- renderUI({
      
      column(width = 12, 
             h3("Sankey Diagram Analysis for Top 3 Population Density Districts", align = "center",  style = "font-size: 24px; color: blue; font-weight: bold;"),
             sankeyNetworkOutput("sankeyPlot_top_3_density", height = "500px", width = "100%"),
      tags$div(
        "The Sankey diagram above visualizes the relationship between various factors and road accidents for the top 3 accident prone districts, intending to understand how apart from population density, the other parameters can lead to accident. This shows a combined view of how all factors can lead to accident.",
        style = "font-size: 14px;",
        tags$br(),
        "Starting with the yearly distribution, it is evident that 2021 experienced more accidents compared to 2022. This suggests that specific events or conditions in 2021 might have contributed to a higher accident rate. In continuation, the monthly trends show January and March stand out with significant accident counts. This could be attributed to extreme weather conditions or the adjustment period post-holidays. When considering the days of the week, Thursday and Friday are notable for their higher accident counts, which has been studied in detail before with the polar charts. This pattern could be linked to the end-of-week rush, where increased traffic volume and possibly higher levels of fatigue or distraction among drivers might lead to more accidents.",
        style = "font-size: 14px;",
        tags$br(),
        "Weather conditions show that most accidents occurred on 'Fine no high winds' days. This indicates that normal weather conditions with higher traffic volumes can be critical. Rainy and misty conditions, though less frequent, still account for a significant number of accidents, highlighting the impact of weather variability on road safety. Further in terms of light conditions, daylight hours see the highest number of accidents, followed by periods of darkness with lit roads. This might be due to higher traffic volumes during the day when people are commuting to work, school, or other activities. However, darkness with street lighting still poses a risk, possibly due to reduced visibility and driver fatigue during evening hours.",
        style = "font-size: 14px;",
        tags$br(),
        "Road conditions reveal that dry roads have the highest accident counts, likely because more vehicles are on the road during favorable driving conditions. Wet and damp roads also contribute to accidents, reflecting the challenge of maintaining vehicle control in slippery conditions.",
        style = "font-size: 14px;",
        tags$br(),
        "Lastly, the severity of accidents shows that the majority are classified as slight, followed by serious and fatal accidents. This distribution indicates that while many accidents are minor, a significant number result in serious injuries or fatalities.",
        style = "font-size: 14px;",
        tags$br(),
        "Overall, the Sankey diagram effectively illustrates that while population density is a significant factor, other conditions like the time of year, specific days, weather conditions, light conditions, and road surface conditions also play crucial roles in influencing accident rates.",
        style = "font-size: 14px;"
      ))
    })
    
    })
  
    
   
  
  observeEvent(input$show_accidents, {
    output$sankey_output <- renderUI({
      column(width = 12, 
             h3("Sankey Diagram Analysis for Top 3 Accident Prone Districts", align = "center",  style = "font-size: 24px; color: blue; font-weight: bold;"),
             sankeyNetworkOutput("sankeyPlot_accidents", height = "500px", width = "100%"),
      tags$div("The Sankey diagram illustrates accident data for districts with the highest population density, highlighting how various factors influence accident numbers."
             ,style = "font-size: 14px;", tags$br(),
             "In 2021, accidents were most frequent, particularly in March and May, likely due to higher traffic volumes and possibly adverse weather conditions. Thursdays and Fridays saw more accidents, suggesting that end-of-week traffic increases accident risks."
             ,style = "font-size: 14px;", tags$br(),
             "Weather conditions also play a role, with 'Fine no high winds' days having the highest accident counts. This indicates that even in good weather, high traffic density can lead to more accidents. Daylight hours saw the most accidents, reflecting heavy traffic during the day, while lit conditions at night also posed risks."
             ,style = "font-size: 14px;", tags$br(),
             "Dry roads experienced the highest accident rates, but wet or damp conditions were also significant, showing that road conditions combined with high population density affect accident likelihood."
             ,style = "font-size: 14px;", tags$br(),
             "The severity of accidents was mostly slight, but districts with high population density also saw serious and fatal accidents, emphasizing the impact of population density along with other factors on accident rates."
             ))
    })
  })
  
  observeEvent(input$Combinedresult, {
    output$sankey_output <- renderUI({
      h3("Analysis of Population Density with other parameters on Accident Type", align = "center",  style = "font-size: 24px; color: blue; font-weight: bold;")
      column(width = 12, "When seen overall results for both the highest Population Density and Accident Count it supports the narrative of enlarged district wise choropleths for the accident count and population density."
             ,style = "font-size: 14px;", tags$br(),
             "For districts with the highest accident counts, 2021 saw the majority of incidents, particularly in January and March. Thursdays and Fridays were peak days, with most accidents occurring in daylight and fine weather conditions and smaller fraction in the other unknown and possibly adverse conditions, highlighting how high traffic volumes contribute to accidents. Dry road conditions saw the highest accident rates, but wet conditions were also significant, reflecting that even favorable weather cannot mitigate the risks posed by high traffic volumes."
             ,style = "font-size: 14px;", tags$br(),
             "In districts with the highest population density, 2021 also recorded the most accidents, with March and May standing out. Similar trends were observed with higher accident rates on Thursdays and Fridays, and during daylight hours. 'Fine no high winds' days and dry road conditions saw the most accidents, underscoring how dense traffic in good weather still leads to accidents. Wet or damp conditions also contributed significantly to accidents.",
    style = "font-size: 14px;", tags$br(),
      "Overall, the similar trends for all the available parameters it can be concluded that the population density has definetly an influence on accident counts when the districts have a above average to high accident count. However, the other parameters play a very significant role for both the categories.")
    })
  })
  
  #network plot 
  
  distinct_districts_data_nd <- final_data %>%
    distinct(District, .keep_all = TRUE)
  
  quantiles <- quantile(distinct_districts_data_nd$population_density, probs = c(0.33, 0.66), na.rm = TRUE)
  distinct_districts_data_nd <- distinct_districts_data_nd %>%
    mutate(
      population_density = case_when(
        population_density > quantiles[2] ~ "High Density",
        population_density > quantiles[1] ~ "Medium Density",
        TRUE ~ "Low Density"
      ),
      Accident_Count = case_when(
        Accident_Count > 3500 ~ "Highest Accident Count",
        Accident_Count > 1000  ~ "Acceptable Accident Count",
        TRUE ~ "Least Accident Count"
      )
    )
  
  #relationship between the population density and the accident count
  # Construct nodes and links for the network diagram
  nodes_network_dgrm <- data.frame(name = unique(c(
    as.character(distinct_districts_data_nd$Accident_Count),
    as.character(distinct_districts_data_nd$population_density),
    as.character(distinct_districts_data_nd$Accident_Severity),
    as.character(distinct_districts_data_nd$District)
  )))
  
  nodes_network_dgrm <- nodes_network_dgrm %>%
    mutate(group = case_when(
      name %in% c("High Density", "Medium Density", "Low Density") ~ "Population Density",
      name %in% c("Highest Accident Count", "Acceptable Accident Count", "Least Accidennt Count") ~ "Accident Count",
      name %in% distinct_districts_data_nd$District ~ "District",
      TRUE ~ "Accident Severity"
    ))
  
  # Initialize an empty data frame for links
  links_nd <- data.frame()
  
  # Function to create links between nodes
  add_links_nd <- function(source_col, target_col) {
    links_nd <<- rbind(links_nd, distinct_districts_data_nd %>%
                         group_by(across(all_of(c(source_col, target_col)))) %>%
                         summarise(count = n(), .groups = 'drop') %>%
                         transmute(
                           source = match(!!rlang::sym(source_col), nodes_network_dgrm$name) - 1,
                           target = match(!!rlang::sym(target_col), nodes_network_dgrm$name) - 1,
                           value = count
                         ))
  }
  
  # Add links for each relationship
  add_links_nd("District", "population_density")
  add_links_nd("population_density", "Accident_Count")
  add_links_nd("Accident_Count", "Accident_Severity")
  
  #     # Ensure links_nd is not empty
  #     print("Checking links_nd:")
  #     print(links_nd)
  #
  #     # Print the structure of nodes_network_dgrm and links_nd
  #     print("Structure of nodes_network_dgrm:")
  #     print(str(nodes_network_dgrm))
  #
  #     print("Structure of links_nd:")
  #     print(str(links_nd))
  
  # Custom color scale
  ColourScal_nd <- 'd3.scaleOrdinal()
                .domain(["District", "Population Density", "Accident Count", "Accident Severity"])
                .range(["green", "blue", "orange", "maroon"])'
  
  output$networkPlot <- renderForceNetwork({
    forceNetwork(Links = links_nd, Nodes = nodes_network_dgrm,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8,
                 fontSize = 14, zoom = TRUE,
                 colourScale = JS(ColourScal_nd))
  })
  
  output$legendPlot <- renderPlot({
    
    legend_data <- data.frame(
      label = c("Accident Severity", "Accident Count", "   Districts", "Population Density"),
      color = c("maroon", "orange", "green", "blue")
    )
    
    legend_plot <- ggplot(legend_data, aes(x = 1, y = label, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +
      theme_void() +
      theme(
        legend.position = "top",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
      ) +
      geom_text(aes(label = label), hjust = -0.5, size = 5)
    
    return(legend_plot)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
