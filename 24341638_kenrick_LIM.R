#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Make sure to Run the Rmd file first!!!!!!!!!!!!!!

library(shiny)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(reshape2)
library(fpc)
library(factoextra)
library(lime)
library(ROCit)
library(fpc)
library(gridExtra)
library(rpart)
library(caret) 


# Define UI
ui <- fluidPage(
  titlePanel("Countries and Death From GDP and Population Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Countries/Demographic Stats"),
      
      # Radio button for EDA, Classification, Clustering
      radioButtons("choice", "Select Analysis Type:", 
                   choices = c("EDA", "Classification", "Clustering")),
      
      # Conditional panel for EDA options
      conditionalPanel(
        condition = "input.choice == 'EDA'",
        selectInput("eda_plot_type", "Select EDA Plot Type:", 
                    choices = c("Scatter Plot", "Histogram", "Boxplot", "Map", "Stacked Bar Chart"))
      ),
      
      # Conditional panel for Classification options
      conditionalPanel(
        condition = "input.choice == 'Classification'",
        selectInput("classification_type", "Select Classification Model:", 
                    choices = c("Decision Tree", "Logistic Regression","Best Model")),
        
        # Dynamically show the outputs based on the selected classification type
        uiOutput("classification_output_ui")
      ),
      
      # Conditional panel for Clustering options
      conditionalPanel(
        condition = "input.choice == 'Clustering'",
        selectInput("num_clusters", "Select Number of Clusters:", 
                    choices = c("2", "3", "4", "5")),
        radioButtons("clustering_criterion", "Select Clustering Criterion:", 
                     choices = c("Calinski-Harabasz" = "ch", "Silhouette Width" = "asw"))
      )
    ),
    
    mainPanel(
      h3("Analysis Result"),
      
      # Conditional rendering of tabs when classification is selected
      conditionalPanel(
        condition = "input.choice == 'Classification'",
        # If "Best Model" is selected, show only ROC and Performance without tab panels
        conditionalPanel(
          condition = "input.model_choice == 'Best Model'",
          plotOutput("roc_plot"),         # Show ROC curve
          verbatimTextOutput("performance_output")  # Show Performance Measures
        ),
        
        # For other models, show the tabs
        conditionalPanel(
          condition = "input.model_choice != 'Best Model'",
          tabsetPanel(
            id = "classification_tabs",
            tabPanel("LIME", plotOutput("lime_plot")),
            tabPanel("ROC", plotOutput("roc_plot")),
            tabPanel("Performance Measure", verbatimTextOutput("performance_output"))
      )
    )
  ),
  # Render different outputs for EDA and Clustering based on user choice
  conditionalPanel(
    condition = "input.choice == 'EDA'",
    plotOutput("main_plot")  # EDA-specific plot/output
  ),
  
  conditionalPanel(
    condition = "input.choice == 'Clustering'",
    plotOutput("clustering_plot"),  # Clustering plot
    verbatimTextOutput("optimal_clusters")  # Clustering optimal clusters output
  )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    # Ensure the analysis type is selected
    req(input$choice)
    
    if (input$choice == "EDA") {
      
      # Ensure the plot type is selected in the UI
      req(input$eda_plot_type)
      
      # Scatter Plot
      if (input$eda_plot_type == "Scatter Plot") {
        ggplot(merged_data3, aes(x = Smoking, y = GDP_per_capita)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_smooth() +
          labs(title = "Scatter Plot of Smoking vs GDP per Capita", 
               x = "Smoking (Cigarettes per Adult per Day)", 
               y = "GDP per Capita") +
          theme_minimal() +
          
          # Annotation for high GDP per capita outliers with low smoking rates
          annotate("text", x = 5, y = 150000, label = "High GDP per Capita in Countries with Low Smoking\n(E.g., Luxembourg, Norway)", 
                   size = 3, color = "red", fontface = "bold", hjust = 0) +
          annotate("segment", x = 5, xend = 6, y = 150000, yend = 125000, 
                   color = "red", arrow = arrow(length = unit(0.2, "cm"))) +
          
          # Annotation for large smoking rates and lower GDP per capita
          annotate("text", x = 20, y = 10000, label = "High Smoking with Lower GDP per Capita\n(E.g., Russia, Indonesia)", 
                   size = 3, color = "red", fontface = "bold", hjust = 1) +
          annotate("segment", x = 20, xend = 19, y = 10000, yend = 9000, 
                   color = "red", arrow = arrow(length = unit(0.2, "cm"))) +
          
          # Annotation for the general trend line
          annotate("text", x = 10, y = 70000, label = "General Trend:\nHigher Smoking Rates Correlate\nwith Lower GDP per Capita", 
                   size = 3, color = "blue", fontface = "italic", hjust = 0) +
          
          # Annotation for mid-level smoking rates and moderate GDP
          annotate("text", x = 12, y = 50000, label = "Moderate Smoking and GDP\n(E.g., Brazil, Turkey)", 
                   size = 3, color = "darkgreen", fontface = "italic", hjust = 0) +
          annotate("segment", x = 12, xend = 11, y = 50000, yend = 45000, 
                   color = "darkgreen", arrow = arrow(length = unit(0.2, "cm"))) +
          
          scale_x_continuous(labels = scales::comma)  # Format x-axis for readability
      }
      
      else if (input$eda_plot_type == "Histogram") {
        
        # Original Histogram for Population
        p1 <- ggplot(merged_data3, aes(x = Population)) +
          geom_histogram(binwidth = 50000000, fill = "steelblue", color = "black") +
          labs(title = "Population Distribution", 
               x = "Population", 
               y = "Frequency") +
          theme_minimal()
        
        # Log-Scaled Histogram for Population
        p2 <- ggplot(merged_data3, aes(x = Population)) +
          geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black") +
          labs(title = "Log-Scaled Population Distribution", 
               x = "Log(Population)", 
               y = "Frequency") +
          theme_minimal() +
          scale_x_log10(labels = scales::comma) +  # Log scale for x-axis and format numbers
          ylim(0, 1000) +  # Maximum frequency capped at 1000
          annotate("text", x = 10^7, y = 900, label = "Most frequent range", color = "black", size = 4, fontface = "italic") +
          annotate("segment", x = 10^7, xend = 10^7, y = 0, yend = 900, color = "black", linetype = "dashed")  # Dashed line at the frequent range
        
        # Arrange the two plots side by side
        grid.arrange(p1, p2, ncol = 2)
      }
      
      else if (input$eda_plot_type == "Boxplot") {
        
        # Regular Boxplot for GDP per Capita by Death Rate Class
        p1 <- ggplot(merged_data3, aes(x = Death_Rate_Class, y = GDP_per_capita, fill = Death_Rate_Class)) +
          geom_boxplot() +
          labs(title = "GDP per Capita by Death Rate Classification", 
               x = "Death Rate Class", 
               y = "GDP per Capita") +
          theme_minimal() +
          annotate("text", x = 1, y = 50000, label = "Outlier", color = "red", size = 4, fontface = "italic") +
          annotate("segment", x = 1, xend = 1, y = 10000, yend = 50000, color = "red", arrow = arrow(length = unit(0.2, "cm")))
        
        # Log-Scaled Boxplot for GDP per Capita by Death Rate Class
        p2 <- ggplot(merged_data3, aes(x = Death_Rate_Class, y = GDP_per_capita, fill = Death_Rate_Class)) +
          geom_boxplot() +
          scale_y_log10() +
          labs(title = "GDP per Capita by Death Rate Classification (Log-Scaled)", 
               x = "Death Rate Class", 
               y = "log(GDP per Capita)") +
          theme_minimal() +
          annotate("text", x = 1, y = 50000, label = "Outlier", color = "red", size = 4, fontface = "italic") +
          annotate("segment", x = 1, xend = 1, y = 10000, yend = 50000, color = "red", arrow = arrow(length = unit(0.2, "cm")))
        
        # Arrange the two plots side by side
        grid.arrange(p1, p2, ncol = 2)
      }
      else if (input$eda_plot_type == "Map") {
        # Map Viz-Load world map data
        world <- ne_countries(scale = "medium", returnclass = "sf")
        
        # Rename "United States" to "United States of America" in your dataset
        merged_data_US <- merged_data3 %>%
          mutate(Country = ifelse(Country == "United States", "United States of America", Country),
                 Country = ifelse(Country == "South Sudan", "S. Sudan", Country),
                 Country = ifelse(Country == "Central African Republic", "Central African Rep.", Country),
                 Country = ifelse(Country == "Eswatini", "eSwatini", Country),
                 Country = ifelse(Country == "Micronesia (country)", "Micronesia", Country),
                 Country = ifelse(Country == "East Timor", "Timor-Leste", Country),
                 Country = ifelse(Country == "Dominican Republic", "Dominican Rep.", Country),
                 Country = ifelse(Country == "Democratic Republic of Congo", "Dem. Rep. Congo", Country),
                 Country = ifelse(Country == "Cape Verde", "Cabo Verde", Country),
                 Country = ifelse(Country == "Antigua and Barbuda", "Antigua and Barb.", Country),
                 Country = ifelse(Country == "Bosnia and Herzegovina", "Bosnia and Herz.", Country),
                 Country = ifelse(Country == "Cote d'Ivoire", "Côte d'Ivoire", Country),
                 Country = ifelse(Country == "Equatorial Guinea", "Eq. Guinea", Country),
                 Country = ifelse(Country == "Marshall Islands", "Marshall Is.", Country),
                 Country = ifelse(Country == "Northern Mariana Islands", "N. Mariana Is.", Country),
                 Country = ifelse(Country == "Saint Kitts and Nevis", "St. Kitts and Nevis", Country),
                 Country = ifelse(Country == "Saint Vincent and the Grenadines", "St. Vin. and Gren.", Country),
                 Country = ifelse(Country == "Sao Tome and Principe", "São Tomé and Principe", Country),
                 Country = ifelse(Country == "Solomon Islands", "Solomon Is.", Country))
        
        # Prepare the dataset for merging
        map_data <- merged_data_US %>%
          dplyr::select(Country, Smoking) %>%
          group_by(Country) %>%
          summarise(Smoking = mean(Smoking, na.rm = TRUE))  # Ensure you are using valid data
        
        # Merge the map data with the world data
        world_data <- left_join(world, map_data, by = c("name" = "Country"))
        
        # Plot the map with Smoking data
        ggplot(data = world_data) +
          geom_sf(aes(fill = Smoking)) +
          scale_fill_viridis_c(option = "C", na.value = "grey50") +
          labs(title = "Global Impact of Smoking on Mortality", fill = "Smoking") +
          theme_minimal() +
          annotate("text", x = 104.1954, y = 35.8617, label = "Highest smoking death", color = "black", size = 3, fontface = "bold") +
          annotate("segment", x = 104.1954, xend = 120, y = 35.8617, yend = 35, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
          annotate("text", x = 120.9605, y = 23.6978, label = "Taiwan is part of China (not a country)", color = "black", size = 2.5, fontface = "italic") +
          annotate("segment", x = 120.9605, xend = 120, y = 23.6978, yend = 25, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
          annotate("text", x = -42.6043, y = 71.7069, label = "Greenland: Territory owned by Denmark", color = "red", size = 2.5, fontface = "italic") +
          annotate("segment", x = -42.6043, xend = -40, y = 71.7069, yend = 70, color = "red", arrow = arrow(length = unit(0.2, "cm"))) +
          annotate("text", x = 0, y = -75, label = "Antarctica is a Continent", color = "red", size = 3, fontface = "italic")
      }
      else if (input$eda_plot_type == "Stacked Bar Chart") {
        # Assuming multiple death causes like Air Pollution, Blood Pressure, and Sodium Intake
        death_causes <- merged_data3[, c("Outdoor.air.pollution", "High.systolic.blood.pressure", "Diet.high.in.sodium", 
                                         "Diet.low.in.whole.grains","Alochol.use","Unsafe.water.source","Secondhand.smoke",
                                         "Low.birth.weight","Diet.low.in.fruits","Child.wasting","Diet.low.in.nuts.and.seeds","Iron.deficiency", 
                                         "Unsafe.sex","Household.air.pollution.from.solid.fuels","Diet.low.in.Vegetables","Low.physical.activity", 
                                         "Smoking", "High.fasting.plasma.glucose","Air.pollution","High.body.mass.index","Unsafe.sanitation",
                                         "No.access.to.handwashing.facility","Drug.use" ,"Low.bone.mineral.density","Vitamin.A.deficiency", 
                                         "Child.stunting","Discontinued.breastfeeding","Non.exclusive.breastfeeding","Death_Rate_Class")]
        
        # Melt data for ggplot
        death_causes_melted <- melt(death_causes, id.vars = "Death_Rate_Class", 
                                    variable.name = "Death_Cause", value.name = "Value")
        
        # Stacked bar chart with Death Cause on the x-axis and Death Rate Classification stacked
        ggplot(death_causes_melted, aes(x = Death_Cause, y = Value, fill = Death_Rate_Class)) +
          geom_bar(stat = "identity", position = "fill") +
          labs(title = "Stacked Bar Chart of Health Causes by Death Rate Classification",
               x = "Death Causes", y = "Contribution of Health Causes") +
          theme_minimal() +
          coord_flip()+
          scale_fill_brewer(palette = "Set2") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      }
    }
  })
  
 
  # Add other plots and render functions as needed for Classification, Clustering, etc.
  
  # LIME Plot Output for Classification Models
  output$lime_plot <- renderPlot({
    req(input$classification_type)  # Ensure input is available before proceeding
    
    if (input$classification_type == "Decision Tree") {
      # LIME for Decision Tree
      explainer_dt <- lime(train_data_selected, tree_model_threshold)  # train_data_selected is global, tree_model_threshold is global
      explanation_dt <- explain(test_data_selected[1:5, ], explainer_dt, n_labels = 1, n_features = 5)  # test_data_selected is global
      plot_features(explanation_dt)
        
    } else if (input$classification_type == "Logistic Regression") {
      # LIME for Logistic Regression
      explainer_log <- lime(train_data_log, logistic_model)  # train_data_log is global, logistic_model is global
      explanation_log <- explain(test_data_log2[1:5, ], explainer_log, n_labels = 1, n_features = 5)  # test_data_log2 is global
      plot_features(explanation_log)
    }
  })
  
  output$roc_plot <- renderPlot({
    req(input$classification_type)  # Ensure input is available

    if (input$classification_type == "Decision Tree") {
      # Predict probabilities for Decision Tree
      predictions_prob_test_original <- predict(tree_model, newdata = test_data2, type = "prob")[, 2]   # Original model
      predictions_prob_test_refined <- predict(tree_model_threshold, newdata = test_data_selected, type = "prob")[, 2]  # Refined model
      
      # ROC Curves using ROCit for test data
      rocit_test_original <- rocit(score = predictions_prob_test_original, class = test_data2$Death_Rate_Class)
      rocit_test_refined <- rocit(score = predictions_prob_test_refined, class = test_data_selected$Death_Rate_Class)
      
      # Plot ROC curves for the test data from both models
      plot(rocit_test_original, col = "red", main = "ROC Curves for Test Data (Original vs Refined Model)", lwd = 2)
      lines(rocit_test_refined$TPR ~ rocit_test_refined$FPR, col = "green", lwd = 2)
      
      # Add legend to distinguish between models
      legend("bottomright", legend = c("Original Model", "Refined Model"), col = c("red", "green"), lwd = 2)
      
    } else if (input$classification_type == "Logistic Regression") {
      # Predict probabilities for Logistic Regression
      predictions_Log <- predict(logistic_model, newdata = test_data_log2, type = "response")
      predictions_Log_selected <- predict(logistic_model_selected, newdata = test_data_log2_s, type = "response")
      
      # ROC Curves using ROCit for test data
      rocit_log_test <- rocit(score = predictions_Log, class = test_data_log2$Death_Rate_Class)
      rocit_log_test2 <- rocit(score = predictions_Log_selected, class = test_data_log2_s$Death_Rate_Class)
      
      # Plot ROC curves for the test data from both models
      plot(rocit_log_test, col = "red", main = "ROC Curves for Test Data (Original vs Refined Model)", lwd = 2)
      lines(rocit_log_test2$TPR ~ rocit_log_test2$FPR, col = "green", lwd = 2)
      
      # Add legend to distinguish between models
      legend("bottomright", legend = c("Original Model", "Refined Model"), col = c("red", "green"), lwd = 2)
    }
    else if (input$classification_type == "Best Model") {
      # Predictions probabilities for the test data from the original and refined models
      predictions_Log <- predict(logistic_model, newdata = test_data_log2, type = "response")
      predictions_prob_test_refined <- predict(tree_model_threshold, newdata = test_data_selected, type = "prob")[, 2]  # Refined model
      
      # ROC Curves using ROCit for test data
      rocit_log_test <- rocit(score = predictions_Log, class = test_data_log2$Death_Rate_Class)
      rocit_test_refined <- rocit(score = predictions_prob_test_refined, class = test_data_selected$Death_Rate_Class)
      
      # Plot ROC curves for the test data from both models
      plot(rocit_log_test, col = "red", main = "ROC Curves for Test Data (Log best vs DT best)", lwd = 2)
      lines(rocit_test_refined$TPR ~ rocit_test_refined$FPR, col = "green", lwd = 2)
      
      # Add legend to distinguish between models
      legend("bottomright", legend = c("Log best Model", "DT best Model"), col = c("red", "green"), lwd = 2)
    }
})
      
    output$performance_output <- renderPrint({
    req(input$classification_type)  # Ensure input is available

    if (input$classification_type == "Decision Tree") {
      # Performance for Decision Tree
      performance_tree_refined <- performanceMeasures(test_data_selected$Death_Rate_Class, predictions_tree_refined, model.name = "Refined Decision Tree")
      print(performance_tree_refined)
      # AUC values for both models
      cat("AUC for Original Model:", rocit_test_original$AUC, "\n")
      cat("AUC for Refined Model:", rocit_test_refined$AUC, "\n")
      
    } else if (input$classification_type == "Logistic Regression") {
      # Performance for Logistic Regression
      performance_Log <- performanceMeasures(test_data_log2$Death_Rate_Class, predictions_Log, model.name = "Logistic Regression")
      print(performance_Log)
      # AUC values for both models
      cat("AUC for Original Model:", rocit_log_test$AUC, "\n")
      cat("AUC for Refined Model:", rocit_log_test2$AUC, "\n")
    }
      else if (input$classification_type == "Best Model") {
        # AUC values for both models
        cat("AUC for Log best Model:", rocit_log_test$AUC, "\n")
        cat("AUC for DT best Model:", rocit_test_refined$AUC, "\n")
      }
      
  })
    # Select numerical columns and scale them for clustering
    numerical_columns <- names(merged_data3)[sapply(merged_data3, is.numeric)]
    numerical_columns <- setdiff(numerical_columns, c("Death_Rate_Proxy","Death_Proxy_Proportion","Death_Rate_Class"))
    data_clustering <- merged_data3[, numerical_columns]
    data_clustering_scaled <- scale(data_clustering)
    
    # Reactive expression for running kmeansruns() based on the selected criterion
    kmeans_clustering <- reactive({
      if (input$clustering_criterion == "ch") {
        set.seed(123)
        kmeans_result_ch <- kmeansruns(data_clustering_scaled, krange = 1:10, criterion = "ch")
        return(list(bestk = kmeans_result_ch$bestk, crit = kmeans_result_ch$crit))
      } else if (input$clustering_criterion == "asw") {
        set.seed(123)
        kmeans_result_asw <- kmeansruns(data_clustering_scaled, krange = 1:10, criterion = "asw")
        return(list(bestk = kmeans_result_asw$bestk, crit = kmeans_result_asw$crit))
      }
    })
    
    # Display the optimal number of clusters based on the selected criterion
    output$optimal_clusters <- renderPrint({
      result <- kmeans_clustering()
      cat("Optimal number of clusters based on selected criterion:", result$bestk, "\n")
    })
    
    # PCA result for dimensionality reduction
    pca_result <- prcomp(data_clustering_scaled)
    
    # Visualize K-means clustering results
    output$clustering_plot <- renderPlot({
      k <- input$num_clusters  # Use the number of clusters selected by the user
      visualize_kmeans(data_clustering_scaled, k, pca_result)
    })
    
}

# Function to visualize K-means clustering results
visualize_kmeans <- function(data_scaled, k, pca_result) {
  set.seed(123)
  kmeans_model <- kmeans(data_scaled, centers = k, nstart = 20)
  pca_df <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], Cluster = as.factor(kmeans_model$cluster))
  
  ggplot(pca_df, aes(PC1, PC2, color = Cluster)) +
    geom_point() +
    stat_ellipse(geom = "polygon", alpha = 0.3) +
    labs(title = paste("K-means Clustering with k =", k)) +
    theme_minimal()
}

# Run the application 
shinyApp(ui = ui, server = server)

