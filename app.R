# ============================================
# AI GLOBAL INDEX ANALYSIS - R SHINY APP
# Author: Mirzajon Shokirjonov
# ============================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(factoextra)
library(randomForest)
library(caret)
library(reshape2)

# ============================================
# MA'LUMOTLARNI YUKLASH
# ============================================
ai_data <- read.csv("AI_index_db.csv") 

# Klasterlash uchun o'zgaruvchilar
cluster_vars <- c("Talent", "Infrastructure", "Operating.Environment",
                  "Research", "Development", "Government.Strategy", "Commercial")

# ============================================
# UI - USER INTERFACE
# ============================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(title = "AI Global Index Analysis", titleWidth = 300),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(" Kirish", tabName = "home", icon = icon("home")),
      menuItem("Ma'lumotlar", tabName = "data", icon = icon("database")),
      menuItem("Klaster Tahlili", tabName = "cluster", icon = icon("project-diagram")),
      menuItem("Random Forest", tabName = "rf", icon = icon("tree")),
      menuItem("Bashorat", tabName = "predict", icon = icon("magic"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; font-size: 18px; }
        .info-box { margin-bottom: 15px; }
      "))
    ),
    
    tabItems(
      # ========== TAB 1: KIRISH ==========
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "AI Global Index Tahlili", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            h3("Loyiha haqida"),
            p("Ushbu interaktiv web-ilova 62 ta mamlakatning sun'iy intellekt (AI) 
              rivojlanish darajasini tahlil qilish uchun yaratilgan."),
            hr(),
            h4("Asosiy funksiyalar:"),
            tags$ul(
              tags$li("Ma'lumotlar tahlili va vizualizatsiya"),
              tags$li("K-means klasterlash yordamida mamlakatlarni guruhlash"),
              tags$li("Random Forest klassifikatsiya modeli"),
              tags$li("Yangi mamlakatlar uchun bashorat qilish")
            ),
            hr(),
            h4("Ishlatilgan indikatorlar:"),
            tags$ol(
              tags$li("Talent - Malakali mutaxassislar"),
              tags$li("Infrastructure - Texnik baza"),
              tags$li("Operating Environment - Qonunchilik muhiti"),
              tags$li("Research - Ilmiy tadqiqotlar"),
              tags$li("Development - Ishlanmalar"),
              tags$li("Government Strategy - Hukumat strategiyasi"),
              tags$li("Commercial - Tijorat faolligi")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_countries", width = 4),
          valueBoxOutput("total_vars", width = 4),
          valueBoxOutput("data_source", width = 4)
        )
      ),
      
      # ========== TAB 2: MA'LUMOTLAR ==========
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Dataset", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Tavsiflovchi Statistika",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("summary_stats")
          ),
          
          box(
            title = "Korrelyatsiya Matritsasi",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlation_heatmap", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "O'zgaruvchilar Taqsimoti",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            selectInput("hist_var", "O'zgaruvchini tanlang:",
                        choices = cluster_vars,
                        selected = "Talent"),
            plotlyOutput("histogram_plot", height = "400px")
          )
        )
      ),
      
      # ========== TAB 3: KLASTER TAHLILI ==========
      tabItem(
        tabName = "cluster",
        fluidRow(
          box(
            title = "Klasterlash Parametrlari",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            sliderInput("n_clusters", 
                        "Klasterlar soni:",
                        min = 2, max = 6, value = 4, step = 1),
            actionButton("run_kmeans", "Klasterlashni boshlash", 
                         class = "btn-primary", icon = icon("play"))
          ),
          
          box(
            title = "Klaster Statistikasi",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            verbatimTextOutput("cluster_stats")
          )
        ),
        
        fluidRow(
          box(
            title = "Optimal Klasterlar Soni (Elbow Method)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("elbow_plot", height = "400px")
          ),
          
          box(
            title = "PCA - Klaster Vizualizatsiyasi",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("pca_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Klaster Markazlari",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("cluster_centers_plot", height = "400px")
          ),
          
          box(
            title = "Mamlakatlar bo'yicha Klaster A'zoligi",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("cluster_membership_table")
          )
        )
      ),
      
      # ========== TAB 4: RANDOM FOREST ==========
      tabItem(
        tabName = "rf",
        fluidRow(
          box(
            title = "Model Parametrlari",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            sliderInput("ntree", "Daraxtlar soni:", 
                        min = 100, max = 1000, value = 500, step = 100),
            sliderInput("train_split", "Train to'plami (%):",
                        min = 60, max = 90, value = 80, step = 5),
            actionButton("run_rf", "Modelni ishga tushirish",
                         class = "btn-success", icon = icon("rocket"))
          ),
          
          box(
            title = "Model Natijalari",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            verbatimTextOutput("rf_results")
          )
        ),
        
        fluidRow(
          box(
            title = "Confusion Matrix",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("confusion_matrix_plot", height = "400px")
          ),
          
          box(
            title = "Variable Importance",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("var_importance_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "OOB Error Rate",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotOutput("oob_error_plot", height = "350px")
          )
        )
      ),
      
      # ========== TAB 5: BASHORAT ==========
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Yangi Mamlakat Ma'lumotlarini Kiriting",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            sliderInput("pred_talent", "Talent:", 
                        min = 0, max = 100, value = 50),
            sliderInput("pred_infra", "Infrastructure:",
                        min = 0, max = 100, value = 50),
            sliderInput("pred_env", "Operating Environment:",
                        min = 0, max = 100, value = 50),
            sliderInput("pred_research", "Research:",
                        min = 0, max = 100, value = 50),
            sliderInput("pred_dev", "Development:",
                        min = 0, max = 100, value = 50),
            sliderInput("pred_gov", "Government Strategy:",
                        min = 0, max = 100, value = 50),
            sliderInput("pred_comm", "Commercial:",
                        min = 0, max = 100, value = 50),
            actionButton("predict_btn", "Bashorat qilish",
                         class = "btn-danger", icon = icon("chart-line"))
          ),
          
          box(
            title = "Bashorat Natijasi",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            uiOutput("prediction_result"),
            hr(),
            plotlyOutput("prediction_viz", height = "400px")
          )
        )
      )
    )
  )
)

# ============================================
# SERVER - BACKEND LOGIC
# ============================================

server <- function(input, output, session) {
  
  # ========== VALUE BOXES ==========
  output$total_countries <- renderValueBox({
    valueBox(
      nrow(ai_data),
      "Mamlakatlar soni",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  output$total_vars <- renderValueBox({
    valueBox(
      length(cluster_vars),
      "Indikatorlar soni",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$data_source <- renderValueBox({
    valueBox(
      "2023",
      "Ma'lumotlar yili",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # ========== DATA TAB ==========
  output$data_table <- renderDT({
    datatable(
      ai_data,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top"
    )
  })
  
  output$summary_stats <- renderPrint({
    summary(ai_data[, cluster_vars])
  })
  
  output$correlation_heatmap <- renderPlotly({
    cor_matrix <- cor(ai_data[, cluster_vars], use = "complete.obs")
    cor_melted <- melt(cor_matrix)
    
    plot_ly(
      data = cor_melted,
      x = ~Var1, y = ~Var2, z = ~value,
      type = "heatmap",
      colors = colorRamp(c("blue", "white", "red")),
      zmin = -1, zmax = 1
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  output$histogram_plot <- renderPlotly({
    plot_ly(
      x = ai_data[[input$hist_var]],
      type = "histogram",
      marker = list(color = "steelblue")
    ) %>%
      layout(
        title = paste("Distribution of", input$hist_var),
        xaxis = list(title = input$hist_var),
        yaxis = list(title = "Count")
      )
  })
  
  # ========== CLUSTERING TAB ==========
  
  # Reactive: Standardized data
  cluster_data_scaled <- reactive({
    scale(ai_data[, cluster_vars])
  })
  
  # Reactive: K-means model
  kmeans_model <- eventReactive(input$run_kmeans, {
    set.seed(123)
    kmeans(cluster_data_scaled(), centers = input$n_clusters, nstart = 25)
  })
  
  output$cluster_stats <- renderPrint({
    req(kmeans_model())
    k <- kmeans_model()
    cat("Klaster hajmlari:\n")
    print(k$size)
    cat("\nKlaster markazlari:\n")
    print(round(k$centers, 3))
    cat("\nBSS/TSS:", round(k$betweenss / k$totss * 100, 2), "%\n")
  })
  
  output$elbow_plot <- renderPlot({
    fviz_nbclust(cluster_data_scaled(), kmeans, method = "wss") +
      labs(title = "Optimal Clusters - Elbow Method")
  })
  
  output$pca_plot <- renderPlot({
    req(kmeans_model())
    pca <- prcomp(cluster_data_scaled(), scale. = FALSE)
    df_cluster <- data.frame(cluster_data_scaled())
    df_cluster$cluster <- factor(kmeans_model()$cluster)
    
    fviz_pca_ind(
      pca,
      col.ind = df_cluster$cluster,
      palette = "jco",
      addEllipses = TRUE,
      ellipse.type = "convex",
      legend.title = paste("Cluster (k =", input$n_clusters, ")")
    )
  })
  
  output$cluster_centers_plot <- renderPlotly({
    req(kmeans_model())
    centers <- as.data.frame(kmeans_model()$centers)
    centers$cluster <- paste("Cluster", 1:nrow(centers))
    centers_long <- reshape2::melt(centers, id.vars = "cluster")
    
    plot_ly(
      data = centers_long,
      x = ~variable,
      y = ~value,
      color = ~cluster,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Cluster Centers Profile",
        xaxis = list(title = "Variables"),
        yaxis = list(title = "Standardized Value")
      )
  })
  
  output$cluster_membership_table <- renderDT({
    req(kmeans_model())
    df <- data.frame(
      Country = ai_data$Country,
      Cluster = kmeans_model()$cluster
    )
    datatable(df, options = list(pageLength = 10))
  })
  
  # ========== RANDOM FOREST TAB ==========
  
  rf_model_data <- eventReactive(input$run_rf, {
    req(kmeans_model())
    
    # Add cluster to data
    ai_data$cluster_Y <- as.factor(kmeans_model()$cluster)
    
    classification_data <- ai_data %>%
      select(all_of(cluster_vars), cluster_Y) %>%
      na.omit()
    
    # Train/test split
    set.seed(123)
    trainIndex <- createDataPartition(
      classification_data$cluster_Y,
      p = input$train_split / 100,
      list = FALSE
    )
    
    train_data <- classification_data[trainIndex, ]
    test_data <- classification_data[-trainIndex, ]
    
    # Train model
    rf_model <- randomForest(
      cluster_Y ~ .,
      data = train_data,
      ntree = input$ntree,
      mtry = 3,
      importance = TRUE
    )
    
    # Predictions
    predictions <- predict(rf_model, test_data)
    conf_matrix <- confusionMatrix(predictions, test_data$cluster_Y)
    
    list(
      model = rf_model,
      train_data = train_data,
      test_data = test_data,
      predictions = predictions,
      conf_matrix = conf_matrix
    )
  })
  
  output$rf_results <- renderPrint({
    req(rf_model_data())
    data <- rf_model_data()
    print(data$model)
    cat("\n=== Confusion Matrix ===\n")
    print(data$conf_matrix$table)
    cat("\nAccuracy:", round(data$conf_matrix$overall['Accuracy'] * 100, 2), "%\n")
  })
  
  output$confusion_matrix_plot <- renderPlot({
    req(rf_model_data())
    cm <- as.data.frame(rf_model_data()$conf_matrix$table)
    
    ggplot(cm, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 8) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  output$var_importance_plot <- renderPlotly({
    req(rf_model_data())
    imp <- importance(rf_model_data()$model)
    imp_df <- data.frame(
      Variable = rownames(imp),
      Importance = imp[, "MeanDecreaseAccuracy"]
    )
    imp_df <- imp_df[order(-imp_df$Importance), ]
    
    plot_ly(
      data = imp_df,
      x = ~Importance,
      y = ~reorder(Variable, Importance),
      type = "bar",
      orientation = "h",
      marker = list(color = "steelblue")
    ) %>%
      layout(
        title = "Variable Importance",
        xaxis = list(title = "Mean Decrease Accuracy"),
        yaxis = list(title = "")
      )
  })
  
  output$oob_error_plot <- renderPlot({
    req(rf_model_data())
    model <- rf_model_data()$model
    plot(model, main = "OOB Error Rate by Number of Trees", lwd = 2)
    legend("topright", colnames(model$err.rate), col = 1:ncol(model$err.rate), lty = 1)
  })
  
  # ========== PREDICTION TAB ==========
  
  prediction_result <- eventReactive(input$predict_btn, {
    req(rf_model_data())
    
    new_data <- data.frame(
      Talent = input$pred_talent,
      Infrastructure = input$pred_infra,
      Operating.Environment = input$pred_env,
      Research = input$pred_research,
      Development = input$pred_dev,
      Government.Strategy = input$pred_gov,
      Commercial = input$pred_comm
    )
    
    # Scale new data
    scaled_new <- scale(
      new_data,
      center = attr(cluster_data_scaled(), "scaled:center"),
      scale = attr(cluster_data_scaled(), "scaled:scale")
    )
    
    # Predict
    prediction <- predict(rf_model_data()$model, new_data, type = "response")
    probabilities <- predict(rf_model_data()$model, new_data, type = "prob")
    
    list(
      cluster = as.numeric(prediction),
      probabilities = probabilities,
      input_data = new_data
    )
  })
  
  output$prediction_result <- renderUI({
    req(prediction_result())
    result <- prediction_result()
    
    cluster_names <- c(
      "1" = "Emerging AI Adopters",
      "2" = "Global AI Leader",
      "3" = "Low AI Readiness",
      "4" = "Rising AI Economies"
    )
    
    cluster_name <- cluster_names[as.character(result$cluster)]
    
    HTML(paste0(
      "<h2 style='color: #3c8dbc;'>Bashorat natijasi: <b>Cluster ", result$cluster, "</b></h2>",
      "<h3 style='color: #00a65a;'>", cluster_name, "</h3>",
      "<hr>",
      "<h4>Ehtimolliklar:</h4>",
      "<ul>",
      paste0("<li>Cluster 1: ", round(result$probabilities[1] * 100, 2), "%</li>"),
      paste0("<li>Cluster 2: ", round(result$probabilities[2] * 100, 2), "%</li>"),
      paste0("<li>Cluster 3: ", round(result$probabilities[3] * 100, 2), "%</li>"),
      paste0("<li>Cluster 4: ", round(result$probabilities[4] * 100, 2), "%</li>"),
      "</ul>"
    ))
  })
  
  output$prediction_viz <- renderPlotly({
    req(prediction_result())
    result <- prediction_result()
    
    df <- data.frame(
      Variable = names(result$input_data),
      Value = as.numeric(result$input_data[1, ])
    )
    
    plot_ly(
      data = df,
      x = ~Variable,
      y = ~Value,
      type = "bar",
      marker = list(color = "coral")
    ) %>%
      layout(
        title = "Kiritilgan Ma'lumotlar",
        xaxis = list(title = ""),
        yaxis = list(title = "Score", range = c(0, 100))
      )
  })
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)