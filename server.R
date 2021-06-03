server <- function(input, output,session) {
  
  tr_data <-  reactive({
    req(input$tr_data$datapath)
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  test_data <-  reactive({
    req(input$test_data$datapath)
    df <- read.csv(input$test_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  tr_cols <- reactive({
    req(input$tr_data$datapath)
    return(colnames(tr_data()))
    })
  

  
  
  #----Tab-2 Data Summary----#
  
  output$samp <- DT::renderDataTable({
    req(input$tr_data$datapath)
    DT::datatable(tr_data(),
                  #filter = "top"
                  options = list(lengthMenu = list(c(10,25,50,-1),c("5","25","50","All")),
                                autoWidth = TRUE),
                  caption = "Table 1: Sample of Data"
                  )
  })
  
  output$data_str <- renderPrint({
    str(tr_data())
  })
  
  output$miss_plot <- renderPlot({
    req(input$tr_data$datapath)
    Amelia::missmap(tr_data())
  })
  
  
  #-------------#
  output$y_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = 'sel_y',label = "Select Y (Target Variable)",choices = tr_cols(),multiple = FALSE)
  })
  
  x_col <- reactive({
    req(input$tr_data$datapath)
    x <- match(input$sel_y,tr_cols())
    y_col <- tr_cols()[-x]
    return(y_col)
  })
  
  output$x_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_x",label="Select X (Features)",choices = x_col(),multiple = TRUE,selected = x_col())
  })
  
  
  output$pca_plot <- renderPlot({
    if(input$task=="clf"){
      y <- tr_data()[,input$sel_y]
      X <- tr_data()[,input$sel_x]
      pca_plot(y,X)
    }else{
      return(NULL)
    }
    
  })
  
  values <- reactiveValues()
  
  data <- eventReactive(input$apply, {
    y <- tr_data()[,input$sel_y]
    X <- tr_data()[,input$sel_x]
    df0 <- data.frame(y,X)
    df0 <- df0 %>% tidyr::drop_na()
    #df0 
    train_test_data <- train_test_split(df0,classifn = input$task,input$tr_per)
    train_data <- train_test_data[[1]]
    test_data <- train_test_data[[2]]
    withProgress(message = 'Training in progress. Please wait ...',
    rf <- randomForest(y ~ ., data=train_data, ntree = input$n_tree, proximity=TRUE,na.action = na.omit))
    p1 <- predict(rf, train_data)
    p2 <- predict(rf, test_data)
    if (input$task == 'clf') {
      train_conf = caret::confusionMatrix(p1, train_data[,1])
      test_conf = caret::confusionMatrix(p2, test_data[,1])
      output$roc <- renderPlot({
                          plot_roc(rf,test_data$y,test_data[,-1])
                    })
      output$roc_val <- renderPrint({
                        auc_l <- print_roc(rf,test_data$y,test_data[,-1])
                        auc_l
                        })
      return(list(rf,train_conf,test_conf))
    }else{
      rmse_train <- RMSE(p1,train_data[,1])
      rmse_test <- RMSE(p2,test_data[,1])
      return(list(rf,rmse_train,rmse_test))
    }
    
  })
  
  output$mod_sum <- renderPrint({
    data()[[1]]
  })
  
  output$train_res <- renderUI({
    if(input$task=="clf"){
      plotOutput("conf_train_plot")
    }else{
      return(NULL)
    }
  })
  
  output$test_res <- renderUI({
    if(input$task=="clf"){
      plotOutput('conf_test_plot')
    }else{
      return(NULL)
    }
  })
  
  output$conf_train_plot <- renderPlot({
    fourfoldplot(data()[[2]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="Confusion Matrix (Train Set)")
  })
 
  
  output$conf_test_plot <- renderPlot({
    fourfoldplot(data()[[3]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="Confuison Matrix (Test Set)")
  })
  
  output$conf_train <- renderPrint({
    if(input$task=="clf"){
      data()[[2]]
    }else{
      cat("RMSE on Train data is ",data()[[2]])
    }
    
  })

  output$conf_test <- renderPrint({
    if(input$task=="clf"){
      data()[[3]]
    }else{
      cat("RMSE on Test data is ",data()[[3]])
    }
    
  })
  
  #----RF Plot output tab ------#
  output$err_rate <- renderPlot({
    req(data()[[1]])
    plot(data()[[1]],main="Error Rate")
  })
  
  # output$roc <- renderPlot({
  #   if(input$task=="clf"){
  #     data()[[4]]
  #   }else{
  #     return(NULL)
  #   }
  #      
  # })
  # 
  #-----Var Imp Plot ----#
  
  output$n_tree <- renderPlot({
    hist(treesize(data()[[1]]),
         main = "No. of Nodes for the Trees",
         col = "green",xlab="Tree Size")
  })
  
  output$var_imp <- renderPlot({
    varImpPlot(data()[[1]],
               sort = T,
               n.var = 10,
               main = "Top 10 - Variable Importance")
  })
  
  output$var_imp_tb <- DT::renderDataTable({
    if(input$task=="clf"){
      imp_df = data.frame("Features" = names(importance(data()[[1]])[,1]),
                          "MeanDecreaseGini"=round(importance(data()[[1]])[,1],2))
      a0 = sort(imp_df$MeanDecreaseGini, decreasing = TRUE, index.return=TRUE)$ix
      rownames(imp_df) = NULL
      # names(imp_df) <- c("Features","MeanDecreaseGini")
      imp_df[a0,]
    }else{
      imp_df = data.frame("Features" = names(importance(data()[[1]])[,1]),
                          "Mean_Decrease_Residual_Sum_Of_Sqr"=round(importance(data()[[1]])[,1],2))
      imp_df$Mean_Decrease_Residual_Sum_Of_Sqr <- format(imp_df$Mean_Decrease_Residual_Sum_Of_Sqr,big.mark = ",",scientific=FALSE)
     # a0 = sort(imp_df$Mean_Residual_Sum_Of_Sqr, decreasing = TRUE, index.return=TRUE)$ix
      rownames(imp_df) = NULL
      # names(imp_df) <- c("Features","MeanDecreaseGini")
      imp_df
    }
    
  
   
  })
  
  # Prediction Tab----#
  out_pred_df <- reactive({ 
    req(test_data())
    pred_data <- test_data()[,input$sel_x]
    p3 = predict(data()[[1]], pred_data)
    p3 <- round(p3,3)
    out_pred_df = data.frame("prediction" = p3, pred_data)
    })# downloadable file. })
  
  output$test_op <- DT::renderDataTable({
       head(out_pred_df(), 25) # display 10 rows of this as HTML tbl
  })
  
  output$download_pred <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      write.csv(out_pred_df(), file,row.names=FALSE)
    }
  )
  
}