server <- function(input, output,session) {
  
  tr_data <-  reactive({
    req(input$tr_data$datapath)
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = TRUE)
    return(df)
  })
  
  test_data <-  reactive({
    req(input$test_data$datapath)
    df <- read.csv(input$test_data$datapath,stringsAsFactors = TRUE)
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
  
  
  data <- eventReactive(input$apply, {
    y <- tr_data()[,input$sel_y]
    X <- tr_data()[,input$sel_x]
    df0 <- data.frame(y,X)
    #df0 
    train_test_data <- train_test_split(df0,classifn = input$task,input$tr_per)
    train_data <- train_test_data[[1]]
    test_data <- train_test_data[[2]]
    rf <- randomForest(y ~ ., data=train_data, ntree = input$n_tree, proximity=TRUE)
    p1 <- predict(rf, train_data)
    if (input$task == 'clf') {
      train_conf = confusionMatrix(p1, train_data[,1])  
      #a1$table # print this as html table
      #print(a1)  # as raw text below the html tbl
    }
    p2 <- predict(rf, test_data)
    if (input$task == 'clf') {
      test_conf = confusionMatrix(p2, test_data[,1])  
      #a1$table # print this as html table
      #print(a1)  # as raw text below the html tbl
    }
    output$roc <- renderPlot({plot_roc(rf,test_data$y,test_data[,-1])})
    #roc.plot <- plot_roc(rf,test_data$y,test_data[,-1])
    list(rf,train_conf,test_conf)
    
  })
  
  output$mod_sum <- renderPrint({
    data()[[1]]
  })
  
  output$conf_train_plot <- renderPlot({
    fourfoldplot(data()[[2]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="")
  })
  
  output$conf_test_plot <- renderPlot({
    fourfoldplot(data()[[3]]$table,
                 color = c("#CC6666", "#99CC99"),
                 conf.level = 0,
                 main="")
  })
  
  output$conf_train <- renderPrint({
    data()[[2]]
  })

  output$conf_test <- renderPrint({
    data()[[3]]
  })
  
  #----RF Plot output tab ------#
  output$err_rate <- renderPlot({
    req(tr_data())
    plot(data()[[1]],main="Error Rate")
  })
  
  output$roc <- renderPlot({
    data()[[4]]
  })
  
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
  
  output$var_imp_tb <- renderDataTable({
    imp_df = data.frame("Features" = rownames(importance(data()[[1]])),
                        "MeanDecreaseGini"=round(importance(data()[[1]]),2))
    a0 = sort(imp_df$MeanDecreaseGini, decreasing = TRUE, index.return=TRUE)$ix
    rownames(imp_df) = NULL
   # names(imp_df) <- c("Features","MeanDecreaseGini")
    imp_df[a0,]
   
  })
  
  # Prediction Tab----#
  out_pred_df <- reactive({ 
    req(test_data())
    pred_data <- test_data()[,input$sel_x]
    p3 = predict(data()[[1]], pred_data)
    out_pred_df = data.frame("prediction" = p3, pred_data)
    })# downloadable file. })
  
  output$test_op <- renderDataTable({
       head(out_pred_df(), 10) # display 10 rows of this as HTML tbl
  })
  
  output$download_pred <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      write.csv(out_pred_df(), file,row.names=FALSE)
    }
  )
  
}