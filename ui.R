
shinyUI(fluidPage(
  
  title = "Random Forest",
  titlePanel(title=div(img(src="logo.png",align='right'),"Random Forest")),
  sidebarPanel(
    source("scripts//uiInput.R",local = TRUE)[[1]], 
    conditionalPanel(condition = "input.tabselected==2",
                     
                     ),
    conditionalPanel(condition="input.tabselected==3",
                     uiOutput("y_ui"),
                     uiOutput("x_ui"),
                     radioButtons("task","Select task",
                                  choices = c("Classification" = 'clf',
                                               "Regression" = "reg")),
                     sliderInput("tr_per",
                                 label = "Percentage of training data",
                                 min = 0,
                                 max = 1,
                                 value = 0.7,
                                 step = 0.05),
                     sliderInput("n_tree",
                                 label = "Number of trees",
                                 min = 100,
                                 max = 1000,
                                 value = 200,
                                 step = 100),
                    actionButton("apply","Train model")
    )
    
  ),
  mainPanel(
    # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
    # id argument is important in the tabsetPanel()
    # value argument is important in the tabPanle()
    tabsetPanel(
      tabPanel("Overview & Example Dataset", value=1, 
               includeMarkdown("overview.md")
      ),
      tabPanel("Data Summary", value=3,
               DT::dataTableOutput("samp"),
               hr(),
               h4("Data Structure"),
               verbatimTextOutput("data_str"),
               h4("Missingness Map"),
               plotOutput("miss_plot")
              
              
      ),
      tabPanel("RF Results", value=3,
               h4("Model Summary"),
               helpText("Training may take a while, upto a minute"),
               verbatimTextOutput("mod_sum"),
               hr(),
               #h4("Confusion Matrix (Train Set)"),
               uiOutput("train_res"),
               
               #plotOutput('conf_train_plot'),
               #HTML('<button data-toggle="collapse" data-target="#demo">Detailed Result</button>'),
              # tags$div(id="demo",class="collapse",),
               verbatimTextOutput("conf_train"),
               hr(),
               #h4("Confuison Matrix (Test Set)"),
              # HTML('<button data-toggle="collapse" data-target="#demo1">Detailed Result</button>'),
              uiOutput("test_res"),
              #plotOutput('conf_test_plot'),
              verbatimTextOutput("conf_test")
               #tags$div(id="demo1",class="collapse",)
               
      ),
      tabPanel("RF Plots",value=3,
               h4('PCA plot'),
               plotOutput("pca_plot"),
              # h4("Error Rate Plot"),
               plotOutput("err_rate"),
              # h4("ROC-AUC Curve"),
               plotOutput("roc")
      ),
      tabPanel("Variable Importance",value=3,
               #h4("No of nodes in trees"),
               plotOutput("n_tree"),
               h4("Variable Importance"),
               plotOutput("var_imp"),
               DT::dataTableOutput("var_imp_tb")
               ),
      tabPanel("Prediction Output",value=3,
               helpText("Note: Please upload test data with same features in train dataset"),
               DT::dataTableOutput("test_op"),
               downloadButton("download_pred")
               
      ),
      id = "tabselected"
    )
  )
))

