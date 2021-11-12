shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Nadiia Nikoniuk"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             verbatimTextOutput(outputId = "DataSummary2"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "gg_Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             helpText("The preprocessing steps and their order are important. ", 
                      "See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(type = "pills",
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "null_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "null_Metrics"),
                        hr(),
                        verbatimTextOutput(outputId = "null_Recipe")
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmnet_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy", "month")),  # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmnet_Metrics"),
                        hr(),
                        plotOutput(outputId = "glmnet_ModelPlots"),
                        verbatimTextOutput(outputId = "glmnet_Recipe"),
                        verbatimTextOutput(outputId = "glmnet_ModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "pls_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "pls_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy", "center", "scale")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "pls_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "pls_Metrics"),
                        hr(),
                        plotOutput(outputId = "pls_ModelPlots"),
                        verbatimTextOutput(outputId = "pls_Recipe"),
                        verbatimTextOutput(outputId = "pls_ModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rpart_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute", "center", "scale")), # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "rpart_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rpart_Metrics"),
                        hr(),
                        plotOutput(outputId = "rpart_ModelPlots"),
                        plotOutput(outputId = "rpart_ModelTree"),
                        verbatimTextOutput(outputId = "rpart_Recipe")
               ),
               tabPanel("rf Model",
                        verbatimTextOutput(outputId = "randomForest_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "randomForest_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute", "center", "scale")), # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "randomForest_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "randomForest_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "randomForest_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "randomForest_Metrics"),
                        hr(),
                        #plotOutput(outputId = "randomForest_ModelPlots"),
                        verbatimTextOutput(outputId = "randomForest_Recipe"),
                        plotOutput(outputId = "randomForest_ModelImportance"),
                        verbatimTextOutput(outputId = "randomForest_Summary2")
               ),
               tabPanel("qrf Model",
                        verbatimTextOutput(outputId = "qrf_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "qrf_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute", "center", "scale")), # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "qrf_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "qrf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "qrf_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "qrf_Metrics"),
                        hr(),
                        #plotOutput(outputId = "qrf_ModelPlots"),
                        verbatimTextOutput(outputId = "qrf_Recipe"),
                        plotOutput(outputId = "qrf_ModelImportance"),
                        verbatimTextOutput(outputId = "qrf_Summary2")
               ),
               tabPanel("knn Model",
                        verbatimTextOutput(outputId = "knn_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "knn_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy","center","scale")), # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "knn_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "knn_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "knn_Metrics"),
                        hr(),
                        plotOutput(outputId = "knn_ModelPlots"),
                        verbatimTextOutput(outputId = "knn_Recipe")
               ),
               
               tabPanel("lm Model",
                        verbatimTextOutput(outputId = "lm_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "lm_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy","dow")), # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "lm_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "lm_Metrics"),
                        hr(),
                        verbatimTextOutput(outputId = "lm_Recipe")
               ),
               tabPanel("neuralnet Model",
                        verbatimTextOutput(outputId = "neuralnet_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "neuralnet_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "neuralnet_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "neuralnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "neuralnet_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "neuralnet_Metrics"),
                        hr(),
                        plotOutput(outputId = "neuralnet_ModelPlots"),
                        verbatimTextOutput(outputId = "neuralnet_Recipe"),
                        verbatimTextOutput(outputId = "neuralnet_ModelSummary2")
               ),
               
               tabPanel("rbf Model",
                        verbatimTextOutput(outputId = "rbf_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rbf_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "rbf_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rbf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "rbf_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rbf_Metrics"),
                        hr(),
                        plotOutput(outputId = "rbf_ModelPlots"),
                        verbatimTextOutput(outputId = "rbf_Recipe"),
                        verbatimTextOutput(outputId = "rbf_ModelSummary2")
               ),
               tabPanel("svmLinear Model",
                        verbatimTextOutput(outputId = "svm_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svm_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "swm_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "svm_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svm_Metrics"),
                        hr(),
                        verbatimTextOutput(outputId = "svm_Recipe"),
                        verbatimTextOutput(outputId = "svm_ModelSummary2")
               ),
               tabPanel("svmPoly Model",
                        verbatimTextOutput(outputId = "svmPoly_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmPoly_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "svmPoly_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmPoly_Metrics"),
                        plotOutput(outputId = "svmPoly_ModelPlots"),
                        hr(),
                        verbatimTextOutput(outputId = "svmPoly_Recipe"),
                        verbatimTextOutput(outputId = "svmPoly_ModelSummary2")
               ),
              tabPanel("svmRadial Model",
                        verbatimTextOutput(outputId = "svmRdial_ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmRadial_Preprocess", # name this control <method>_Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                 bsTooltip(id = "svmRadial_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmRadial_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                 bsTooltip(id = "svmRadial_Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmRadial_Metrics"),
                       plotOutput(outputId = "svmRadial_ModelPlots"),
                        hr(),
                        verbatimTextOutput(outputId = "svmRadial_Recipe"),
                        verbatimTextOutput(outputId = "svmRadial_ModelSummary2")
               ),
              tabPanel("enet Model",
                       verbatimTextOutput(outputId = "enet_ModelSummary0"),
                       fluidRow(
                         column(width = 4, 
                                selectizeInput(inputId = "enet_Preprocess", # name this control <method>_Preprocess
                                               label = "Pre-processing", 
                                               choices = ppchoices,  
                                               multiple = TRUE, 
                                               selected = c("dow","knnimpute","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                bsTooltip(id = "enet_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                         ),
                         column(width = 1, 
                                actionButton(inputId = "enet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                bsTooltip(id = "enet_Go", title = "This will train or retrain your model")
                         )
                       ),
                       hr(),
                       h3("Resampled performance:"),
                       tableOutput(outputId = "enet_Metrics"),
                       plotOutput(outputId = "enet_ModelPlots"),
                       hr(),
                       verbatimTextOutput(outputId = "enet_Recipe"),
                       verbatimTextOutput(outputId = "enet_ModelSummary2")
              ),
              tabPanel("blackboost Model",
                       verbatimTextOutput(outputId = "blackboost_ModelSummary0"),
                       fluidRow(
                         column(width = 4, 
                                selectizeInput(inputId = "blackboost_Preprocess", # name this control <method>_Preprocess
                                               label = "Pre-processing", 
                                               choices = ppchoices,  
                                               multiple = TRUE, 
                                               selected = c("knnimpute","dow","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                bsTooltip(id = "blackboost_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                         ),
                         column(width = 1, 
                                actionButton(inputId = "blackboost_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                bsTooltip(id = "blackboost_Go", title = "This will train or retrain your model")
                         )
                       ),
                       hr(),
                       h3("Resampled performance:"),
                       tableOutput(outputId = "blackboost_Metrics"),
                       plotOutput(outputId = "blackboost_ModelPlots"),
                       hr(),
                       verbatimTextOutput(outputId = "blackboost_Recipe"),
                       verbatimTextOutput(outputId = "blackboost_ModelSummary2")
              ),
              tabPanel("cubist Model",
                       verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                       fluidRow(
                         column(width = 4, 
                                selectizeInput(inputId = "cubist_Preprocess", # name this control <method>_Preprocess
                                               label = "Pre-processing", 
                                               choices = ppchoices,  
                                               multiple = TRUE, 
                                               selected = c("knnimpute", "dow","dummy")),   # <-- These are suggested starting values. Set these to your best recommendation
                                bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                         ),
                         column(width = 1, 
                                actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                bsTooltip(id = "cubist_Go", title = "This will train or retrain your model")
                         )
                       ),
                       hr(),
                       h3("Resampled performance:"),
                       tableOutput(outputId = "cubist_Metrics"),
                       plotOutput(outputId = "cubist_ModelPlots"),
                       hr(),
                       verbatimTextOutput(outputId = "cubist_Recipe"),
                       verbatimTextOutput(outputId = "cubist_ModelSummary2")
              )
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here

               
               
               
               
               # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
