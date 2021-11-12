shinyServer(function(input, output, session) {
  
  if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install()
  BiocManager::install("BiocParallel")
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    model <- readRDS(file = rdsfile)
    #  model <- setEnvironment(model)
    models[[name]] <- model
    
    # try to update the preprocessing steps with the ones that were used
    steps <- model$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    preprocessingInputId <- paste0(name, "_Preprocess")
    updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate)
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  output$gg_Missing <- renderPlot({
    naniar::gg_miss_upset(data = getData(), nsets = 5)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  # output DataSummary2 ----
  output$DataSummary2 <- renderPrint({
    summary(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", #seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.rpart)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  
  
  # METHOD * knn ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRandomForestRecipe ----
  getknnRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$knn_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent knn_Go ----
  observeEvent(
    input$knn_Go,
    {
      #library(knn)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      tryCatch({
        deleteRds(method)
        model <- caret::train(getknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output knn_ModelSummary0 (print) ----
  output$knn_ModelSummary0 <- renderText({
    description("knn")   # Use the caret method name here
  })
  
  # output knn_Metrics (table) ----
  output$knn_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  # output knn_Recipe (print) ----
  output$knn_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  # output knn_ModelPlots (plot) ----
  output$knn_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })

  
  # METHOD * randomForest ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRandomForestRecipe ----
  getRandomForestRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$randomForest_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent randomForest_Go ----
  observeEvent(
    input$randomForest_Go,
    {
      library(randomForest)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      mtry <- sqrt(ncol(getTrainData()))
      tunegrid <- expand.grid(.mtry=mtry)
      
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRandomForestRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, tuneGrid=tunegrid, importance = TRUE)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output randomForest_ModelSummary0 (print) ----
  output$randomForest_ModelSummary0 <- renderText({
    description("rf")   # Use the caret method name here
  })
  
  # output randomForest_Metrics (table) ----
  output$randomForest_Metrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  # output randomForest_Recipe (print) ----
  output$randomForest_Recipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  # output randomForest_ModelPlots (plot) ----
  output$randomForest_ModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  # output randomForest_ModelImportance (plot) ----
  output$randomForest_ModelImportance <- renderPlot({
    req(models$rf)
    library(randomForest)
    randomForest::varImpPlot(models$rf$finalModel)
    
  })
  # output randomForest_Summary2 (print) ----
  output$randomForest_Summary2 <- renderPrint({
    req(models$rf)
    print(models$rf)
  })
  
  # METHOD * qrf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getqrfRecipe ----
  getqrfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$qrf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent qrf_Go ----
  observeEvent(
    input$qrf_Go,
    {
      library(quantregForest)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      mtry <- sqrt(ncol(getTrainData()))
      tunegrid <- expand.grid(.mtry=mtry)
      
      tryCatch({
        deleteRds(method)
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, tuneGrid=tunegrid, importance = TRUE)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output qrf_ModelSummary0 (print) ----
  output$qrf_ModelSummary0 <- renderText({
    description("qrf")   # Use the caret method name here
  })
  
  # output qrf_Metrics (table) ----
  output$qrf_Metrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrf_Recipe (print) ----
  output$qrf_Recipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrf_ModelPlots (plot) ----
  output$qrf_ModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  # output qrf_ModelImportance (plot) ----
  output$qrf_ModelImportance <- renderPlot({
    req(models$qrf)
    library(randomForest)
    randomForest::varImpPlot(models$qrf$finalModel)
    
  })
  # output qrf_Summary2 (print) ----
  output$qrf_Summary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
  
  # METHOD * lm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getlmRecipe ----
  getlmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      #library(lm)
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      tryCatch({
        deleteRds(method)
        model <- caret::train(getlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output elm_ModelSummary0 (print) ----
  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })
  
  # output lm_Metrics (table) ----
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # output elm_Recipe (print) ----
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  # METHOD * neuralnet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getneuralnetRecipe ----
  getneuralnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$neuralnet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent neuralnet_Go ----
  observeEvent(
    input$neuralnet_Go,
    {
      library(neuralnet)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "neuralnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      mtry <- sqrt(ncol(getTrainData()))
      tunegrid <- expand.grid(.mtry=mtry)
      
      tryCatch({
        deleteRds(method)
        model <- caret::train(getneuralnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5,  act.fct="tanh", linear.output=TRUE, stepmax=1e7)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output neuralnet_ModelSummary0 (print) ----
  output$neuralnet_ModelSummary0 <- renderText({
    description("neuralnet")   # Use the caret method name here
  })
  
  # output neuralnet_Metrics (table) ----
  output$neuralnet_Metrics <- renderTable({
    req(models$neuralnet)
    models$neuralnet$results[ which.min(models$neuralnet$results[, "RMSE"]), ]
  })
  
  # output neuralnet_Recipe (print) ----
  output$neuralnet_Recipe <- renderPrint({
    req(models$neuralnet)
    models$neuralnet$recipe
  })  
  
  # METHOD * rbf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrbfRecipe ----
  getrbfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$rbf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rbf_Go ----
  observeEvent(
    input$rbf_Go,
    {
      #library(rbf)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rbfDDA"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrbfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rbf_ModelSummary0 (text) ----
  output$rbf_ModelSummary0 <- renderText({
    description("rbfDDA")   # Use the caret method name here
  })
  
  # output rbf_Metrics (table) ----
  output$rbf_Metrics <- renderTable({
    req(models$rbfDDA)
    models$rbfDDA$results[ which.min(models$rbfDDA$results[, "RMSE"]), ]
  })
  
  # output rbf_ModelPlots (plot) ----
  output$rbf_ModelPlots <- renderPlot({
    req(models$rbfDDA)
    plot(models$rbfDDA)
  })     
  
  # output rbf_Recipe (print) ----
  output$rbf_Recipe <- renderPrint({
    req(models$rbfDDA)
    models$rbfDDA$recipe
  })  
  
  # output rbf_ModelSummary2 (print) ----
  output$rbf_ModelSummary2 <- renderPrint({
    req(models$rbfDDA)
    summary(models$rbfDDA$finalModel)
  })
  
  # METHOD * svmLinear---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getsvmRecipe ----
  getsvmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$svm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svm_Go ----
  observeEvent(
    input$svm_Go,
    {
      library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svm_ModelSummary0 (text) ----
  output$svm_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })
  
  # output svm_Metrics (table) ----
  output$svm_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  
  # output svm_Recipe (print) ----
  output$svm_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output svm_ModelSummary2 (print) ----
  output$svm_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    summary(models$svmLinear$finalModel)
  })
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getsvmPolyfRecipe ----
  getsvmPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$svmPoly_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmPoly_Go ----
  observeEvent(
    input$svmPoly_Go,
    {
      library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmPoly_ModelSummary0 (text) ----
  output$svmPoly_ModelSummary0 <- renderText({
    description("svmPoly")   # Use the caret method name here
  })
  
  # output svmPoly_ModelPlots (plot) ----
  output$svmPoly_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })     
  
  # output svmPoly_Metrics (table) ----
  output$svmPoly_Metrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmPoly_ModelPlots (plot) ----
  output$svmPoly_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })     
  
  # output svmPoly_Recipe (print) ----
  output$svmPoly_Recipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmPoly_ModelSummary2 (print) ----
  output$svmPoly_ModelSummary2 <- renderPrint({
    req(models$svmPoly)
    summary(models$svmPoly$finalModel)
  })
  
  # METHOD * svmRadial ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getsvmRadialRecipe ----
  getsvmRadialRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmRadial_Go ----
  observeEvent(
    input$svmRadial_Go,
    {
      library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmRadial_ModelSummary0 (text) ----
  output$svmRadial_ModelSummary0 <- renderText({
    description("svmRadial")   # Use the caret method name here
  })
  
  # output svmRadial_Metrics (table) ----
  output$svmRadial_Metrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  # output svmRadial_ModelPlots (plot) ----
  output$svmRadial_ModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })     
  
  # output svmRadial_ModelPlots (plot) ----
  output$svmRadial_ModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })     
  
  # output svmRadial_Recipe (print) ----
  output$svmRadial_Recipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  # output svmRadial_ModelSummary2 (print) ----
  output$svmRadial_ModelSummary2 <- renderPrint({
    req(models$svmRadial)
    summary(models$svmRadial$finalModel)
  })
  
  
  # METHOD * enet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getenetRecipe ----
  getenetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$enet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent enet_Go ----
  observeEvent(
    input$enet_Go,
    {
      library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getenetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output enet_ModelSummary0 (text) ----
  output$enet_ModelSummary0 <- renderText({
    description("enet")   # Use the caret method name here
  })
  
  # output enet_Metrics (table) ----
  output$enet_Metrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  # output enet_ModelPlots (plot) ----
  output$enet_ModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  }) 
  
  # output enet_ModelPlots (plot) ----
  output$enet_ModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })     
  
  # output enet_Recipe (print) ----
  output$enet_Recipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })  
  
  # output enet_ModelSummary2 (print) ----
  output$enet_ModelSummary2 <- renderPrint({
    req(models$enet)
    summary(models$enet$finalModel)
  })
  
  # METHOD * blackboost ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getblackboostRecipe ----
  getblackboostRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$blackboost_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent blackboost_Go ----
  observeEvent(
    input$blackboost_Go,
    {
      library(plyr)  
      library(party)
      library(mboost)
      library(partykit)
      method <- "blackboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getblackboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output blackboost_ModelSummary0 (text) ----
  output$blackboost_ModelSummary0 <- renderText({
    description("blackboost")   # Use the caret method name here
  })
  
  # output blackboost_Metrics (table) ----
  output$blackboost_Metrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  # output blackboost_ModelPlots (plot) ----
  output$blackboost_ModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  }) 
  
  # output blackboost_ModelPlots (plot) ----
  output$blackboost_ModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })     
  
  # output blackboost_Recipe (print) ----
  output$blackboost_Recipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })  
  
  # output blackboost_ModelSummary2 (print) ----
  output$blackboost_ModelSummary2 <- renderPrint({
    req(models$blackboost)
    summary(models$blackboost$finalModel)
  })
  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubist_Go ----
  observeEvent(
    input$cubist_Go,
    {
      library(Cubist)  
      library(plyr)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output cubist_ModelSummary0 (text) ----
  output$cubist_ModelSummary0 <- renderText({
    description("cubist")   # Use the caret method name here
  })
  
  # output cubist_Metrics (table) ----
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubist_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  }) 
  
  # output cubist_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })     
  
  # output cubist_Recipe (print) ----
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubist_ModelSummary2 (print) ----
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  

  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    req(length(models) > 1)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    # hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})
