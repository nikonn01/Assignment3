library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)

if (!library("mixOmics", quietly = TRUE, logical.return = TRUE)) {
  library(devtools)
  install_github("mixOmicsTeam/mixOmics")  # for some reason this has been withdrawn from CRAN
  library("mixOmics")
}

ppchoices <- c("knnimpute", "bagimpute", "medianimpute", "modeimpute", "YeoJohnson", "naomit", "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv","other", "dummy")

startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

dynamicSteps <- function(recipe, preprocess) {
  for (s in preprocess) {
    if (s == "knnimpute") {
      recipe <- step_impute_knn(recipe, all_numeric(), all_nominal(), -has_role("outcome"), -has_role("id"), neighbors  = 5) # 5 is a reasonable guess
    } else if (s == "bagimpute") {
      recipe <- step_impute_bag(recipe, all_numeric(), all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "medianimpute") {
      recipe <- step_medianimpute(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "modeimpute") {
      recipe <- step_modeimpute(recipe, all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "YeoJohnson") {
      recipe <- step_YeoJohnson(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "naomit") { #Remove observations with missing values
      recipe <- step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") { #PCA Signal Extraction. centre and scale are required prior
      recipe <- step_pca(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), threshold = 0.95)
    } else if (s == "pls") {#Partial Least Squares Feature Extraction
      recipe <- step_pls(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), outcome = "Y", num_comp = 25)
    } else if (s == "ica") {#ICA Signal Extraction
      recipe <- step_ica(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), num_comp = 10)
    } else if (s == "center") {
      recipe <- step_center(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "scale") {
      recipe <- step_scale(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "month") {
      recipe <- step_date(recipe, has_type("date"), features = c("month"), ordinal = TRUE)
    } else if (s == "dow") { #day of week
      recipe <- step_date(recipe, has_type("date"), features = c("dow"), ordinal = TRUE)
    } else if (s == "dateDecimal") {
      recipe <- step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)
    } else if (s == "nzv") {#Near-Zero Variance Filter
      recipe <- step_nzv(recipe, all_predictors())
    }else if (s == "zv") {#Near-Zero Variance Filter
      recipe <- step_zv(recipe, all_predictors())
    } else if (s == "other") { #Collapse Some Categorical Levels
      recipe <- step_other(recipe, all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "dummy") {#Dummy Variables Creation
      recipe <- step_dummy(recipe, all_nominal(), -has_role("outcome"), -has_role("id"), one_hot = FALSE)
    } else if (s == "poly") {#Orthogonal Polynomial Basis Functions
      recipe <- step_poly(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), options = list(degree = 2))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}

deleteRds <- function(name) {
  ok <- TRUE
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  if (file.exists(file)) {
    ok <- unlink(file, force = TRUE)
  }
  ok
}

# strip off the global environment to save file size
clearEnvironment <- function(object) {
  # for (s in seq_along(object$recipe$steps)) {
  #   attr(object$recipe$steps[[s]]$terms, ".Environment") <- NULL
  #   for (t in seq_along(object$recipe$steps[[s]]$terms)) {
  #     attr(object$recipe$steps[[s]]$terms[[t]], ".Environment") <- NULL
  #   }
  #   if (!is.null(object$recipe$steps[[s]]$levels)) {
  #     for (l in seq_along(object$recipe$steps[[s]]$levels)) {
  #       attr(object$recipe$steps[[s]]$levels[[l]], ".Environment") <- NULL
  #     }
  #   }
  #   if (!is.null(object$recipe$steps[[s]]$impute_with)) {
  #     for (i in seq_along(object$recipe$steps[[s]]$impute_with)) {
  #       attr(object$recipe$steps[[s]]$impute_with[[i]], ".Environment") <- NULL
  #     }
  #   }
  # }
  # if (!is.null(object$finalModel$terms)) {
  #   attr(object$finalModel$terms, ".Environment") <- NULL  
  # }  
  rapply(object, classes = "ANY", how = "replace", function(o) {
    if (!is.null(attr(o, ".Environment"))) {
      attr(o, ".Environment") <- NULL
    }
    o
  })
}

# able to reattach the global environment (probably not needed) 
setEnvironment <- function(model) {
  for (step in model$recipe$steps) {
    for (term in step$terms) {
      attr(term, ".Environment") <- global_env()
    }
    if (exists("step$levels")) {
      for (level in step$levels) {
        attr(level, ".Environment") <- global_env()
      }
    }
    if (exists("step$impute_with")) {
      for (imp in step$impute_with) {
        attr(imp, ".Environment") <- global_env()
      }
    }
  }
  if (!is.null(model$finalModel$terms)) {
    attr(model$finalModel$terms, ".Environment") <- global_env()
  }
  model
}


# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  model <- clearEnvironment(rlang::duplicate(model, shallow = FALSE))
  cat("size = ", length(serialize(model, NULL)) )
  saveRDS(model, file)
}
