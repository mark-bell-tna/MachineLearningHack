library(rpart.plot)
library(shiny)
library(dotwhisker)
library(kknn)
library(class)
library(e1071)
library(neuralnet)
library(DiagrammeR)

data(ptitanic)
#colnames(ptitanic)[which(names(ptitanic) == "sex")] <- "gender"
ptitanic$row_id <- row.names(ptitanic)
ptitanic$survived_enc <- as.integer(ptitanic$survived == "survived")
ptitanic$gender_enc <- as.integer(ptitanic$sex == "male")
ptitanic$pclass_enc <- as.integer(substr(ptitanic$pclass,1,1))
ptitanic[is.na(ptitanic$age),]$age <- mean(ptitanic[!is.na(ptitanic$age),]$age)
ptitanic$age_normal <- ptitanic$age / max(ptitanic$age)
all_titanic <<- ptitanic
main_smp_size <- floor(0.95 * nrow(ptitanic))
set.seed(246)
main_ind <- sample(seq_len(nrow(ptitanic)), size = main_smp_size)
main_titanic <- ptitanic[main_ind, ]
secret_test <- ptitanic[-main_ind, ]

titanic_choices <- c("Survived" = "survived", "Ticket Class" = "pclass", "Gender" = "sex",
                     "Age" = "age", "#Siblings/Spouses" = "sibsp", "#Parents/Children" = "parch")

smp_size <- floor(0.75 * nrow(main_titanic))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(main_titanic)), size = smp_size)

training_set <- main_titanic[train_ind, ]
validation_set <- main_titanic[-train_ind, ]


rpart_wrap <- function(model, data, algo_args) {
  rpart(model, data, method = "class",
        control = rpart.control(minsplit = algo_args$minsplit,
                                minbucket = algo_args$minsplit,
                                cp = algo_args$cp))
}

logit_wrap <- function(model, data, algo_args) {
  
  glm(model, data = data, family = eval(parse(text=algo_args$method)))
}

nnet_wrap <- function(model, data, algo_args) {
  print(paste0("Hidden: ", algo_args$hidden))
  neuralnet(model, data = data, hidden = algo_args$hidden, linear.output = T,
            stepmax=1e6, threshold = 0.1)
}

svm_wrap <- function(model, data, algo_args) {
  model <- eval(parse(text=model))
  print(paste0(class(model), " : ", model))
  svm(eval(parse(text = deparse(model))),
      data = data, type = 'C-classification', kernel = algo_args$kernel)
  #svm(x = data[,feature_cols], y = data[,predict_col], type='C-classification', kernel=algo_args$kernel)
}

bayes_wrap <- function(model, data, algo_args) {
  naiveBayes(eval(parse(text=model)), data)
}

knn_wrap <- function(model, data, algo_args, prob=FALSE) {
  print(model)
  col_list <- trimws(unlist(strsplit(model, "~|\\+")))
  print(col_list)
  predict_col <- col_list[1]
  feature_cols <- col_list[2:length(col_list)]
  print(predict_col)
  print(feature_cols)
  print(head(algo_args$validation[,feature_cols]))
  knn(data.frame(data[,feature_cols]),
      data.frame(algo_args$validation[,feature_cols]),
      data[,predict_col], k = algo_args$neighbours, prob = prob)
}




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
  selectInput("algorithm", label = "Algorithm:",
              choices = c("Decision Tree" = "decision", "Logistic" = "logistic",
                          "K Nearest Neighbours" = "knn",
                          "Naive Bayes" = "bayes",
                          "Support Vector Machine" = "svm", "Neural" = "neural"),
              selected = "decision",
              selectize = TRUE, multiple = FALSE),
  selectInput("predict_col", label = "Predict:",
              choices = c("Survived" = "survived"),
              selected = "survived",
              selectize = TRUE, multiple = FALSE),
  selectInput("feature_cols", label = "Features:",
              choices = c("Ticket Class" = "pclass", "Gender" = "sex",
                          "Age" = "age", "#Siblings/Spouses" = "sibsp",
                          "#Parents/Children" = "parch"),
              selected = NULL,
              selectize = TRUE, multiple = TRUE),
  textInput("algo_params", "Parameters"),
  actionButton("do", label = "Run"),
  actionButton("secret", label = "?"),
  selectInput("training_set", label = "Training Data:",
              choice = c("All" = "all_titanic", "Training" = "training_set",
                         "Validation" = "validation_set"),
              selected = "all_titanic", selectize = TRUE),
  selectInput("validation_set", label = "Validation Data:",
              choice = c("All" = "all_titanic", "Training" = "training_set",
                         "Validation" = "validation_set"),
              selected = "all_titanic", selectize = TRUE)
  ,width = 3),
  mainPanel(
  tabsetPanel(
    tabPanel("Plot",
             textOutput("accuracy"),
             plotOutput("plot"),
            tableOutput("evaluation")
    ),
    tabPanel("Plot",
             textOutput("accuracy2"),
             grVizOutput("plot2"),
             tableOutput("evaluation2")))
  )
  #tabsetPanel(tabPanel("Plot",
  #            plotOutput("plot")),
  #tabPanel("B", plotOutput("plot"))),
  #plotOutput("plot"),
  #, width = 9))
))

server <- function(input, output, session) {
  v <- reactiveValues(algo = NULL, model = NULL)
  algo_fit <<- NULL
  algo_table <<- NULL
  algorithm <<- NULL
  feature_cols <<- c()
  predict_col <<- ""
  training <<- ""
  
  observeEvent(input$secret, {
    print("SECRET!")
    updateSelectInput(session, "validation_set", label = "Validation Data:",
                      choice = c("All" = "all_titanic", "Training" = "training_set",
                                 "Validation" = "validation_set", "Test" = "secret_test"))
  })
  
  observeEvent(input$algorithm, {
    algo <- input$algorithm
    if (algo == "decision") {
      param_value <- "50 10 0.01"
    } else if (algo == "knn") {
      param_value <- 3
    } else if (algo == "neural") {
      param_value <- 5
    } else if (algo == "svm") {
      param_value <- "sigmoid"
    } else if (algo == "logistic") {
      param_value <- "binomial"
    } else {
      param_value <- ""
    }
    updateTextInput(session, "algo_params", label = "Parameters",
                    value = param_value)
  })
  
  observeEvent(input$do, {
    v$text1 <- "N/A"
    v$text2 <- "N/A"
    feature_cols <<- input$feature_cols
    predict_col <<- input$predict_col
    algo_params <- input$algo_params
    algo_params <- unlist(strsplit(algo_params," "))
    
    training <<- eval(parse(text=input$training_set))
    validation <- eval(parse(text=input$validation_set))
    
    algorithm <<- input$algorithm
    print(paste0("ALGO:",algorithm))
    if (algorithm == "decision") {
      algo <- rpart_wrap
      algo_args <- list(method = "class", data = "ptitanic",
                        predict_type = "class", minsplit = as.integer(algo_params))
      
      #model <- paste(predict_col, "~", paste(feature_cols, collapse=" + "))
    } else if (algorithm == "logistic") {
      algo <- logit_wrap
      algo_args <- list(method = "binomial", predict_type = "response")
      if (predict_col == "survived") {
        predict_col <<- "survived_enc"
      }
      feature_cols[feature_cols == "sex"] <<- "gender_enc"
      feature_cols[feature_cols == "pclass"] <<- "pclass_enc"
      feature_cols[feature_cols == "age"] <<- "age_normal"
      
      #model <- paste(predict_col, "~", paste(feature_cols, collapse=" + "))
    } else if (algorithm == "knn") {
      algo <- knn_wrap
      algo_args <- list(validation=validation,
                        neighbours=as.integer(algo_params),
                        predict_type = "response")
      feature_cols[feature_cols == "sex"] <<- "gender_enc"
      feature_cols[feature_cols == "pclass"] <<- "pclass_enc"
      feature_cols[feature_cols == "age"] <<- "age_normal"
    } else if (algorithm == "bayes") {
      algo <- bayes_wrap
      algo_args <- list(predict_type = "class")
    } else if (algorithm == "svm") {
      algo <- svm_wrap
      algo_args <- list(kernel = algo_params[1], predict_type = "class")
      feature_cols[feature_cols == "sex"] <<- "gender_enc"
      feature_cols[feature_cols == "pclass"] <<- "pclass_enc"
      feature_cols[feature_cols == "age"] <<- "age_normal"
      #model <- paste(predict_col, "~", paste(feature_cols, collapse=" + "))
    } else if (algorithm == "neural") {
      if (predict_col == "survived") {
        predict_col <<- "survived_enc"
      }
      feature_cols[feature_cols == "sex"] <<- "gender_enc"
      feature_cols[feature_cols == "pclass"] <<- "pclass_enc"
      feature_cols[feature_cols == "age"] <<- "age_normal"
      algo <- nnet_wrap
      algo_args <- list(hidden = as.integer(algo_params), predict_type = "class")
      
    }
    
    model <- paste(predict_col, "~", paste(feature_cols, collapse=" + "))
    print(c(algo, "feature",feature_cols))
    model <<- eval(parse(text = model))
    print("Start Algo")
    print(model)
    print(head(training,1))
    algo_fit <<- algo(model, data = training, algo_args = algo_args)
    print(paste0("End Algo:", Sys.time()))
    
    print(paste0("ALGO:",algorithm))
    if (algorithm == "neural") {
      print("IS NEURAL")
      prediction <- neuralnet::compute(algo_fit, validation[,feature_cols])
      prediction <- as.integer(prediction$net.result >= 0.5)
    } else if (algorithm == "knn") {
      prediction <- algo_fit
    } else if (algorithm == "svm") {
      print(head(validation,1))
      prediction <- predict(algo_fit, newdata = validation[,feature_cols],
                            type = algo_args$predict_type)
      print(paste0("PREDICTION", length(prediction)))
    } else {
      print(paste0("PREDICTION", algo_args$predict_type))
      
      prediction <- predict(algo_fit, validation,  algo_args$predict_type)
    }
    if (algorithm == "logistic") {
      prediction <- as.integer(prediction >= 0.5)
    }
    print(head(validation,1))
    print(paste0("With:", predict_col, "Preds:", length(prediction),
                 "rows:",nrow(validation[validation[,predict_col] == prediction,])))
    print(head(validation[,predict_col],1))
    v$text1 <- sprintf("Model Accuracy: %2.5f",
                       nrow(validation[validation[,predict_col] == prediction,])/nrow(validation))
    
    print(paste0("Accuracy", v$text1))
    #algo_table <<- table(validation$survived, prediction)
  })
  
  output$accuracy <- renderText({
    v$text1
  })

  output$accuracy2 <- renderText({
    v$text1
  })
  
  output$plot2 <- renderGrViz({
    v$text1
    if (is.null(algo_fit)) {
      return()
    }
    if (algorithm == "bayes") {
      letters <- c('b','c','d','e','f','g')
      nb <- algo_fit
      gr_def <- "digraph { node [fontname = Helvetica] a [label = 'survived']"
      for (i in 1:length(feature_cols))
      {
        s_row <- algo_fit$tables[[feature_cols[i]]]["survived",]
        max_col <- which(s_row == max(s_row))
        col_name <- names(max_col)
        
        feature_pct <- as.numeric(s_row[max_col])
        gr_def <-paste(gr_def, " ", letters[i],
                       " [label = '", feature_cols[i], " \n(",
                       col_name, "=",feature_pct , ")'] ", sep = '')
      }
      gr_def <- paste(gr_def, " a -> {")
      
      for (i in 1:length(feature_cols)) {
        gr_def <- paste(gr_def, letters[i])
      }
      gr_def <- paste0(gr_def, "}}")
      
      grViz(gr_def)
      
    }
  })
  
  output$plot <- renderPlot({
    v$text1
    if (is.null(algo_fit)) {
      return()
    }
    if (algorithm == "decision") {
      cols <- ifelse(algo_fit$frame$yval == 1, "darkred", "green4") # green if survived
      
      prp(algo_fit, main="assorted arguments",
          extra=106,           # display prob of survival and percent of obs
          nn=TRUE,             # display the node numbers
          fallen.leaves=TRUE,  # put the leaves on the bottom of the page
          shadow.col="gray",   # shadows under the leaves
          branch.lty=3,        # draw branches using dotted lines
          branch=.5,           # change angle of branch lines
          faclen=0,            # faclen=0 to print full factor names
          #trace=1,             # print the automatically calculated cex
          split.cex=1.2,       # make the split text larger than the node text
          split.prefix="is ",  # put "is " before split text
          split.suffix="?",    # put "?" after split text
          col=cols, border.col=cols,   # green if survived
          split.box.col="lightgray",   # lightgray split boxes (default is white)
          split.border.col="darkgray", # darkgray border on split boxes
          split.round=.5)              # round the split box corners a tad
      
    } else if (algorithm == "logistic") {
      dwplot(algo_fit)
    } else if (algorithm == "knn") {
      tk <- train.kknn(model, training)
      plot(tk)
    } else if (algorithm == "svm") {
      vis_cols <- eval(parse(text=paste(feature_cols[1:2], collapse="~")))
      plot(algo_fit, data = training, formula = vis_cols)
    } else if (algorithm == "neural") {
      print("Plotting neural")
      plot(algo_fit, rep = "best", intercept = FALSE)
    }
  })
  
  output$evaluation <- renderTable({
    v$text1
    algo_table
  })
  
  output$evaluation2 <- renderTable({
    v$text1
    algo_table
  })
}

shinyApp(ui, server)
