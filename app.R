library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #tags$hr(),
      # Output: Data file ----
      
      #tableOutput("contents"),
      
      #tags$hr(),
      # Output: Header + summary of distribution ----
      
      #h4("Summary"),
      #verbatimTextOutput("summary"),
      
      
      tags$hr(),
      # Output: Header + table of distribution ----
      h4("ROC COMPARISION"),
      plotOutput("ROC"),
      

      
      # tags$hr(),
      # # Output: Header + table of distribution ----
      # h4("ImpVars"),
      # tableOutput("IV"),
      
      
      
      # tags$hr(),
      # # Output: Header + table of distribution ----
      # h4("ROC PLOTS"),
      # tableOutput("ROCPLOT"),
      
      
      tags$hr(),
      # Output: Header + table of distribution ----
      h4("Final Predictions"),
      tableOutput("FD")
      
      
    )
    
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  maketable <- reactive({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       stringsAsFactors = FALSE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  

  
  
  output$ROC <- renderPlot({
    
    df = maketable()
    
    mydata <- df
    
    library(caret)
    library(ROCR)
    library(pROC)
    
    class(mydata$Class)
    
    
    names(mydata)[names(mydata)=="Attrition"] = "Class"
    removevars  = c("decile" , "predicted")
    mydata = mydata[!names(mydata) %in% removevars]
    class(mydata$Class)
    mydata$Class = ifelse(mydata$Class == "Yes" ,1 , 0 )
    mydata$Class  =as.factor(mydata$Class)
    
    pred_1 <- prediction(mydata$pred_SVM ,mydata$Class )
    pred_2 <- prediction(mydata$pred_CART ,mydata$Class )
    pred_3 <- prediction(mydata$pred_KNN ,mydata$Class )
    pred_4 <- prediction(mydata$pred_LDA ,mydata$Class )
    pred_5 <- prediction(mydata$pred_RF ,mydata$Class )
    pred_6 <- prediction(mydata$pred_BAG ,mydata$Class )
    pred_7 <- prediction(mydata$pred_GBM ,mydata$Class )
    pred_8 <- prediction(mydata$pred_C50 ,mydata$Class )
    pred_9 <- prediction(mydata$pred_GLM ,mydata$Class )
    
    
    
    auc.perf = performance(pred_1, measure = "auc")
    a = auc.perf@y.values
    
    auc.perf = performance(pred_2, measure = "auc")
    b = auc.perf@y.values
    
    auc.perf = performance(pred_3, measure = "auc")
    c = auc.perf@y.values
    
    auc.perf = performance(pred_4, measure = "auc")
    d = auc.perf@y.values
    
    auc.perf = performance(pred_5, measure = "auc")
    e = auc.perf@y.values
    
    auc.perf = performance(pred_6, measure = "auc")
    f = auc.perf@y.values
    
    auc.perf = performance(pred_7, measure = "auc")
    g = auc.perf@y.values
    
    auc.perf = performance(pred_8, measure = "auc")
    h = auc.perf@y.values
    
    auc.perf = performance(pred_9, measure = "auc")
    i = auc.perf@y.values
 
    
    
    auc_list  = data.frame(do.call(rbind,list(a,b,c,d,e,f,g,h,i)))
    Algorithm_Name  = c("SVM","CART","KNN","LDA","RF","BAG","GBM","C50","GLM")
    
    # ROC_PLOTS
    
    library(pROC)
    
    SVM_ROC = roc( mydata$Class , mydata$pred_SVM )
    CART_ROC = roc( mydata$Class , mydata$pred_CART )
    KNN_ROC = roc( mydata$Class , mydata$pred_KNN )
    LDA_ROC = roc( mydata$Class , mydata$pred_LDA )
    RF_ROC = roc( mydata$Class , mydata$pred_RF )
    BAG_ROC = roc( mydata$Class , mydata$pred_BAG )
    BOOST_ROC = roc( mydata$Class , mydata$pred_GBM )
    C50_ROC = roc( mydata$Class , mydata$pred_C50 )
    GLM_ROC = roc( mydata$Class , mydata$pred_GLM )
    
    
    plot(SVM_ROC,main="ROC Comparison",  col="blue")
    plot(CART_ROC, add=TRUE ,col="red")
    plot(KNN_ROC, add=TRUE, col="green")
    plot(LDA_ROC, add=TRUE, col="grey")
    plot(RF_ROC, add=TRUE, col="yellow")
    plot(BAG_ROC, add=TRUE, col="black")
    plot(BOOST_ROC, add=TRUE, col="lightblue")
    plot(C50_ROC, add=TRUE, col="pink")
    plot(GLM_ROC, add=TRUE, col="orange")
    
    #Add Legend to the plot that includes each model AUC score
    textos <- c("SVM","CART","KNN","LDA","RF","BAG","BOOST" ,"C50","GLM")
    
     AUC <- round(c(SVM_ROC$auc, CART_ROC$auc, KNN_ROC$auc, LDA_ROC$auc, RF_ROC$auc, BAG_ROC$auc, BOOST_ROC$auc,
                    C50_ROC$auc ,GLM_ROC$auc),3)
    
    #AUC = as.vector(round(do.call(rbind , Metric_Summary$Area_Under_Curve),3))
    textos <- paste(textos, AUC,sep=" - ")
    colors <- c("blue","red","green","grey","yellow","black","lightblue","pink", "orange")
    legend(0.4,0.4, legend = textos, col = colors, bty="n", cex=0.8, lty=1, lwd=2)
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$FD <- renderTable({
    
    df = maketable()
    df
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
