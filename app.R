############################################################
#                                                          #
#                           Prep                           #
#                                                          #
############################################################
# Load packages
library(shiny)
library(ggplot2)

############################################################
#                                                          #
#                          ggplot Theme                    #
#                                                          #
############################################################

scale_colour_continuous <- function(...) {
    scale_colour_grey(...,
                      start = 0.4, end = 1)
}

scale_fill_continuous <- function(...) {
    scale_fill_grey(...,
                    start = 0.4, end = 1)
}

scale_colour_discrete <- function(...) {
    scale_colour_grey(...,
                      start = 0.4, end = 1)
}

scale_fill_discrete <- function(...) {
    scale_fill_grey(...,
                    start = 0.4, end = 1)
}

# Update theme_linedraw
theme_academic <- theme_linedraw(base_size = 14) +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = '#000000',
                                   size = rel(1)),
          axis.ticks = element_line(colour = '#000000',
                                    size = rel(1)),
          panel.grid = element_blank(),
          legend.title = element_blank())

# Set 'theme_new' as the default
theme_set(theme_academic)


############################################################
#                                                          #
#                         Shiny UI                         #
#                                                          #
############################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ShinyPlotR"),
  
   # Sidebar that changes depending on which tab is selected
   sidebarLayout(
      sidebarPanel(
          conditionalPanel(condition="input.tabSelected==1",h5("Insert Text Here")),
          
          conditionalPanel(condition = "input.tabSelected==2",
                           fileInput("file", "Upload file"),
                           radioButtons("sep","Delimited file data separator", 
                                        choices = c(Comma = ",", 
                                                    Semicolon = ";", 
                                                    Tab = "/t", 
                                                    `Blank space` = " ")),
                           em("(Max file size is 5MB)")),
          
          conditionalPanel(condition = "input.tabSelected==3",
                           uiOutput("plotType"),
                           uiOutput("xvar"), 
                           uiOutput("yvar"),
                           uiOutput("boxGrouping"),
                           uiOutput("xLabel"),
                           uiOutput("yLabel"),
                           uiOutput("plotTitle")
                           
                        
                           )
      ),
      
      # Show relevant ouput for each tab
      mainPanel(
          tabsetPanel(type = "tab",
                      tabPanel("About", value=1, conditionalPanel(condition = "input.choice==1"),
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce maximus orci vitae erat commodo tincidunt. 
                               Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Quisque lobortis."),
                        p("Quisque urna libero, dapibus sed nibh pretium, congue vestibulum urna. Aliquam elementum nisl quis nibh porta varius.
                               Etiam malesuada, arcu nec iaculis sollicitudin, lacus purus aliquet ipsum, sit amet dapibus arcu mi vitae turpis.
                               Nam ut est quis neque porta aliquam ut eget augue.")),
                      
                      tabPanel("Data Upload", value=2,
                               conditionalPanel(condition = "input.choice==2"),
                               h2('Summary of uploaded data'),
                               htmlOutput("waiting"),
                               verbatimTextOutput("dimensions"),
                               verbatimTextOutput("tibble_head")),
                      
                      tabPanel("Data Visualization",value=3, 
                               htmlOutput("waitingPlot"),
                               downloadButton("download_plot_PDF", "Download Plot"),
                               plotOutput("plot")),
                      id = "tabSelected"
          )
      )
   )
)


############################################################
#                                                          #
#                       Shiny server                       #
#                                                          #
############################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   ################
   #  UPLOAD TAB  #
   ################
   
   # Generate reactive dataframe object
   df <- reactive({
       if(is.null(input$file)) {
           return()
       } else {
           readr::read_delim(file = input$file$datapath, 
                             delim = input$sep)
       }
   })
   
   # Waiting message
   output$waiting <- renderPrint({
       if(!is.null(df())) {
           tags$p("")
           } else {
               tags$p("Waiting for data to be uploaded.")
           }
       })
    
   # Render data frame dimensions
   output$dimensions <- renderText({
       if(!is.null(df())) {
           rows <- paste("Rows:", "\t\t", dim(df())[1])
           columns <- paste("Columns:", "\t", dim(df())[2])
           paste(rows, columns,
                 sep = "\n")
           }
       })
    
   # Render data frame head
   output$tibble_head <- renderPrint({
       if(!is.null(df())) {
           df()
           }
       })
   
   ########################
   #  DATA VISUALISATION  #
   ########################
   
   output$uploadMessage <- renderText({
       if(is.null(df())){
           tags$h("pls upload data")
       } else {
           return(NULL)
       }
       
   })
   

   
   var <- reactive({
        "userData" = names(df())
   })
   
   output$plotType <- renderUI({
       radioButtons("graphType", "Choose Plot", 
                    choices = c("Scatter Plot" = "scatter", 
                                "Histogram" = "histo", 
                                "Box Plot" = "box" ))
   })

   output$xvar <- renderUI({
       if (is.null(df()) || is.null(input$graphType) || input$graphType == "box") return(NULL)
       selectInput("x",
                   "x variable:",
                   choices = var())
   })
   
   output$yvar <- renderUI({
       if (is.null(df()) || is.null(input$graphType) || input$graphType == "histo") return(NULL)
       selectInput("y",
                   "y variable:",
                   choices = var())
   })
   
   output$boxGrouping <- renderUI({
       if (is.null(df()) || input$graphType != "box") return(NULL)
       selectInput("grouping",
                   "Grouping:",
                   choices = var())
       
   })
   
   output$xLabel <- renderUI({
       if (is.null(df()) || is.null(input$graphType)) return(NULL)
       textInput(inputId = "xLab",
                 label = "X axis label:",
                 value = input$x)
   })
   
   output$yLabel <- renderUI({
       if (is.null(df()) || is.null(input$graphType) || input$graphType == "histo" ) return(NULL)
       textInput(inputId = "yLab",
                 label = "Y axis label:",
                 value = input$y)
   })
   
   output$plotTitle <- renderUI({
       if (is.null(df())) return(NULL)
       textInput(inputId = "title",
                 label = "Title")
   })
   
   output$plot <- renderPlot  ({
       if (is.null(df()) || is.null(input$graphType)) return(NULL)
       if (input$graphType == "scatter") {
           p <- ggplot(df(),
                       aes(x = df()[,input$x], y = df()[,input$y])) + geom_point()
           p + labs(x = input$xLab,
                    y = input$yLab) + ggtitle(input$title)
       } else if(input$graphType == "histo") {
           p <- ggplot(df(),
                       aes(x = df()[,input$x])) + geom_histogram()
           p + labs(x = input$xLab,
                    y = "Frequency") + ggtitle(input$title)
       } else if (input$graphType == "box") {
           p  <- ggplot(data=df(), aes(x= input$grouping, y=df()[,input$y]))
           p + geom_boxplot(aes(fill=df()[,input$grouping])) + 
               ylab(input$yLab) + xlab(input$grouping) + ggtitle(input$title)

           
       }
       
   })
   
   # Waiting message
   output$waitingPlot <- renderPrint({
       if(!is.null(df())) {
           tags$p("")
       } else {
           tags$p("Waiting for data to be uploaded.")
       }
   })
   
   ########################
   #  Download data       #
   ########################
   
   output$download_plot_PDF <- downloadHandler(
       filename <- function() {
           paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
       },
       content <- function(file) {
           if (input$graphType == "scatter") {
               p <- ggplot(df(),
                           aes(x = df()[,input$x], y = df()[,input$y])) + geom_point()
               p + labs(x = input$xLab,
                        y = input$yLab) + ggtitle(input$title)
           } else if(input$graphType == "histo") {
               p <- ggplot(df(),
                           aes(x = df()[,input$x])) + geom_histogram()
               p + labs(x = input$xLab,
                        y = "Frequency") + ggtitle(input$title)
           } else if (input$graphType == "box") {
               p  <- ggplot(data=df(), aes(x= input$grouping, y=df()[,input$y]))
               p + geom_boxplot(aes(fill=df()[,input$grouping])) + 
               ylab(input$yLab) + xlab(input$grouping) + ggtitle(input$title)
               
               
           }
           
           ggsave(file, p)
       },
       contentType = "application/pdf" # MIME type of the image
       
   )
   
   

   
   
   }

############################################################
#                                                          #
#                     Run Application                      #
#                                                          #
############################################################
shinyApp(ui = ui, server = server)

