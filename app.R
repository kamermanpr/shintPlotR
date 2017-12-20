############################################################
#                                                          #
#                           Prep                           #
#                                                          #
############################################################
# Load packages
library(shiny)
library(ggplot2)



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
                           radioButtons("graphType", "Choose Plotlolo", 
                                        selected = character(0),
                                        choices = c("Scatter Plot" = "scatter", 
                                                    "Histogram" = "histo", 
                                                    "Box Plot" = "/t" )),
                           uiOutput("xvar"), # vx is coming from renderUI in server.r
                           uiOutput("yvar") # vy is coming from renderUI in server.r
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
                               conditionalPanel(condition = "input.choice==3"), 
                               plotOutput("p")),
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
   
   var <- reactive({
        "userData" = names(df())
   })
   

   output$xvar <- renderUI({
       if (is.null(df())) return(NULL)
       selectInput("x",
                   "x variable:",
                   choices = var())
   })
   
   output$yvar <- renderUI({
       if (is.null(df())) return(NULL)
       selectInput("y",
                   "y variable:",
                   choices = var())
   })

   output$p <- renderPlot  ({
       if (is.null(df())) return(NULL)
       ggplot(df(),
       aes(x = df()[,input$x], y = df()[,input$y])) + geom_point()
   })
   
   
   
   
   }

############################################################
#                                                          #
#                     Run Application                      #
#                                                          #
############################################################
shinyApp(ui = ui, server = server)

