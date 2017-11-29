############################################################
#                                                          #
#                           Prep                           #
#                                                          #
############################################################
# Load packages
library(shiny)

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
          conditionalPanel(condition="input.tabSelected==1",h5("This is Tab 1")),
          conditionalPanel(condition = "input.tabSelected==2",h5("This is Tab 2")),
          conditionalPanel(condition = "input.tabSelected==3",h5("This is Tab 3"), sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30))
      ),
      
      # Show relevant ouput for each tab
      mainPanel(
          tabsetPanel(type = "tab",
                      
                      tabPanel("About", value=1, conditionalPanel(condition = "input.choice==1")),
                      tabPanel("Data Upload", value=2, conditionalPanel(condition = "input.choice==2")),
                      tabPanel("Data Visualization",value=3, conditionalPanel(condition = "input.choice==3"), plotOutput("distPlot")),
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
}

############################################################
#                                                          #
#                     Run Application                      #
#                                                          #
############################################################
shinyApp(ui = ui, server = server)

