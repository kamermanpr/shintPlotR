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
                      
                      tabPanel("About", value=1, conditionalPanel(condition = "input.choice==1"),
                               
                                p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce maximus orci vitae erat commodo tincidunt. 
                               Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Quisque lobortis, 
                               metus sit amet volutpat mollis, velit ex lobortis ligula, non hendrerit risus nulla eu risus. Praesent et ultrices orci,
                               et molestie sem. Suspendisse elementum mauris eleifend dolor vestibulum interdum. Aenean mauris odio, efficitur in libero in,
                               aliquam venenatis metus. Nunc at accumsan metus. Donec non dui vitae nulla sodales lobortis at sit amet quam. 
                               Duis pellentesque odio a porta posuere."),
                                p("Quisque urna libero, dapibus sed nibh pretium, congue vestibulum urna. Aliquam elementum nisl quis nibh porta varius.
                               Etiam malesuada, arcu nec iaculis sollicitudin, lacus purus aliquet ipsum, sit amet dapibus arcu mi vitae turpis.
                               Nam ut est quis neque porta aliquam ut eget augue. Nulla ut semper dui. Donec luctus lectus ut risus consequat,
                               pharetra lobortis risus efficitur. Aliquam vitae ipsum malesuada, rhoncus sem et, pellentesque erat. 
                               Duis sed sem vel odio fringilla eleifend ut sit amet ex. Fusce eleifend, odio eu faucibus iaculis, 
                                    mi ligula fermentum velit, sit amet lacinia arcu mi at risus. Nam vitae tristique nibh.")),
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

