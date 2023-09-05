
# Lets make some simulated insect stuff

# make a string with the normal distribution

aphid.density <- rnorm(200, mean = 100, sd = 25)

#sort (for funsies)
aphid.density <- sort(aphid.density)
hist(aphid.density)

#step plot
plot(aphid.density, type="s")
#time series plot
plot(ts(aphid.density))

# ok what if we make a cyclical plot?
# messy but kinda neat

x = sin(seq(0, 6, 0.25) * pi)*rnorm(200,100,25)
plot(x, type="b")


# what about a logistic growth curve?
z = SSlogis(1:25, Asym = 100, xmid = 10, scal = 5)
plot(z, type="b")

# asymp = carrying capacity
# 1:25 = generations
# x mid is when the pop hits the exponential phase
# scale = width of the exponential phase (larger = slower growth rate)



# ok I have a simple function that lets you manipulate generation time in an exponential growth curve scenario


aphid.simulator <- function(generation.time) {
  aphid.abundance <- plot(SSlogis(1:25, Asym = 100, xmid = 10, scal = (generation.time)))
  return(aphid.abundance)
}

aphid.simulator(generation.time=2)

# can I get this simple thing into a very very basic shiny app?
# shiny ####
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Aphid simulator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Aphid generation time",
                  min = 0.1,
                  max = 10,
                  value = 0.1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Aphid population size",
         main = "Aphid population plot")
    
  })
  
  
  # crap i don't know how to replace faithful$waiting with aphid abundance
  
}

#this probably isn't the best because the output is just a histogram with a changing # of bins. good practice though

# output app
shinyApp(ui, server)





# ok lets try another thing


counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}

ui <- fluidPage(
  counterButton("counter1", "Counter #1")
)

server <- function(input, output, session) {
  counterServer("counter1")
}

shinyApp(ui, server)