header <- dashboardHeader(
  title = "Conservation Visualization Tool - Alpha",
  titleWidth = 450
  
)

vars <- c(
  "Alabama"="AL","Louisiana"="LA","Texas"="TX","Florida"="FL","Mississippi"="MS"
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "button.css"),
    useShinyjs()
  ),
  fluidRow(
    column(width = 8,
           div(id="mapgeneral", 
               style="position: fixed; width: 65%;",
               box(width = NULL, solidHeader = TRUE, 
               leafglOutput(
                 outputId = 'myMap',height = "650px"
               )
               )
           )
    ),
    column(width = 4,
           bsCollapse(id = "collapseExample", open = "Panel 1",
                      bsCollapsePanel("Get started",value = "Panel 1", 
                                      "Welcome to Conservation visualization tool! This tool provides region-wide visualization based on data measure selected.",
                                      hr(),
                                      radioGroupButtons(
                                        inputId = "geography",
                                        label = "Select a geographic parameter:",
                                        size = "lg",
                                        choiceNames = list(
                                          
                                          HTML("<img src='region.JPG' height='60' width='100'>
                                               <p>Regions</p>"),
                                          HTML("<img src='texas.JPG' height='60' width='100'>
                                               <p>States</p>")
                                        ),
                                        choiceValues = list(
                                          "region", "states"
                                        ),
                                        selected = "region",
                                        width = '100%'
                                      ),
                                      conditionalPanel("input.geography=='states'",
                                                       pickerInput("states","Select states",choices = vars,width = "90%",
                                                                   multiple = TRUE,selected = NULL,
                                                                   options = list(`actions-box` = TRUE,maxOptions = 1))
                                                       ),
                                      actionButton("nextto2","Start fresh"),
                                      actionButton("loadbookmark","Load a model from Bookmark"),
                                      style = "success"),
                      bsCollapsePanel("Data measures", value = "Panel 2",style = "info",
                                      box(width = NULL,solidHeader = F,status = "warning",
                                      pickerInput("habitat","Habitat",choices = hab_measure,width = "90%",
                                                  multiple = TRUE,selected = NULL,
                                                  options = list(`actions-box` = TRUE, `maxOptions` = 1)),
                                      pickerInput("wq","Water Quality & Quantity",choices = wq_measure,width = "90%",
                                                  multiple = TRUE,selected = NULL,
                                                  options = list(`actions-box` = TRUE,maxOptions = 1)),
                                      pickerInput("lcmr","Living Coastal Marine Resource",choices = lcmr_measure,width = "90%",
                                                  multiple = TRUE,selected = NULL,
                                                  options = list(`actions-box` = TRUE,maxOptions = 1)),
                                      pickerInput("cl","Community Resilience",choices = cl_measure,width = "90%",
                                                  multiple = TRUE,selected = NULL,
                                                  options = list(`actions-box` = TRUE,maxOptions = 1)),
                                      pickerInput("eco","Gulf Economy",choices = eco_measure,width = "90%",
                                                  multiple = TRUE,selected = NULL,
                                                  options = list(`actions-box` = TRUE,maxOptions = 1))
                                      ), 
                                      actionButton("nextto3","Load measures selected"),
                                      actionButton("individual1","Explore individual dataset with r shiny", onclick = "window.open('https://scagulf.shinyapps.io/SingleMeasureV1/')")
                                      ),
                      bsCollapsePanel("Goal weights", value = "Panel 3", 
                                      
                                      sliderInput("habslide","Habitat",min=0,max=100,value = 0,step = 10),
                                      sliderInput("wqslide","Water Quality",min=0,max=100,value = 0,step = 10),
                                      sliderInput("lcmrslide","LCMR",min=0,max=100,value = 0,step = 10),
                                      sliderInput("clslide","Community Resilience",min=0,max=100,value = 0,step = 10),
                                      sliderInput("ecoslide","Economy",min=0,max=100,value = 0,step = 10),
                                      p("Total sum:"),
                                      verbatimTextOutput("sumgoalweight"),
                                      actionButton("nextto4","Finalize goal weights"),
                                      style = "primary"),
                      bsCollapsePanel("Review & Result",value="Panel 4", 
                                      "Review the weights options below and proceed to result",
                                      hr(),
                                      tags$b("Goal Weights Summary: "),actionButton("adjustgoal","Adjust Goal Weights"),
                                      tableOutput("goalweight"),
                                      hr(),
                                      tags$b("Measure Weights Summary: "),actionButton("adjustmeasure","Adjust Measure Weights"),
                                      tableOutput("measureselected"),
                                      hr(),
                                      actionButton("nextto1","Result"),
                                      actionButton("screening","Screening result"),
                                      style = "danger")
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,skin = "black"
)