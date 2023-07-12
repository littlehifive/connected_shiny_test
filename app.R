library(shinydashboard)
library(ggplot2)
library(scales)

source(here::here("R/data_prep.R"))
source(here::here("R/plotting.R"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Youth Impact", 
                  tags$li(class = "dropdown", 
                          uiOutput("round_selection"))),
  dashboardSidebar(disable = T), # disable sidebar
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color: steelblue; }
        .skin-blue .main-header .navbar { background-color: steelblue; }
        .box {
          background-color: white; 
          text-align: center;
        }
        .box .inner h3 {
          font-size: 2em;
        }
      "))
    ),
    fluidRow(
      box(title = "Total Students", div(class = "inner", h3(textOutput("total_students"))), width = 3, solidHeader = TRUE, status = "primary"),
      box(title = "Total Schools", div(class = "inner", h3(textOutput("total_schools"))), width = 3, solidHeader = TRUE, status = "primary"),
      box(title = "Total Facilitators", div(class = "inner", h3(textOutput("total_facilitators"))), width = 3, solidHeader = TRUE, status = "primary"),
      box(title = "Total Regions", div(class = "inner", h3(textOutput("total_regions"))), width = 3, solidHeader = TRUE, status = "primary")
    ),
    fluidRow(
      box(title = "School % Statistics", width = 3, plotlyOutput("n_schools")),
      box(title = "Region % Statistics", width = 3, plotlyOutput("n_regions")),
      box(title = "Gender % Statistics", width = 3, plotlyOutput("n_gender")),
      box(title = "Treatment % Statistics", width = 3, plotlyOutput("n_treatment"))
    ),
    fluidRow(
      box(title = "Proportion of Student Level at Endline by Treatment Condition", width = 3, plotlyOutput("p_stu_level")),
      box(title = "Proportion of Innumeracy at Endline by Treatment Condition", width = 3, plotlyOutput("p_innumeracy")),
      box(title = "Proportion of Numeracy at Endline by Treatment Condition", width = 3, plotlyOutput("p_numeracy")),
      box(title = "Proportion of Students that Learned New Operations at Endline by Treatment Condition", width = 3, plotlyOutput("p_learn_newop"))
    )
    # fluidRow(
    #   box(title = "Progress by treatment", width = 12, plotlyOutput("progress_by_treatment"))
    # )
  )
)

# Server
server <- function(input, output, session) {
  
  output$round_selection <- renderUI({
    selectizeInput("selected_round", 
                   HTML("Select Rounds <br> (Hit backspace to omit certain rounds):"), 
                   choices = unique(dat$round), 
                   selected = unique(dat$round), 
                   multiple = TRUE)
  })
  
  selected_data <- reactive({
    validate(
      need(input$selected_round != "", "Please select at least one round.")
    )
    dat[dat$round %in% input$selected_round, ]
  })
  
  output$total_students <- renderText({nrow(selected_data())})
  output$total_schools <- renderText({length(unique(selected_data()$school_name_bl))})
  output$total_facilitators <- renderText({length(unique(selected_data()$facilitator_id_bl))})
  output$total_regions <- renderText({length(unique(selected_data()$region_bl))})
  
  output$n_schools <- renderPlotly({
    plot_schools(selected_data())
  })
  
  output$n_regions <- renderPlotly({
    plot_regions(selected_data())
  })

  output$n_gender <- renderPlotly({
    plot_gender(selected_data())
  })
  
  output$n_treatment <- renderPlotly({
    plot_treatment(selected_data())
  })
  
  output$p_stu_level <- renderPlotly({
    plot_stu_level_e(selected_data())
  })
  
  output$p_innumeracy <- renderPlotly({
    plot_innumeracy_e(selected_data())
  })
  
  output$p_numeracy <- renderPlotly({
    plot_numeracy_e(selected_data())
  })
  
  output$p_learn_newop <- renderPlotly({
    plot_learn_newop_e(selected_data())
  })
  
  # output$progress_by_treatment <- renderPlotly({
  #   plot_progress(selected_data())
  # })
}

shinyApp(ui, server)
