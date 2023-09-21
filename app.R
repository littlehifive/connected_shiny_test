library(shinydashboard)
library(crosstalk)
library(bslib)
library(shinyWidgets)
library(shiny)

#library(ggplot2)
#library(scales)

source(here::here("R/data_prep.R"))
source(here::here("R/plotting.R"))
source(here::here("R/functions.R"))

# UI
# dat_app <- SharedData$new(dat)
  
# round_options <- filter_select("round", "Round Selection", dat_app, ~round)
# plot_test <- plot_ly(dat) |> add_histogram(x = ~region_bl)

# value boxes in page 1
list_value_box <- list(
  value_box(
    title = "Number of Students",
    value = textOutput("total_students"),
    showcase = bsicons::bs_icon("person"),
    theme_color = "success"),
  
  value_box(
    title = "Number of Schools",
    value = textOutput("total_schools"),
    showcase = bsicons::bs_icon("hospital"),
    theme_color = "success"),

  value_box(
    title = "Number of Facilitators",
    value = textOutput("total_facilitators"),
    showcase = bsicons::bs_icon("person-workspace"),
    theme_color = "success"),
  
  value_box(
    title = "Number of Regions",
    value = textOutput("total_regions"),
    showcase = bsicons::bs_icon("pin-map-fill"),
    theme_color = "success")
)

# create sidebar page #1
sidebar_1 <- page_sidebar(
  sidebar = uiOutput("round_selection_1"),
  #fillable = FALSE,
  page_fillable(
    layout_column_wrap(
      width = "250px",
      height = "100px",
      !!!list_value_box
    ),
    layout_column_wrap(
      width = 1/2,
      height = "350px",
      card(card_header("School % Statistics"), plotlyOutput("n_schools")),
      card(card_header("Region % Statistics"), plotlyOutput("n_regions"))
    ),
    layout_column_wrap(
      width = 1/2,
      height = "350px",
      card(card_header("Gender % Statistics"), plotlyOutput("n_gender")),
      card(card_header("Treatment % Statistics"), plotlyOutput("n_treatment"))
    )
  )
)

# create sidebar page #2
sidebar_2 <- page_sidebar(
  sidebar = uiOutput("round_selection_2"),
  # fillable = FALSE,
  layout_columns(
  navset_card_tab(
    nav_panel(title = "Student Levels (Pre-Post Comparison)", 
              layout_column_wrap(
                width = 1/2,
                height = "400px",
                card(card_header("Baseline Student Levels"), plotlyOutput("p_stu_level_b")),
                card(card_header("Endline Student Levels"), plotlyOutput("p_stu_level_e"))
                )
              ),
    nav_panel(title = "Student Levels (Pre-Post Comparison by Round)", 
              layout_column_wrap(
                width = 1/2,
                height = "500px",
                card(card_header("Baseline Student Levels by Round"), plotlyOutput("p_stu_level_b_r")),
                card(card_header("Endline Student Levels by Round"), plotlyOutput("p_stu_level_e_r"))
              )
    )
))

)

# create sidebar page #3
sidebar_3 <- page_sidebar(
  sidebar = uiOutput("round_selection_3"),
  fillable = FALSE,
  layout_columns(
    navset_card_tab(
      nav_panel(title = "Student Innumeracy (Pre-Post Comparison)", 
                layout_column_wrap(
                  width = 1/3,
                  height = "350px",
                  card(card_header("Baseline and Endline Innumeracy"), plotlyOutput("p_innumeracy_be")),
                )
      ),
      nav_panel(title = "Student Innumeracy (Pre-Post Comparison by Round)", 
                layout_column_wrap(
                  width = 1/2,
                  #card(card_header("Baseline Innumeracy by Round"), plotlyOutput("p_innumeracy_b_r")),
                  card(card_header("Endline Innumeracy by Round"), plotlyOutput("p_innumeracy_e_r"))
                )
      )
    )),
  
  layout_columns(
    navset_card_tab(
      nav_panel(title = "Student Numeracy (Pre-Post Comparison)", 
                layout_column_wrap(
                  width = 1/3,
                  height = "350px",
                  card(card_header("Baseline and Endline Numeracy"), plotlyOutput("p_numeracy_be")),
                )
      ),
      nav_panel(title = "Student Numeracy (Pre-Post Comparison by Round)", 
                layout_column_wrap(
                  width = 1/2,
                  #card(card_header("Baseline Numeracy by Round"), plotlyOutput("p_numeracy_b_r")),
                  card(card_header("Endline Numeracy by Round"), plotlyOutput("p_numeracy_e_r"))
                )
      )
    )),
  
  layout_columns(
    nav_panel(title = "Learned New Operations", 
              layout_column_wrap(
                width = 1/2,
                card(card_header("Endline Learned New Operations by Round"), plotlyOutput("p_learn_newop_e_r"))
              )
    )
    
  )
  
)

# create UI
ui <- page_navbar(
  title = "Youth Impact - ConnectEd Dashboard",
  nav_panel("1. Descriptive Statistics", sidebar_1),
  nav_panel("2. Student Levels", sidebar_2),
  nav_panel("3. Innumeracy, Numeracy, and New Operations Learned", sidebar_3)
)

# Old UI
# ui <- dashboardPage(
#   set_header(title = "Youth Impact"),
#   dashboardSidebar(disabled = TRUE),
#   dashboardBody(
#     tags$head(tags$style(
#       HTML("
#              .sidebar-menu { padding-top: 60px; }
#              ")
#     )),
#     dashboardSidebar(
#       sidebarMenu(
#         menuItem("Page 1", tabName = "page1"),
#         menuItem("Page 2", tabName = "page2")
#       )
#     ),
#     tabItems(
#       tabItem(tabName = "page1",
#               set_style(),
#               fluidRow(
#                 box(title = "Total Students", div(class = "inner", h3(textOutput("total_students"))), width = 3, solidHeader = TRUE, status = "primary"),
#                 box(title = "Total Schools", div(class = "inner", h3(textOutput("total_schools"))), width = 3, solidHeader = TRUE, status = "primary"),
#                 box(title = "Total Facilitators", div(class = "inner", h3(textOutput("total_facilitators"))), width = 3, solidHeader = TRUE, status = "primary"),
#                 box(title = "Total Regions", div(class = "inner", h3(textOutput("total_regions"))), width = 3, solidHeader = TRUE, status = "primary")
#               ),
#               fluidRow(
#                 box(title = "School % Statistics", width = 3, plotlyOutput("n_schools")),
#                 box(title = "Region % Statistics", width = 3, plotlyOutput("n_regions")),
#                 box(title = "Gender % Statistics", width = 3, plotlyOutput("n_gender")),
#                 box(title = "Treatment % Statistics", width = 3, plotlyOutput("n_treatment"))
#               ),
#               fluidRow(
#                 box(title = "Proportion of Student Level at Endline by Treatment Condition", width = 6, plotlyOutput("p_stu_level"))
#               )
#       ),
#       
#       tabItem(tabName = "page2",
#               set_style(),
#               fluidRow(
#                 box(title = "Proportion of Innumeracy at Endline by Treatment Condition", width = 6, plotlyOutput("p_innumeracy"))
#               ),
#               fluidRow(
#                 box(title = "Proportion of Numeracy at Endline by Treatment Condition", width = 6, plotlyOutput("p_numeracy")),
#                 box(title = "Proportion of Students that Learned New Operations at Endline by Treatment Condition", width = 6, plotlyOutput("p_learn_newop"))
#               )
#       )
#     )
#   )
# )


# Server
server <- function(input, output, session) {
  
  # see format_dropdown function in functions.R
  # I haven't found a workaround for setting the round selections globally across pages
  
  # first page round selection
  output$round_selection_1 <- format_dropdown(inputId = "selected_round_1")
  
  selected_data_1 <- reactive({
    validate(
      need(input$selected_round_1 != "", "Please select at least one round.")
    )
    dat[dat$round %in% input$selected_round_1, ]
  })
  
  # second page round selection
  output$round_selection_2 <- format_dropdown(inputId = "selected_round_2")
  
  selected_data_2 <- reactive({
    validate(
      need(input$selected_round_2 != "", "Please select at least one round.")
    )
    dat[dat$round %in% input$selected_round_2, ]
  })

  # third page round selection
  output$round_selection_3 <- format_dropdown(inputId = "selected_round_3")
  
  selected_data_3 <- reactive({
    validate(
      need(input$selected_round_3 != "", "Please select at least one round.")
    )
    dat[dat$round %in% input$selected_round_3, ]
  })
  
  output$total_students <- renderText({nrow(selected_data_1())})
  output$total_schools <- renderText({length(unique(selected_data_1()$school_name_bl))})
  output$total_facilitators <- renderText({length(unique(selected_data_1()$facilitator_id_bl))})
  output$total_regions <- renderText({length(unique(selected_data_1()$region_bl))})
  
  output$n_schools <- renderPlotly({
    plot_schools(selected_data_1())
  })
  
  output$n_regions <- renderPlotly({
    plot_regions(selected_data_1())
  })

  output$n_gender <- renderPlotly({
    plot_gender(selected_data_1())
  })
  
  output$n_treatment <- renderPlotly({
    plot_treatment(selected_data_1())
  })
  
  output$p_stu_level_b <- renderPlotly({
    plot_stu_level_b(data = selected_data_2())
  })
  
  output$p_stu_level_e <- renderPlotly({
    plot_stu_level_e(data = selected_data_2())
  })
  
  output$p_stu_level_b_r <- renderPlotly({
    plot_stu_level_b_r(data = selected_data_2(),
                       selected_rounds = input$selected_round_2)
  })
  
  output$p_stu_level_e_r <- renderPlotly({
    plot_stu_level_e_r(data = selected_data_2(),
                     selected_rounds = input$selected_round_2)
  })
  
  output$p_innumeracy_be <- renderPlotly({
    plot_innumeracy_be(data = selected_data_3())
  })
  
  # output$p_innumeracy_b_r <- renderPlotly({
  #   plot_innumeracy_b_r(data = selected_data_3(),
  #                     selected_rounds = input$selected_round_3
  #   )
  # })
  
  output$p_innumeracy_e_r <- renderPlotly({
    plot_innumeracy_e_r(data = selected_data_3(),
                      selected_rounds = input$selected_round_3
                      )
  })
  
  output$p_numeracy_be <- renderPlotly({
    plot_numeracy_be(data = selected_data_3())
  })
  
  # output$p_numeracy_b_r <- renderPlotly({
  #   plot_numeracy_b_r(data = selected_data_3(),
  #                       selected_rounds = input$selected_round_3
  #   )
  # })
  
  output$p_numeracy_e_r <- renderPlotly({
    plot_numeracy_e_r(data = selected_data_3(),
                        selected_rounds = input$selected_round_3
    )
  })
  
  output$p_learn_newop_e_r <- renderPlotly({
    plot_learn_newop_e_r(data = selected_data_3(),
                    selected_rounds = input$selected_round_3
    )
  })
  
  # output$progress_by_treatment <- renderPlotly({
  #   plot_progress(selected_data())
  # })
}

shinyApp(ui, server)
