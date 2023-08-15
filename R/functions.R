# set header to dropdown menu for selecting rounds
set_header <- function(title){
  dashboardHeader(
    title = "Youth Impact",
    tags$li(class = "dropdown",
            tags$a(id = "round_selection",
                   style = "padding-top: 0",
                   uiOutput("round_selection") 
            )   
    )
  )
}


# set body font style
set_style <- function(){
  tags$head(
    tags$style(
      HTML(".skin-blue .main-header .logo { background-color: steelblue; }
            .skin-blue .main-header .navbar { background-color: steelblue; }
            .box {
                  background-color: white; 
                  text-align: center;
                }
            .box .inner h3 {
                  font-size: 2em;
                }")
      )
    )
}

# Re-format dropdown menu for round selection
format_dropdown <- function(inputId = "selected_round_1"){
    renderUI({
      pickerInput(
        inputId = inputId,
        label = HTML("<strong>Select Rounds</strong> <br> (Click on the dropdown menu to select/omit certain rounds):"),
        choices = unique(dat$round),
        selected = unique(dat$round),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",  # show the count format when more than 2 items are selected
          `count-selected-text` = "{0} rounds selected",  # custom text format
          `show-tick` = TRUE
        )
      )
    })
}