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