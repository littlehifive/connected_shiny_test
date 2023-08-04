library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)

# Function to plot schools
plot_schools <- function(data = dat) {
  
  # Create a data frame with counts of students per school
  df <- as.data.frame(table(data$school_name_bl)) |>
    rename(School = Var1, Count = Freq) |>
    arrange(desc(Count))
  
  # Only keep top 7 schools and group the rest as "Other"
  df <- df[1:7,] |> 
    bind_rows(data.frame(School = "Other", Count = sum(df$Count[-c(1:7)])))
  
  # Generate a color palette
  color_palette <- brewer.pal(length(unique(df$School)), "Blues")
  
  # Create a plotly pie chart
  p <- df |>
    plot_ly(labels = ~School, values = ~Count) |> 
    add_pie(hole = 0.6, marker = list(colors = color_palette)) |> 
    layout(showlegend = TRUE, 
           legend = list(orientation = "v", x = 1, y = 0.5), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}

# Function to plot regions
plot_regions <- function(data = dat) {
  
  # Similar process as in plot_schools, but for regions
  df <- as.data.frame(table(data$region_bl)) |>
    rename(Region = Var1, Count = Freq) |>
    arrange(desc(Count))
  
  color_palette <- brewer.pal(length(unique(df$Region)), "Blues")
  
  p <- df |>
    plot_ly(labels = ~Region, values = ~Count) |> 
    add_pie(hole = 0.6, marker = list(colors = color_palette)) |> 
    layout(showlegend = TRUE, 
           legend = list(orientation = "v", x = 1, y = 0.5), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}

# Function to plot gender
plot_gender <- function(data = dat){
  
  # Similar process as above, but for gender, and we're creating a bar chart instead
  df <- as.data.frame(table(data$stud_sex_bl)) |>
    rename(Gender = Var1, Count = Freq) |>
    arrange(desc(Count))
  
  # Calculate proportions for the hover text
  df$Proportion <- df$Count / sum(df$Count)
  df$CountText <- paste('Count: ', df$Count)
  df$ProportionText <- paste('Proportion: ', round(df$Proportion*100, 2), '%')
  
  p <- df %>%
    plot_ly(x = ~Gender, 
            y = ~Proportion, 
            type = 'bar', 
            color = ~Gender, 
            colors = 'Blues', 
            text = ~paste(CountText, '<br>', ProportionText), 
            hoverinfo = 'text') %>%
    layout(yaxis = list(title = 'Proportion'), 
           xaxis = list(title = 'Gender'))
  
  return(p)
}

# Function to plot treatment assignment
plot_treatment <- function(data = dat){
  
  # Similar process as above, but for treatment
  df <- as.data.frame(table(data$treatment)) |>
    rename(Treatment = Var1, Count = Freq) |>
    arrange(desc(Count))
  
  df$Proportion <- df$Count / sum(df$Count)
  df$CountText <- paste('Count: ', df$Count)
  df$ProportionText <- paste('Proportion: ', round(df$Proportion*100, 2), '%')
  
  p <- df %>%
    plot_ly(x = ~Treatment, 
            y = ~Proportion, 
            type = 'bar', 
            color = ~Treatment, 
            colors = 'Blues', 
            text = ~paste(CountText, '<br>', ProportionText), 
            hoverinfo = 'text') %>%
    layout(showlegend = FALSE,
           yaxis = list(title = 'Proportion'), 
           xaxis = list(title = 'Treatment', tickfont = list(size = 10)),
           margin = list(b = 80))
  
  return(p)
}

# Function to plot progress
plot_progress <- function(data = dat){
  
  # First, we create a cross-tabulation of treatment and level_diff
  df <- as.data.frame.matrix(table(data$treatment, data$level_diff))
  
  # Add a Treatment column
  df$Treatment <- rownames(df)
  
  # Convert wide data to long format
  df <- df %>%
    tidyr::pivot_longer(cols = -Treatment, names_to = "Progress", values_to = "Count")
  
  # Create a grouped bar chart
  p <- df %>%
    plot_ly(x = ~Treatment, 
            y = ~Count, 
            color = ~Progress, 
            type = 'bar',
            text = ~paste('Progress: ', Progress, '<br>', 'Count: ', Count),  # Add custom text
            hoverinfo = 'text',  # Show the custom text
            hovertemplate = '%{text}<extra></extra>') %>%  # Adjust hover template
    layout(yaxis = list(title = 'Count'), 
           xaxis = list(title = 'Treatment'), 
           barmode = 'group')
  
  return(p)
}

# Function to plot baseline student level (not by rounds)
plot_stu_level_b <- function(data = dat){

  df <- data |> 
    mutate(stud_level_baseline = factor(stud_level_endline, 
                                       levels = rev(c("Beginner", "Addition", 
                                                      "Subtraction", "Multiplication", "Division")), 
                                       ordered = TRUE)
           )
  
  df_counts_b <- df %>%
    count(treatment, stud_level_baseline) %>%
    na.omit() |> 
    group_by(treatment) %>%
    mutate(fraction = n / sum(n)) 
  
  p <- df_counts_b %>%
    plot_ly(x = ~treatment, y = ~fraction, type = 'bar', color = ~stud_level_baseline,
            legendgroup = ~stud_level_baseline,
            hoverinfo = "text", 
            hovertext = ~paste("Proportion of", stud_level_baseline, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%")) %>%
    layout(yaxis = list(title = 'Proportion', tickformat = ".0%", range = c(0, 1)), 
           xaxis = list(title = 'Treatment'), 
           barmode = 'stack',
           title = "")
  
  return(p)
  
}

# Function to plot endline student level (not by rounds)
plot_stu_level_e <- function(data = dat){
  
  df <- data |> 
    mutate(stud_level_endline = factor(stud_level_endline, 
                                        levels = rev(c("Beginner", "Addition", 
                                                       "Subtraction", "Multiplication", "Division")), 
                                        ordered = TRUE)
    )
  
  df_counts_b <- df %>%
    count(treatment, stud_level_endline) %>%
    na.omit() |> 
    group_by(treatment) %>%
    mutate(fraction = n / sum(n)) 
  
  p <- df_counts_b %>%
    plot_ly(x = ~treatment, y = ~fraction, type = 'bar', color = ~stud_level_endline,
            legendgroup = ~stud_level_endline,
            hoverinfo = "text", 
            hovertext = ~paste("Proportion of", stud_level_endline, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%")) %>%
    layout(yaxis = list(title = 'Proportion', tickformat = ".0%", range = c(0, 1)), 
           xaxis = list(title = 'Treatment'), 
           barmode = 'stack',
           title = "")
  
  return(p)
  
}

# Function to plot baseline student level by round
plot_stu_level_b_r <- function(data = dat,
                               selected_rounds = c("R5", "R6", "R7", "R8")){
  
  df <- data |> 
    mutate(stud_level_baseline = factor(stud_level_baseline, 
                                       levels = rev(c("Beginner", "Addition", 
                                                      "Subtraction", "Multiplication", "Division")), 
                                       ordered = TRUE))
  
  plot_list <- purrr::imap(selected_rounds, function(r, index) {
    data_subset <- df %>% filter(round == r)
    
    df_counts <- data_subset %>%
      count(round, treatment, stud_level_baseline) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) 
    
    show_legend = ifelse(index == 1, TRUE, FALSE)
    
    df_counts %>%
      plot_ly(x = ~treatment, y = ~fraction, type = 'bar', color = ~stud_level_baseline,
              legendgroup = ~stud_level_baseline,
              hoverinfo = "text", 
              hovertext = ~paste("Proportion of", stud_level_baseline, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
              showlegend = show_legend) %>%
      layout(yaxis = list(title = 'Proportion', tickformat = ".0%", range = c(0, 1)), 
             xaxis = list(title = paste0('Treatment (', r, ")"), tickfont = list(size = 8)), 
             barmode = 'stack',
             title = "")
    
    
  })
  
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
}

# Function to plot endline student level by round
plot_stu_level_e_r <- function(data = dat,
                               selected_rounds = c("R5", "R6", "R7", "R8")){
  
  df <- data |> 
    mutate(stud_level_endline = factor(stud_level_endline, 
                                       levels = rev(c("Beginner", "Addition", 
                                                  "Subtraction", "Multiplication", "Division")), 
                                       ordered = TRUE))
  
  plot_list <- purrr::imap(selected_rounds, function(r, index) {
    data_subset <- df %>% filter(round == r)
    
    df_counts <- data_subset %>%
      count(round, treatment, stud_level_endline) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) 
    
    show_legend = ifelse(index == 1, TRUE, FALSE)
    
    df_counts %>%
      plot_ly(x = ~treatment, y = ~fraction, type = 'bar', color = ~stud_level_endline,
              legendgroup = ~stud_level_endline,
              hoverinfo = "text", 
              hovertext = ~paste("Proportion of", stud_level_endline, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
              showlegend = show_legend) %>%
      layout(yaxis = list(title = 'Proportion', tickformat = ".0%", range = c(0, 1)), 
             xaxis = list(title = paste0('Treatment (', r, ")"), tickfont = list(size = 8)), 
             barmode = 'stack',
             title = "")
    
    
  })
  
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
}


# Function to plot baseline innumeracy (not by rounds)
plot_innumeracy_b <- function(data = dat){
  
    # Calculate the counts and proportions
    df_counts <- data %>%
      count(treatment, innumeracy_bl) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(innumeracy_bl == "Cannot add")
    
    # Plot
    p <- df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              hovertext = ~paste("Proportion of", innumeracy_bl, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = "Treatment"), 
             yaxis = list(title = "Proportion"))

  
  return(p)
  
}

# Function to plot endline innumeracy (not by rounds)
plot_innumeracy_e <- function(data = dat){
  
  # Calculate the counts and proportions
  df_counts <- data %>%
    count(treatment, innumeracy_el) %>%
    na.omit() |> 
    group_by(treatment) %>%
    mutate(fraction = n / sum(n)) |>
    filter(innumeracy_el == "Cannot add")
  
  # Plot
  p <- df_counts %>%
    plot_ly(x = ~treatment, 
            y = ~fraction, 
            type = "bar", 
            color = ~treatment, 
            colors = "Greens", 
            hovertext = ~paste("Proportion of", innumeracy_el, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
            hoverinfo = "text",
            showlegend = F) %>%
    layout(xaxis = list(title = "Treatment"), 
           yaxis = list(title = "Proportion"))
  
  
  return(p)
  
}

# Function to plot baseline innumeracy by round
plot_innumeracy_b_r <- function(data = dat,
                                selected_rounds = c("R5", "R6", "R7", "R8")){
  
  # Create a list of plots
  plot_list <- map(selected_rounds, function(r) {
    data_subset <- data %>% filter(round == r)
    
    # Calculate the counts and proportions
    df_counts <- data_subset %>%
      count(round, treatment, innumeracy_bl) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(innumeracy_bl == "Cannot add")
    
    # Plot
    df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              text = ~paste0("Round ", unique(df_counts$round), " - ", treatment, ": ", 
                             sprintf("%.1f", 100 * fraction),
                             "% Innumeracy at Baseline"), 
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = paste0("Treatment (", r, ")"), tickfont = list(size = 8)), 
             yaxis = list(title = "Proportion"))
  })
  
  # Check how many plots are selected
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  # Combine the plots
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
  
}

# Function to plot endline innumeracy by round
plot_innumeracy_e_r <- function(data = dat,
                              selected_rounds = c("R5", "R6", "R7", "R8")){
  
  # Create a list of plots
  plot_list <- map(selected_rounds, function(r) {
    data_subset <- data %>% filter(round == r)
    
    # Calculate the counts and proportions
    df_counts <- data_subset %>%
      count(round, treatment, innumeracy_el) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(innumeracy_el == "Cannot add")
    
    # Plot
    df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              text = ~paste0("Round ", unique(df_counts$round), " - ", treatment, ": ", 
                             sprintf("%.1f", 100 * fraction),
                             "% Innumeracy at Endline"), 
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = paste0("Treatment (", r, ")"), tickfont = list(size = 8)), 
             yaxis = list(title = "Proportion"))
  })
  
  # Check how many plots are selected
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  # Combine the plots
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))

  return(p)

}

# Function to plot baseline innumeracy (not by rounds)
plot_numeracy_b <- function(data = dat){
  
  # Calculate the counts and proportions
  df_counts <- data %>%
    count(treatment, numeracy_bl) %>%
    na.omit() |> 
    group_by(treatment) %>%
    mutate(fraction = n / sum(n)) |>
    filter(numeracy_bl == "Can divide")
  
  # Plot
  p <- df_counts %>%
    plot_ly(x = ~treatment, 
            y = ~fraction, 
            type = "bar", 
            color = ~treatment, 
            colors = "Greens", 
            hovertext = ~paste("Proportion of", numeracy_bl, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
            hoverinfo = "text",
            showlegend = F) %>%
    layout(xaxis = list(title = "Treatment"), 
           yaxis = list(title = "Proportion"))
  
  
  return(p)
  
}

# Function to plot endline numeracy (not by rounds)
plot_numeracy_e <- function(data = dat){
  
  # Calculate the counts and proportions
  df_counts <- data %>%
    count(treatment, numeracy_el) %>%
    na.omit() |> 
    group_by(treatment) %>%
    mutate(fraction = n / sum(n)) |>
    filter(numeracy_el == "Can divide")
  
  # Plot
  p <- df_counts %>%
    plot_ly(x = ~treatment, 
            y = ~fraction, 
            type = "bar", 
            color = ~treatment, 
            colors = "Greens", 
            hovertext = ~paste("Proportion of", numeracy_el, "in", "\n", treatment, "Group: ", "\n", round(fraction*100, 1), "%"),
            hoverinfo = "text",
            showlegend = F) %>%
    layout(xaxis = list(title = "Treatment"), 
           yaxis = list(title = "Proportion"))
  
  
  return(p)
  
}

# Function to plot baseline numeracy by round
plot_numeracy_b_r <- function(data = dat,
                                selected_rounds = c("R5", "R6", "R7", "R8")){
  
  # Create a list of plots
  plot_list <- map(selected_rounds, function(r) {
    data_subset <- data %>% filter(round == r)
    
    # Calculate the counts and proportions
    df_counts <- data_subset %>%
      count(round, treatment, numeracy_bl) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(numeracy_bl == "Can divide")
    
    # Plot
    df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              text = ~paste0("Round ", unique(df_counts$round), " - ", treatment, ": ", 
                             sprintf("%.1f", 100 * fraction),
                             "% Numeracy at Baseline"), 
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = paste0("Treatment (", r, ")"), tickfont = list(size = 8)), 
             yaxis = list(title = "Proportion"))
  })
  
  # Check how many plots are selected
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  # Combine the plots
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
  
}

# Function to plot endline numeracy by round
plot_numeracy_e_r <- function(data = dat,
                                selected_rounds = c("R5", "R6", "R7", "R8")){
  
  # Create a list of plots
  plot_list <- map(selected_rounds, function(r) {
    data_subset <- data %>% filter(round == r)
    
    # Calculate the counts and proportions
    df_counts <- data_subset %>%
      count(round, treatment, numeracy_el) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(numeracy_el == "Can divide")
    
    # Plot
    df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              text = ~paste0("Round ", unique(df_counts$round), " - ", treatment, ": ", 
                             sprintf("%.1f", 100 * fraction),
                             "% Numeracy at Endline"), 
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = paste0("Treatment (", r, ")"), tickfont = list(size = 8)), 
             yaxis = list(title = "Proportion"))
  })
  
  # Check how many plots are selected
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  # Combine the plots
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
  
}

# Function to plot endline learned new operation
plot_learn_newop_e <- function(data = dat,
                               selected_rounds = c("R5", "R6", "R7", "R8")){
  
  # Create a list of plots
  plot_list <- map(selected_rounds, function(r) {
    data_subset <- dat %>% filter(round == r)
    
    # Calculate the counts and proportions
    df_counts <- data_subset %>%
      count(round, treatment, learned_new_operation) %>%
      na.omit() |> 
      group_by(treatment) %>%
      mutate(fraction = n / sum(n)) |>
      filter(learned_new_operation == "Yes")
    
    # Plot
    df_counts %>%
      plot_ly(x = ~treatment, 
              y = ~fraction, 
              type = "bar", 
              color = ~treatment, 
              colors = "Greens", 
              text = ~paste0("Round ", unique(df_counts$round), " - ", treatment, ": ", 
                             sprintf("%.1f", 100 * fraction),
                             "% Innumeracy at Endline"), 
              hoverinfo = "text",
              showlegend = F) %>%
      layout(xaxis = list(title = paste0("Treatment (", r, ")")), 
             yaxis = list(title = "Proportion"))
  })
  
  # Check how many plots are selected
  nrows <- ifelse(length(plot_list) <= 2, 1, 2)
  
  # Combine the plots
  p <- do.call(subplot, c(plot_list, 
                          list(nrows = nrows, 
                               titleY = TRUE, 
                               titleX = TRUE, 
                               margin = c(0.1, 0.1, 0.1, 0.1))))
  
  return(p)
  
}