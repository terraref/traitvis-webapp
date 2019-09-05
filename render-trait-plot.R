library(ggplot2)


# render box plot time series from trait records in a given subexperiment, for the selected variable
# if a cultivar is selected, render line plot from trait records for that cultivar
render_trait_plot <- function(plot_data, selected_variable, selected_cultivar, units, start_date, end_date){
  data_max <- max(plot_data[[ 'mean' ]])
  unique_vals <- unique(plot_data[[ 'mean' ]])
  all_vals_integers <- all(unique_vals %% 1 == 0)
  num_unique_vals <- length(unique_vals)
  
  plot_data$method[is.na(plot_data$method) | plot_data$method == 'NA'] <- 'Not Provided'
  
  trait_plot <- ggplot(data = plot_data, aes(x = as.Date(date), y = mean, color = method)) +
    scale_colour_brewer(palette = "Set1")
  
  {
    if ((num_unique_vals < 20) & (max(unique_vals) < 30) & all_vals_integers) {
      trait_plot <- trait_plot + 
        geom_count() + 
        geom_vline(aes(xintercept = as.numeric(as.Date(date))),
                   color = 'grey')
    } else {
      trait_plot <- trait_plot + 
        geom_violin(scale = 'width', width = 1, aes(group = as.Date(date))) +
        geom_boxplot(outlier.alpha = 0.25, width = 0.2, aes(group = as.Date(date)))
    }
    }
  
  if (selected_cultivar != 'None') {
    title <- paste0(selected_variable, '\nCultivar ', selected_cultivar, ' in red')
    trait_plot <- trait_plot + 
      geom_point(data = subset(plot_data, cultivar_name == selected_cultivar), 
                 aes(x = as.Date(date), y = mean, group = site_id)) +
      geom_line(data = subset(plot_data, cultivar_name == selected_cultivar), 
                size = 0.5, alpha = 0.5, aes(x = as.Date(date), y = mean, group = site_id)) 
  } else {
    title <- selected_variable
  }
  
  trait_plot + 
    labs(
      title = paste0(title, '\n'),
      x = "Date",
      y = units
    ) +
    theme_bw() + 
    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom') +
    xlim(start_date, end_date) +
    ylim(0, data_max)
  
}

# render info box for date and value of cursor when hovering box/line plot
render_plot_hover <- function(hover_input_id, input){
  
  req(input[[ hover_input_id ]])
  
  hover <- input[[ hover_input_id ]]
  
  wellPanel(class = 'plot-hover-info push-down',
            HTML(paste0(
              'Date', '<br>',
              toString(
                as.Date(hover$x, origin = lubridate::origin)
              ),
              '<br><br>',
              'Value', '<br>',
              format(round(hover$y, 2))
            ))
  )
  
}
