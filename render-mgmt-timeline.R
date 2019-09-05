library(timevis)

# render timeline from management records in a given subexperiment
render_mgmt_timeline <- function(management_data){
  
  if (nrow(management_data) > 0) {
    
    types <- management_data[[ 'mgmttype' ]]
    dates <- management_data[[ 'date' ]]
    
    timeline_data <- data.frame(
      id = 1:nrow(management_data),
      content = types,
      start = dates
    )
    
    timevis(
      timeline_data,
      options = list(zoomable = FALSE)
    )
  }
  
}

# render info box for date, type, and notes of selected (clicked) timeline item
render_timeline_hover <- function(selected_input_id, subexp_data, input){
  
  req(input[[ selected_input_id ]])
  selected <- input[[ selected_input_id ]]
  
  management_data <- subexp_data[[ 'managements' ]]
  selected_record <- management_data[ as.numeric(selected), ]
  formatted_notes <- ''
  if (selected_record[[ 'notes' ]] != '') {
    formatted_notes <- paste0('<br><br>', selected_record[[ 'notes' ]])
  }
  wellPanel(class = 'mgmt-select-info',
            HTML(paste0(
              selected_record[[ 'mgmttype' ]], '<br>',
              selected_record[[ 'date' ]],
              formatted_notes
            ))
  )
}
