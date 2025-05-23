## Quality control: eventRemarks columns

quality_control <- function(dt, col) {
  year_pattern <- "^\\d{4}$"
  dt[grepl(year_pattern, get(col)), eventRemarks := "high confidence"]
  return(dt)
}