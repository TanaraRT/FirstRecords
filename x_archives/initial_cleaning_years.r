##
## Initial cleaning of the years data
##

initial_cleaning_years <- function(dt, col) {
  # Basic cleaning
  dt[, (col) := {
    x <- get(col)
    x <- gsub(",", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    x <- tolower(x)
    x
  }]

  # Word to number replacement
  word_to_number <- c(
    "half a" = "0.5", "one" = "1", "two" = "2", "three" = "3",
    "four" = "4", "five" = "5", "six" = "6", "seven" = "7",
    "eight" = "8", "nine" = "9", "ten" = "10", "eleven" = "11",
    "twelve" = "12", "thirteen" = "13", "fourteen" = "14",
    "fifteen" = "15", "sixteen" = "16", "seventeen" = "17",
    "eighteen" = "18", "nineteen" = "19", "twenty" = "20"
  )

  for (word in names(word_to_number)) {
    pattern <- paste0("\\b", gsub(" ", "\\\\s+", word), "\\b")
    dt[, (col) := gsub(pattern, word_to_number[[word]],
                       get(col), ignore.case = TRUE)]
  }

  return(dt)
}
