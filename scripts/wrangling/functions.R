make_duration <- function(hhmm_string) {
  parts <- strsplit(hhmm_string, ":")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  total_minutes <- hours * 60 + minutes
  
  structure(
    list(
      original = hhmm_string,
      minutes = total_minutes
    ),
    class = "hhmm_duration"
  )
}
