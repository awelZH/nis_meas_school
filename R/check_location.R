get_location <- function() {
  system_name <- Sys.info()[1]

  if (system_name == "Linux") {
    location <- "/home/file-server/"
  } else if (system_name == "Windows") {
    location <- "L:/STAT/"
  }


  return(location)
}
