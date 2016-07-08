# This script should be run in a shell in context of the .RProfile...

if ("--version" %in% commandArgs(TRUE)) {
  cat("v0.0.0\n")
} else {
  
  # Load the Rserve library:
  library("Rserve")

  # Start Rserve
  run.Rserve(debug = TRUE, 6311, args = NULL, config.file = "Rserv.conf")
}
