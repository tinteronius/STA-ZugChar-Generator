# Contents
# . Helper Functions for File Handling
# . Helepr Functions for 

# install all needed packages if not installed
helper.neededPackages = c("parallel", "doParallel", "foreach", "lpSolveAPI", "ggplot2")
helper.updatePackages = function() {
  needed_packages = helper.neededPackages
  new_packages = needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
  if (length(new_packages) > 0) install.packages(new_packages)
}

# create Folder if not existing
helper.safeCreateFolder = function (path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# Create result directory if necessary
RESULT_PATH = paste0("./result_detail_v", RESULT_VERSION, "/")
if (!dir.exists(RESULT_PATH)) {
  dir.create(RESULT_PATH)
}

helper.getResultPath = function (filename) {
  return (paste0(RESULT_PATH, filename))
}

# Progressbar Functions
#helper.log_socket = helper.getResultPath("log.txt")
helper.log = function(msg) {
  print_message = sprintf(paste0(as.character(Sys.time()), ": ", msg, "\n"))
  cat (print_message)
  
}

# Check if File Exists, Raise Error if not (error_message = additional error message, if file not found)
helper.requireFile = function (path, error_message = "") {
  if (! file.exists(path)) {
    stop(paste0(path, " not found ! - Please set working directory. ", error_message))
  }
}

# if everything works
helper.updatePackages()
library("parallel")
library("doParallel")
library("foreach")
library("lpSolveAPI")
library("ggplot2")

helper.included = T