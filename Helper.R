helper.included = T
# Contents
# . Helper Functions for File Handling

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
library("txtProgressBar")