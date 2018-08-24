# # # # # # # # # # # # # # 
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #
source("a-v-calculations.R")
source("T10kmCalculator.R")

RESULT_VERSION = 12
STA_RESULT_FOLDER = "STAs/"
A_FRAME_RESULT_FOLDER = "a_frame/"

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



sta_resultfile_prefix = paste0(helper.getResultPath(STA_RESULT_FOLDER), "STA_")
a_frame_resultfile_prefix = paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER))

sta = '151'
sta_file = paste0(sta_resultfile_prefix, sta, '.csv')
aFrame = read.csv2(paste0(a_frame_resultfile_prefix, sta, '.csv'))
aMatrix = t(as.matrix(aFrame[,2:ncol(aFrame)]))

library(lpSolveAPI)

lprec = make.lp(nrow = nrow(aMatrix), ncol = ncol(aMatrix) )

for (i in 1:nrow(aMatrix)) {
  
  set.row(lprec, i, aMatrix[i,])
  
}
set.rhs(lprec, rep(1, nrow(aMatrix)))  
set.constr.type(lprec, rep(2, nrow(aMatrix)))

for (i in 1:nrow(aMatrix)) {
  col = rep(0, nrow(aMatrix))
  col[i] = 1
  add.column(lprec, col)
}

for (i in 1:(ncol(aMatrix) + nrow(aMatrix))) {
  set.type(lprec, i, type = "binary")
}

joker_col = c(rep(0, ncol(aMatrix)), rep(1, nrow(aMatrix)))
add.constraint(lprec, joker_col, 1,  floor(nrow(aMatrix) * 0.1))
add.constraint(lprec, rep(1, ncol(aMatrix) + nrow(aMatrix)), 1, 2)

indices = c(colSums(aMatrix), rep(0, nrow(aMatrix)))
set.objfn(lprec, indices)

write.lp(lprec, helper.getResultPath("temp.lp"), type = "lp")

solve(lprec)

which(get.variables(lprec) != 0)

aMatrix[,111] + aMatrix[,2540]

calculate10kmWithI()
