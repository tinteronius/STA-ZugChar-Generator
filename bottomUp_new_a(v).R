# # # # # # # # # # # # # # 
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #
source("a-v-calculations.R")
source("T10kmCalculator.R")

# STA
staFolder <- helper.getResultPath(STA_RESULT_FOLDER)
helper.safeCreateFolder(staFolder)

files <- list.files(path = staFolder, full.names = T, pattern = ".csv$")
fileNames <- list.files(path = staFolder, full.names = F, pattern = ".csv$")

staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)
staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""

dt <- read.csv2(file = helper.getResultPath(TEMP_TFZ_FRAME_FILEPATH))
dt <- dt[!duplicated(dt[, c("TOTALWEIGHT", "BREAKCLASS", "TFZ", "NUM_TFZ")]),]
dt <- dt[dt$TOTALWEIGHT >= 500,]

dt$T10WithI <- NULL
avList <- list()
for(j in 1:nrow(dt)){
    cat (j, "/", nrow(dt),"\n")
    elem <- tfzNames[tfzNames$name == dt$TFZ[j], ]
    avModel = getAVModel(elem$i, elem$j, dt$TOTALWEIGHT[j], dt$NUM_TFZ[j], addTfzMass = T)
    avList <- c(avList, list(avModel))
    
    dt$T10WithI[j] = calculate10kmWithI(avModel, dt$VMAX[j], dt$BREAKCLASS[j], 7)
    
}

write.csv2(dt, file = helper.getResultPath(TFZ_LIST_FOR_A_FRAME_FILEPATH), row.names = F)

tempFrame <- data.frame()
for(i in 1:length(files)){
  cat (i, "/", length(files),"\n")
  tempFrame <- rbind(tempFrame, read.csv2(file = files[i], stringsAsFactors = F))
}

ds <- tempFrame[!duplicated(tempFrame[,c("TFZ", "TOTALWEIGHT", "NUM_TFZ")]),c("TFZ", "TOTALWEIGHT", "NUM_TFZ", "VMAX")]
#ds <- rbind(dt, data.frame(TRACTION = c("189-2", "185-2", "185-2", "185-2", "203-1"), TOTALMASS = c(2945, 4000, 5000, 5500, 1950), DOUBLETRACTION = F, VMAX = 100, stringsAsFactors = F))


avSTA <- list()
for(j in 1:nrow(ds)){
  cat (j, "/", nrow(ds),"\n")
  elem <- tfzNames[tfzNames$name == ds$TFZ[j], ]
  avSTA <- c(avSTA, list(getAVModel(elem$i, elem$j, ds$TOTALWEIGHT[j], ds$NUM_TFZ[j], addTfzMass = F)))
}

# Prepare Paths
sta_resultfile_prefix = paste0(helper.getResultPath(STA_RESULT_FOLDER), "STA_")
helper.safeCreateFolder(helper.getResultPath(BOTTOMUP_RESULT_FOLDER))
helper.safeCreateFolder(helper.getResultPath(A_FRAME_RESULT_FOLDER))

# Settting up Parallel Computing
cl <- makeCluster(NUMBER_OF_CORES)
registerDoParallel(cl)
list_of_done = logical(nrow(staGroups))

cbind(1:nrow(staGroups), staGroups$ID)[1:20,]

# Execute a_frame / T10 calculation as a parrallized loop
res = foreach(i = 1:nrow(staGroups),
              .export = c("getReduction", "helper.getResultPath", "calculate10kmAcceleration", 
                          "calculate10km", "calculate10kmWithI", "getAVModel", "helper.log" ),
              .packages = c("XML")) %dopar% {
                # Init message
                msg = paste0("Started STA ", staGroups$ID[i], " (", i, "/", nrow(staGroups), ") \n")
                helper.log(msg)
                
                # Load Data
                tempFrame <- read.csv2(file = paste0(sta_resultfile_prefix, staGroups$ID[i], ".csv"), stringsAsFactors = F)
                if(staGroups$PARTNER[i] != ""){
                  fi <- paste0(sta_resultfile_prefix, staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
                  for(f in fi){
                    tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
                  }
                }
                tempFrame$VMAX[is.na(tempFrame$VMAX)] <- min(tempFrame$VMAX, na.rm = T)
                
                
                ninety <- ceiling(0.9*length(tempFrame$TRAINRUN)) 
                lng <- length(tempFrame$X)
                
                v <- unique(sort(tempFrame$VMAX, decreasing = T)[ninety:lng])
                b <- unique(sort(tempFrame$BrH, decreasing = T)[ninety:lng])
                # 1 - class P or better, 0 - class G
                c <- ifelse(unique(sort(tempFrame$BREAKCLASS, decreasing = T)[ninety:lng]) != "G", "P", "G")
                
                
                # Check compatibility tempFrame <-> avList
                # Result is the "a_frame"
                
                a_frame <- data.frame(tr = seq(1,length(tempFrame$X)))
                
                for(j in 1:length(avList)){
                  ind <- max(which(avList[[j]]$a >=0 & !is.na(avList[[j]]$s_kum)))
                  vmax <- min(avList[[j]]$v[ind], dt$VMAX[j])
                  ind <- which(avList[[j]]$v == vmax)
                  
                  # get tolerance of a(v)
                  reduce <- getReduction(avList[[j]]$a[1], avList[[j]]$a[ind])
                  a_tol <- avList[[j]]$a - seq(0.1,1,0.009)* reduce
                  check_a <- logical(length(tempFrame$X))
                  
                  for(k in 1:length(tempFrame$X)){
                    p <- which(tempFrame$TFZ[k] == ds$TFZ & tempFrame$TOTALWEIGHT[k] == ds$TOTALWEIGHT & tempFrame$NUM_TFZ[k] == ds$NUM_TFZ)
                    ind_tf <- max(which(avSTA[[p]]$a >=0 & !is.na(avSTA[[p]]$s_kum)))
                    vm <- min(avSTA[[p]]$v[ind_tf], tempFrame$VMAX[k])
                    ind <- min(ind, which(avSTA[[p]]$v == vm))									  
                    check_a[k] <- sum(avSTA[[p]]$a[1:ind] >= a_tol[1:ind]) == ind
                  }
                  a_frame <- cbind(a_frame, check_a)
                }
                names(a_frame) <- c("tr", seq(1:length(avList)))
                
                write.csv2(t(a_frame)[-1,], file = paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), staGroups$ID[i], ".csv"), row.names = T)
                
                # Check if anyone gets 90%
                x <- apply(a_frame, 2, sum)/length(tempFrame$X)
                x <- x[2:length(x)]
                
                a <- which(x >= 0.9)
                if(length(a)<1){
                  print(paste(i, "no train covers at least 90% in", fileNames[i]))
                  # # # #
                  next  # here we can add combine90s.R when ready
                }
                
                # Prepare data for calculating T10
                a_frame <- a_frame[,c(1,a+1)]
                v_frame <- data.frame(tr = seq(1,length(tempFrame$X)))
                for(k in v){
                  v_frame <- cbind(v_frame, tempFrame$VMAX >= k)
                }
                names(v_frame) <- c("tr", v)
                
                b_frame <- data.frame(tr = seq(1,length(tempFrame$X)))
                for(k in b){
                  b_frame <- cbind(b_frame, tempFrame$BrH >= k)
                }
                names(b_frame) <- c("tr", b)
                
                c_frame <- data.frame(tr = seq(1,length(tempFrame$X)))
                for(k in c){
                  # 1 - class P or better, 0 - class G
                  y <- tempFrame$BREAKCLASS != "G"
                  c_frame <- cbind(c_frame, y >= (k!="G"))
                }
                names(c_frame) <- c("tr", c)
                
                # get all possible combinations
                df <- expand.grid(a = a, b = b, c = c, v = v)
                for(k in 1:length(df$a)){
                  df$a_res[k] <- 1.0 * sum(a_frame[,paste0(df$a[k])]) / length(tempFrame$X)
                  df$b_res[k] <- 1.0 * sum(b_frame[,paste0(df$b[k])]) / length(tempFrame$X)
                  df$c_res[k] <- 1.0 * sum(c_frame[,paste0(df$c[k])]) / length(tempFrame$X)
                  df$v_res[k] <- 1.0 * sum(v_frame[,paste0(df$v[k])]) / length(tempFrame$X)
                  
                  df$total_res[k] <- 1.0 * sum(a_frame[,paste0(df$a[k])] & b_frame[,paste0(df$b[k])] & c_frame[,paste0(df$c[k])] & v_frame[,paste0(df$v[k])]) / length(tempFrame$X)
                  
                }
                
                df <- df[df$total_res >= 0.9,]
                all90 <- cbind(df, dt[df$a,])
                
                # Caching is done by intelligent sorting of calculation parameters
                # -> calculation can be skipped if values do not change
                num = nrow(all90)
                tfz_ij = tfzNames[match(all90$TFZ, tfzNames$name, nomatch = NA), ]
                anfragen = data.frame(a = all90$a, i = tfz_ij$i, j = tfz_ij$j, m = all90$TOTALWEIGHT, anzTfz = all90$NUM_TFZ, v = all90$v, c = all90$c) #, addTfzMass = T)
                order_index = order(anfragen$i, anfragen$j, anfragen$m, anfragen$anzTfz)
                anfragen <- anfragen[order_index,]
                
                # Init Cache
                last_avRequest = 1:4
                last_fullRequest = 1:6
                av_skip_counter = 0 # Counters For Debugging
                t10_skip_counter = 0 # Counters For Debugging
                
                t10 <- integer(num)
                for(n in 1:num){
                  
                  avRequest = anfragen[n,2:5]
                  fullRequest = anfragen[n, 2:7]
                  
                  # check if new avModel is needed
                  if (any(avRequest != last_avRequest)){
                    avModel <- getAVModel(i = avRequest$i, j = avRequest$j, m = avRequest$m, anzTfz = avRequest$anzTfz, addTfzMass = T)
                    T10kmAccel = calculate10kmAcceleration(avModel, 7)
                    last_avRequest <- avRequest
                  } else {
                    # Count For Debugging
                    av_skip_counter = av_skip_counter + 1
                  }
                  
                  # check if new T10 values are needed when VMAX or BrH 
                  if (any(fullRequest != last_fullRequest)) {
                    T10kmWithI = calculate10kmWithI(avModel, fullRequest$v, fullRequest$c, 7)
                    T10km = calculate10km(avModel, fullRequest$v, fullRequest$c)
                    last_fullRequest <- fullRequest
                  } else {
                    # Count For Debugging
                    t10_skip_counter = t10_skip_counter + 1
                  }
                  
                  # T10 - final calculation
                  t10_avg <- 0.5 * T10km  + 
                    0.4 * T10kmWithI + 
                    0.1 * T10kmAccel
                  
                  t10[n] <- t10_avg			  
                }
                # Re-order the Values to match the order of all90 data.frame
                reorder_index = order(order_index)
                all90$T10 <- t10[reorder_index]

                if(sum(t10 < 20000) <= 0){
                  s <- sort(unique(t10))[20]
                  all90 <- all90[all90$T10 <= s,]
                }else{
                  all90 <- all90[all90$T10 < 20000,]
                }			
          
                write.csv2(all90, file = paste0(helper.getResultPath(BOTTOMUP_RESULT_FOLDER), staGroups$ID[i], ".csv"), row.names = F)
                
                # Logging
                msg = paste0("Finished STA ", staGroups$ID[i], " (", i, "/", nrow(staGroups), ") \n")
                helper.log(msg)
  
}
stopCluster(cl) # shut down the cluster

################ STOP HERE AND CONTINUE WITH NEXT FILE #############################################################

if (F) {

files <- list.files(paste0(folder, "all90/"), full.names = T, pattern = ".csv$")
fileNames <- list.files(paste0(folder, "all90/"), full.names = F, pattern = ".csv$")

for(i in 1:length(fileNames)){
  print(fileNames[i])
  tempFrame <- read.csv2(files[i], stringsAsFactors = F)
  for (j in 1:length(tempFrame$a)) {
    elem <- tfzNames[tfzNames$name == tempFrame$TFZ[j], ]
    avModel <- getAVModel(i = elem$i, j = elem$j, m = tempFrame$TOTALWEIGHT[j], anzTfz = tempFrame$NUM_TFZ[j], addTfzMass = T)
    t10_half <- 0.5 * calculate10km(avModel, tempFrame$VMAX[j], tempFrame$BREAKCLASS[j]) + 
                0.4 * calculate10kmWithI(avModel, tempFrame$VMAX[j], tempFrame$BREAKCLASS[j], 7) + 
                0.1 * calculate10kmAcceleration(avModel, 7)
    tempFrame$T10[j] <- t10_half
  }
  write.csv2(tempFrame, file = files[i], row.names = F)
}

for(i in 1:length(fileNames)){
  tempFrame <- read.csv2(files[i], stringsAsFactors = F)
  if(min(tempFrame$T10) > 25000){print(paste(i, fileNames[i], ": no feasible train"))}
  print(paste(fileNames[i], "MinT10", round(min(tempFrame$T10), 1), 
              "5%Quantil", round(quantile(x = tempFrame$T10, probs = 0.05), 1),
              "Anz < 1.1*Min", sum(tempFrame$T10 <= 1.05*min(tempFrame$T10))))
}

############################ STOP #################################################################

fi <- list.files(path = "./result_detail_v5/all90/", full.names = T, pattern = ".csv$")
for(i in 1:length(fi)){
    print(i)
    tempFrame <- read.csv2(file = fi[i], stringsAsFactors = F)
    tempFrame$T10 <- 0
    for(j in 1:length(tempFrame$a)){
        elem <- tfzNames[tfzNames$name == tempFrame$TFZ[j], ]
        avModel <- getAVModel(elem$i, elem$j, tempFrame$TOTALWEIGHT[j], tempFrame$NUM_TFZ[j], addTfzMass = F)
        tempFrame$T10[j] <- calculate10km(avModel = avModel,vmax = tempFrame$v[j], breakclass = tempFrame$c[j])
    }
    write.csv2(tempFrame, file = fi[i], row.names = F)
}




files <- list.files(path = "./result_detail_v4/borders_a(v)/", full.names = T, pattern = ".csv$")
fileNames <- list.files(path = "./result_detail_v4/borders_a(v)/", full.names = F, pattern = ".csv$")
staNumbers <- gsub(".csv", "", fileNames)

lng <- length(staNumbers)

completeFrame <- data.frame(tfz = integer(lng), totalmass = integer(lng), num_tfz = integer(lng),
                            vmax = integer(lng), brh = integer(lng), breakclass = integer(lng), t10 = integer(lng),
                            total_res = integer(lng), sta = integer(lng), stringsAsFactors = F)

for(i in 1:length(files)){
    print(i)
    tempFrame <- read.csv2(file = files[i], stringsAsFactors = F) 
    completeFrame$tfz[i] <- tempFrame$TFZ[tempFrame$select_t10 == 1]
    completeFrame$totalmass[i] <- 1.0 * tempFrame$TOTALWEIGHT[tempFrame$select_t10 == 1] / tempFrame$NUM_TFZ[tempFrame$select_t10 == 1]
    
    completeFrame$num_tfz[i] <- 1
    completeFrame$vmax[i] <- min(tempFrame$v[tempFrame$select_t10 == 1], tempFrame$VMAX[tempFrame$select_t10 == 1])
    completeFrame$brh[i] <- tempFrame$b[tempFrame$select_t10 == 1]
    completeFrame$breakclass[i] <- tempFrame$c[tempFrame$select_t10 == 1]
    completeFrame$total_res[i] <- round(tempFrame$total_res[tempFrame$select_t10 == 1], 4)
    completeFrame$sta[i] <- staNumbers[i]
    completeFrame$t10[i] <- tempFrame$T10[tempFrame$select_t10 == 1]
}


write.csv2(completeFrame, file = paste0("./bottomup/merge_a(v)_v5/AllSelections.csv"))

completeFrame <- read.csv2(file = paste0("./bottomup/merge_a(v)/AllSelections.csv"), stringsAsFactors = F)
completeFrame$actual_vmax <- 0

for(i in 1:length(completeFrame$X)){
    print(i)
    elem <- tfzNames[tfzNames$name == completeFrame$tfz[i], ]
    tempAv <- getAVModel(elem$i, elem$j, completeFrame$totalmass[i], 1)
    
    ind <- max(which(tempAv$a >=0 & !is.na(tempAv$s_kum)))
    completeFrame$actual_vmax[i] <- min(tempAv$v[ind], completeFrame$vmax[i])
}

completeFrame$round_vmax <- 10* round(1.0 * (completeFrame$actual_vmax-1) / 10, 0)

write.csv2(completeFrame, file = paste0("./bottomup/merge_a(v)/AllSelections_v.csv"), row.names = F)
completeFrame <- read.csv2(file = paste0("./bottomup/merge_a(v)/AllSelections_v.csv"), stringsAsFactors = F)



#########################################################################


for(v in seq(30,100,10)){
    tempFrame <- completeFrame[completeFrame$vmax == v,]
    br <- unique(tempFrame[,c("tfz", "totalmass")])
    #br <- rbind(br, data.frame(tfz = "203-1",totalmass = 2770, stringsAsFactors = F))
    
    avList <- list()
    for(j in 1:length(br$tfz)){
        elem <- tfzNames[tfzNames$name == br$tfz[j], ]
        avList <- c(avList, list(getAVModel(elem$i, elem$j, br$totalmass[j], 1)))
    }
    
    a_frame <- data.frame(tr = seq(1,length(br$tfz)))
    
    for(j in 1:length(avList)){
        ind <- max(which(avList[[j]]$a >=0 & !is.na(avList[[j]]$s_kum)))
        vmax <- min(avList[[j]]$v[ind], v)
        ind <- which(avList[[j]]$v == vmax)
        
        # get tolerance of a(v)
        reduce <- getReduction(avList[[j]]$a[1], avList[[j]]$a[ind])
        a_tol <- avList[[j]]$a - seq(0.1,1,0.009)* reduce
        check_a <- logical(length(br$tfz))
        
        for(k in 1:length(br$tfz)){
            check_a[k] <- sum(avList[[k]]$a[1:ind] >= a_tol[1:ind]) == ind
        }
        a_frame <- cbind(a_frame, check_a)
    }
    names(a_frame) <- c("tr", seq(1:length(avList)))
    
    x <- apply(a_frame, 2, sum)/length(br$tfz)
    x <- x[2:length(x)]
    
    a_frame[,1] | a_frame[,3] | a_frame[,6]
    
    min(tempFrame$brh[!a_frame[,6]|!a_frame[,15]])
    min(tempFrame$brh[!a_frame[,6]])
    min(tempFrame$brh[!a_frame[,15]])
    min(tempFrame$brh)
    
    y <- list()
    
    for(m in 2:32){
        for(n in 3:33){
            for(o in 4:34){
                y <- c(y, list(c(m,n,o)))
            }
        }
    }
    
    s <- list()
    
    for(m in 1:length(y)){
        t <- a_frame[,y[[m]]]
        r <- apply(t, 1, sum)
        if(sum(r>0)< length(r)){next}
        s <- c(s, list(c(m, sum(r<2))))
    }
    
    
}

################################################################################

standards <- read.csv2("./bottomup/merge_a(v)/Systematisierung2013_v02.csv", stringsAsFactors = F)
standards$count <- 0
decision <- read.csv2("./bottomup/merge_a(v)/SYSLIST_2013_v02.csv", stringsAsFactors = F)

for(i in 1:length(decision$tfz)){
    sel <- decision$tfz[i] == standards$tfz & decision$totalmass[i] == standards$totalmass & 
        decision$brh[i] == standards$brh & decision$breakclass[i] == standards$breakclass & decision$vmax[i] == standards$vmax
    if(sum(sel) > 1){print(paste("ERROR", i))}
    standards$count[sel] <- 1+ standards$count[sel]
}

##################################################################################

library(ggplot2)

avMaster <- data.frame(name = c("182-1", "185-2", "189-2", "185-2"), masse = c(1620, 1400, 1590, 1600), double = F)
avMaster <- data.frame(name = dt$TRACTION[c(19,26,27)], masse = dt$TOTALMASS[c(19,26,27)], double = F)
avMaster <- data.frame(name = br$tfz, masse = br$totalmass, double = F)
avFrame <- data.frame()

for(l in 1:length(avMaster$name)){
    elem <- tfzNames[tfzNames$name == avMaster$name[l], ]
    avFrame <- rbind(avFrame, getAVModel(elem$i, elem$j, avMaster$masse[l], 1+avMaster$double[l]))
}

flex <- avFrame[304:404,]
flex$a <- flex$a - seq(0.1,1,0.009)*0.02401332
flex$tfz <- "flex"
avFrame <- rbind(avFrame, flex)


e <- qplot(x = avFrame$v, y = avFrame$a, color = avFrame$tfz, alpha = 0.7) + theme_light() + 
    scale_color_manual(values = c("#e41a1c", "#ff7f00","#984ea3", "blue", "brown")) + scale_x_continuous(breaks = seq(0,100,20))
e <- qplot(x = avFrame$v, y = avFrame$a, color = avFrame$tfz, alpha = 0.7) + theme_light() 
ggsave(filename=paste0("./bottomup/merge_a(v)/", "example_paper.png"), plot=e, width = 15, height = 9)

write.csv2(avFrame[102:202,], file= "a-v-Diagram-185.2_1400t.csv")




########################################################

library(lpSolveAPI)

lprec <- make.lp(0, 4)
set.objfn(lprec, c(1, 3, 6.24, 0.1))
add.constraint(lprec, c(0, 78.26, 0, 2.9), ">=", 92.3)
add.constraint(lprec, c(0.24, 0, 11.31, 0), "<=", 14.8)
add.constraint(lprec, c(12.68, 0, 0.08, 0.9), ">=", 4)
set.bounds(lprec, lower = c(28.6, 18), columns = c(1, 4))
set.bounds(lprec, upper = 48.98, columns = 4)
RowNames <- c("THISROW", "THATROW", "LASTROW")
ColNames <- c("COLONE", "COLTWO", "COLTHREE", "COLFOUR")
dimnames(lprec) <- list(RowNames, ColNames)
lprec

solve(lprec)
get.objective(lprec)
get.variables(lprec)

} # if False