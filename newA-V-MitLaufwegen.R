# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

source("a-v-calculations.R")
source("T10kmCalculator.R")

# Contents
# . Preparing STAs and Fahrlagen for further calculations


# # # # # # # # # # # # # #
# Maping STA and BTS

if (DO_MAPPING_STA_BTS) { # Checking Block if Mapping enabled

files <- list.files(STA_FOLDER, full.names = T, pattern = ".csv$")
allSta <- unlist(lapply(strsplit(files, "_"), function(x){x[5]}))
staNumber <- sort(unique(allSta))


# Map which BTS belong to a STA
sta <- data.frame(ID <- integer(0), BTS = integer(0), stringsAsFactors = F)
mapping <- read.csv2(file = STA_MAPPING_FILE, stringsAsFactors = F)

for(s in staNumber){
    print(s)
    ind <- which(s == allSta)
    if(length(ind) > 2){stop("more than 2 STA!")}

    f <- files[ind]
    bts <- integer(0)
    for(i in 1:length(f)){
        bts <- unique(c(bts, read.csv2(f[i], stringsAsFactors = F)$RIL100))
    }
    bts <- bts[bts != "Path Cost " & bts != "Total Priority " & bts != "Total Priority/No.Of Fahrwege "]
    # take only bts with no numbers within
    #bts <- bts[!grepl(".*\\d+.*", bts)]
    old <- mapping$alt[mapping$neu == s]
    sta <- rbind(sta, data.frame(NEW_ID = s, ID = unlist(strsplit(old, "_"))[1],
                                 OLD_STA = old, BTS = bts, stringsAsFactors = F))
}

# Manual Refactorings
id <- c("001", "005", "9A", "173C")
new_id <- c("201", "205", "203", "631")
old_sta <- c("1", "005", "9A", "173C")
bts <- c("AWLB", "AWLB", "AWLB", "RSG")
sta <- rbind(sta, data.frame(NEW_ID = new_id, ID = id, OLD_STA = old_sta, BTS = bts, stringsAsFactors = F))


write.csv2(sta, file = helper.getResultPath(BTS2STA_FILEPATH), row.names = F)

} else { # Checking Block if Mapping enabled

  sta <- read.csv2(file = helper.getResultPath(BTS2STA_FILEPATH), stringsAsFactors = F)

}

# # # # # # # # # # # # # #
# DEBUGGING?

#for(w in finveBTS$FinVe.BTS){
#    if(w %in% sta$BTS){next()}
#    print(w)
#}


# # # # # # # # # # # # # #
# calculate if two STA overlap
# . Known Issue: Error when multiple groups found -> merge into new big group??

if (DO_OVERLAPPING) { # Checking Block if Overlapping enabled
overlappings <- data.frame(STA = staNumber, PARTNERS = integer(length(staNumber)),
                           BIG_OVERLAP = integer(length(staNumber)),
                           RATIO_OVERLAP = integer(length(staNumber)),
                           stringsAsFactors = F)


for(s in 1:(length(staNumber)-1)){
    print(s)
    partners <- integer(0)
    overlap <- integer(0)
    ratio <- integer(0)
    for(k in (s+1):length(staNumber)){
        tempBTS <- sta$BTS[sta$ID == staNumber[s]]
        len <- length(tempBTS)
        first <- tempBTS[1]
        last <- tempBTS[len]
        count <- 0
        for(b in tempBTS){
            count <- count + (b %in% sta$BTS[sta$ID == staNumber[k]])
        }

        threshold <- max(floor(0.1*len), 5)
        if(count >= threshold){
            #print(paste(s, k))
            partners <- c(partners, staNumber[k])
            minlen <- min(len, length(sta$BTS[sta$ID == staNumber[k]]))
            maxlen <- max(len, length(sta$BTS[sta$ID == staNumber[k]]))
            crit1 <- (1.0* count/maxlen) > 0.44
            # crit2 <- (1.0* count/minlen) > 0.66
            if(crit1){
                overlap <- c(overlap, staNumber[k])
                ratio <- c(ratio, paste(round(1.0* count/maxlen, 2)))#, round(1.0* count/minlen, 2), sep="+"))
            }
        }
    }

    overlappings$PARTNERS[s] <- paste(partners, collapse = "#")
    overlappings$BIG_OVERLAP[s] <- paste(overlap, collapse = "#")
    overlappings$RATIO_OVERLAP[s] <- paste(ratio, collapse = "#")
    if(s == (length(staNumber)-1)){
        overlappings$PARTNERS[s+1] <- ""
        overlappings$BIG_OVERLAP[s+1] <- ""
        overlappings$RATIO_OVERLAP[s+1] <- ""
    }
}
staNumber <- sort(unique(sta$ID))

staGroups <- data.frame(ID = overlappings$STA, GROUP = rep(NA, length(staNumber)),
                        PARTNER = rep(NA, length(staNumber)), stringsAsFactors = F)
id <- 1
partner <- 1000

for(i in 1:length(overlappings$STA)){

    tempID <- staGroups$GROUP[i]
    tempPartner <- staGroups$PARTNER[i]
    if(is.na(tempID)){
        tempID <- id
        id <- id + 1
    }

    if(is.na(tempPartner) & overlappings$BIG_OVERLAP[i] != ""){
        tempPartner <- partner
        partner = partner + 1
    }
    staGroups$GROUP[i] <- tempID
    staGroups$PARTNER[i] <- tempPartner
    succ <- unlist(strsplit(overlappings$PARTNERS[i], "#"))

    succ_group <- staGroups[which(staGroups$ID %in% succ),"GROUP"]
    if(any(!is.na(succ_group))){
        print(paste(i, succ[!is.na(succ_group)], "is already in a group!"))
        gr <- unique(succ_group[!is.na(succ_group)])
        if(length(gr) > 1){stop(i, " ", gr, " multiple groups!")}
        staGroups[which(staGroups$ID %in% succ),"GROUP"][is.na(succ_group)] <- gr
        staGroups$GROUP[i] <- gr
    }else{
        staGroups[which(staGroups$ID %in% succ),"GROUP"] <- tempID
        }
    succ_p <- unlist(strsplit(overlappings$BIG_OVERLAP[i], "#"))
    succ_p_group <- staGroups[which(staGroups$ID %in% succ_p),"PARTNER"]
    if(any(!is.na(succ_p_group))){
        print(paste(i, succ_p[!is.na(succ_p_group)], "has already a partner!"))
        pa <- unique(succ_p_group[!is.na(succ_p_group)])
        if(length(pa) > 1){stop(i, " ", gr, " multiple partners!")}
        staGroups[which(staGroups$ID %in% succ_p),"PARTNER"][is.na(succ_p_group)] <- pa
        staGroups$PARTNER[i] <- pa
    }else{
        staGroups[which(staGroups$ID %in% succ_p),"PARTNER"] <- tempPartner
    }
}

staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""

# DEBUGGING?
for(i in 1:82){
    if(length(staGroups$ID[staGroups$GROUP == i])==0){next()}
    print(paste(i, paste(staGroups$ID[staGroups$GROUP == i], collapse = "#")))
}

for(i in 1000:1015){
    print(paste(i, paste(staGroups$ID[staGroups$PARTNER == i][!is.na(staGroups$ID[staGroups$PARTNER == i])], collapse = "#")))
}

write.csv2(staGroups, file = helper.getResultPath(STAGROUPS_FILEPATH), row.names = F)

} else { # Checking Block if Overlapping enabled
  helper.requireFile(helper.getResultPath(STAGROUPS_FILEPATH), "Try Copy from Input Folder.")
  staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)

}

# # # # # # # # # # # # # #
# Meassure share (STAFIT) Fahrlage on STA

if (DO_STAFIT) { # Checking Block if STAFIT enabled

# Load data
data <- read.csv2(file = FAHRLAGEN_FILEPATH, stringsAsFactors = F)
finveBTS <- read.csv2(file = FINVEBTS_FILEPATH, stringsAsFactors = F)

wayPoints <- strsplit(data$WAY, "#")

data$STA_FIT <- integer(nrow(data))
grp <- sort(unique(staGroups$GROUP))

for(i in 1:nrow(data)){
    cat (i, " / ", nrow(data),"\n")
    if(length(wayPoints[[i]]) < 5){next()}
    for(j in grp){
        staNames <- staGroups$ID[staGroups$GROUP == j]
        fit <- double(length(staNames))
        for(k in 1:length(staNames)){
            tBTS <- sta$BTS[sta$ID == staNames[k]]
            fit[k] <- 1.0 * sum(wayPoints[[i]] %in% tBTS) / length(tBTS)
            #if(wayPoints[[i]][1] %in% tBTS){fit[k] <- fit[k] + 0.2}
            #if(wayPoints[[i]][length(wayPoints[[i]])] %in% tBTS){fit[k] <- fit[k] + 0.2}
        }
        id <- which.max(fit)
        if(length(id) == 0 | fit[id] < 0.25){next()}
        data$STA_FIT[i] <- ifelse(data$STA_FIT[i] == 0, staNames[id], paste(data$STA_FIT[i], staNames[id], sep = "#"))
    }
}

sum(data$STA_FIT != 0)
write.csv2(data, file = helper.getResultPath(FAHRLAGEN_STAFIT_FILEPATH), row.names = F)

} else { # Checking Block if Overlapping enabled

  data <- read.csv2(file = helper.getResultPath(FAHRLAGEN_STAFIT_FILEPATH), stringsAsFactors = F)

}

# # # # # # # # # # # # # #
# Map Fahrlagen to STA (...groups)

helper.safeCreateFolder(helper.getResultPath(STA_RESULT_FOLDER))
sta_resultfile_prefix = paste0(helper.getResultPath(STA_RESULT_FOLDER), "STA_")

# Exclude Wolken-BTS from STA trainpaths
wolken <- read.csv2(file = WOLKEN_FILEPATH, stringsAsFactors = F)
sta = sta[!(sta$BTS %in% wolken$BSTID) ,]

staList <- strsplit(data$STA_FIT, "#")
staNumber <- sort(unique(sta$ID))

for(i in 1:length(staNumber)){
    ind <- which(unlist(lapply(staList, function(x) staNumber[i] %in% x)))
    if(length(ind) < 1){
        print(paste(i , "Kein Zug auf STA", staNumber[i],"!"))
        df <- data.frame(TRAINRUN = integer(0), TFZ = integer(0), NUM_TFZ = integer(0),
                         VMAX= integer(0), TOTALLENGTH = integer(0),
                         TOTALWEIGHT = integer(0), BREAKCLASS = integer(0),
                         BrH = integer(0), LZB = integer(0), ELECTRIC = integer(0),
                         TRAINCLASS = integer(0), stringsAsFactors = F)
        write.csv2(df, file = paste0(sta_resultfile_prefix, staNumber[i], ".csv"))
        next()
        }

    trainrun <- data$ANFORDERUNGNAME[ind]



    vmax <- as.integer(data$VMAX[ind])
    brh <- as.integer(data$BrH[ind])
    tfzg <- data$TFZ[ind]
    totalLength <- as.integer(data$TOTALLENGTH[ind])
    totalMass <- as.integer(data$TOTALWEIGHT[ind])
    breakclass <- data$BREAKCLASS[ind]
    num_tfz <- data$NUM_TFZ[ind]
    lzb <- as.logical(data$LZB[ind])
    trainclass <- data$TRAINCLASS[ind]

    currentBTSinSTA <- sta$BTS[sta$ID == staNumber[i]]
    for(j in 1:length(trainrun)){
        changes <- unlist(strsplit(data$CHARARCTERISTIC_CHANGE[ind[j]], "#"))
        if(length(changes) < 1){next()}
        for(m in 1:length(changes)){
            chg <- unlist(strsplit(changes[m], ":"))
            ### get index position of tfzchange and first BTS of STA
            trainPath <- unlist(strsplit(data$WAY[ind[j]], "#"))

            idChange <- min(which(trainPath %in% chg[1]))
            idSTA <- min(unlist(lapply(lapply(currentBTSinSTA, function(x) trainPath %in% x), which)))

            if(is.infinite(idChange) | is.infinite(idSTA)){
                x <- unlist(strsplit(data$WAY[ind[j]], "#"))
                x[which(x == paste0(chg[1],chg[1]))] <- chg[1]
                data$WAY[ind[j]] <- paste(x, collapse = "#")

                trainPath <- unlist(strsplit(data$WAY[ind[j]], "#"))
                idChange <- min(which(trainPath %in% chg[1]))

                if(is.infinite(idChange) | is.infinite(idSTA)){stop("BTS in trainPath not found")}

            }

            ### if tfzChange is on or after first BTS of STA then ignore it
            if (idChange > idSTA){ # replaced '>=' with '>' to match changes on the first BTS
                print(paste("change on or after STA --> ignore it:", trainrun[j]))
                next()
            }
            ### if tfzChange is before first BTS of STA then change
            ### replace the characteristic
            print(paste("change before STA --> replace:", trainrun[j], chg[2]))
            diffChar <- unlist(strsplit(chg[2], "\\$"))
            for(k in 1:length(diffChar)){
                val <- unlist(strsplit(diffChar[k], " "))
                if(val[1] == "TFZ"){
                    if(length(val) > 2){
                        val[2] <- paste(val[2:length(val)], collapse = " ")
                    }
                    if(val[2] == "28. Jan"){val[2] <- "28-1"}
                    tfzg[j] <- val[2]
                    next()
                }
                if(val[1] == "TOTALLENGTH"){
                    totalLength[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "TOTALWEIGHT"){
                    totalMass[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "LZB"){
                    lzb[j] <- as.logical(val[2])
                    next()
                }
                if(val[1] == "BrH"){
                    brh[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "VMAX"){
                    vmax[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "NUM_TFZ"){
                    num_tfz[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "BREAKCLASS"){
                    breakclass[j] <- val[2]
                    next()
                }
            }
        }
    }

    df <- data.frame(TRAINRUN = trainrun, TFZ = tfzg, NUM_TFZ = num_tfz,
                     VMAX= vmax, TOTALLENGTH = totalLength,
                     TOTALWEIGHT = totalMass, BREAKCLASS = breakclass,
                     BrH = brh, LZB = lzb, ELECTRIC = integer(length(lzb)),
                     TRAINCLASS = trainclass, stringsAsFactors = F)

    for(p in 1:length(df$TRAINRUN)){
        #print(p)
        elem <- tfzNames[tfzNames$name == df$TFZ[p], ]
        #avModel <- getAVModel(elem$i, elem$j, df$TOTALMASS[p], 1+df$DOUBLETRACTION[p])
        #avModel$tfz <- df$TRACTION[p]
        #avModel$mWagenzug <- df$TOTALMASS[p]
        #avModel$doubleTraction <- (1+df$DOUBLETRACTION[p])>1
        # write.csv2(avModel, file = paste0("./a-v/", tempBTS,"-",df$TRACTION[p], ".csv"))
        #df$T_100[p] <- ifelse(is.na(avModel$t_kum[101]), 999999, avModel$t_kum[101])
        #df$T_90[p] <- ifelse(is.na(avModel$t_kum[91]), 999999, avModel$t_kum[91])
        #df$T_80[p] <- ifelse(is.na(avModel$t_kum[81]), 999999, avModel$t_kum[81])
        #df$T_70[p] <- ifelse(is.na(avModel$t_kum[71]), 999999, avModel$t_kum[71])
        #df$T_60[p] <- ifelse(is.na(avModel$t_kum[61]), 999999, avModel$t_kum[61])
        #df$T_50[p] <- ifelse(is.na(avModel$t_kum[51]), 999999, avModel$t_kum[51])
        #df$T_40[p] <- ifelse(is.na(avModel$t_kum[41]), 999999, avModel$t_kum[41])
        #df$T_30[p] <- ifelse(is.na(avModel$t_kum[31]), 999999, avModel$t_kum[31])
        #df$T_20[p] <- ifelse(is.na(avModel$t_kum[21]), 999999, avModel$t_kum[21])
        #df$T_10[p] <- ifelse(is.na(avModel$t_kum[11]), 999999, avModel$t_kum[11])
        df$ELECTRIC[p] <- as.integer(xmlGetAttr(tfz[[elem$i]][["Triebfahrzeugbaureihenvarianten"]][[elem$j]][["Stromartausruestungen"]][["Stromartausruestung"]][["Stromart"]],
                                                "Schluessel", "0")) != 0
    }

    write.csv2(df, file = paste0(sta_resultfile_prefix, staNumber[i], ".csv"))

    print(paste("STA", staNumber[i], length(df$TRAINRUN)))
}



################ STOP HERE AND CONTINUE WITH NEXT FILE #############################################################

if (F) {

staAssignment <- data.frame(STA = integer(length(staNumber)), BTSn = integer(length(staNumber)), stringsAsFactors = F)

for(i in 1:length(staNumber)){
    staAssignment$STA[i] <- tempSTA <- staNumber[i]
    staBTS <- sta$BSTID[sta$SYSTEMTRASSENANFORDERUNGID == tempSTA]
    a <- integer(0)
    for(j in 1:length(finveBTS$FinVe.BTS)){
        if(finveBTS$FinVe.BTS[j] %in% staBTS){a <- c(a,j)}
    }
    staAssignment$BTSn[i] <- paste(finveBTS$FinVe.BTS[a], collapse = "#")
}

finVeAssignment <- data.frame(BTS = integer(length(finveBTS$FinVe.BTS)), STAs = integer(length(finveBTS$FinVe.BTS)),
                              NUM_STA = integer(length(finveBTS$FinVe.BTS)), OTHER_BTS = integer(length(finveBTS$FinVe.BTS)), stringsAsFactors = F)

for(i in 1:length(finveBTS$FinVe.BTS)){
    finVeAssignment$BTS[i] <- tempBTS <- finveBTS$FinVe.BTS[i]
    tempSTAs <- sta$SYSTEMTRASSENANFORDERUNGID[which(tempBTS == sta$BSTID)]
    finVeAssignment$STAs[i] <- paste(tempSTAs, collapse = "#")
    finVeAssignment$NUM_STA[i] <- length(tempSTAs)
    otherBTS <- unique(unlist(strsplit(staAssignment[tempSTAs,]$BTSn, "#")))
    finVeAssignment$OTHER_BTS[i] <- paste(otherBTS[otherBTS != tempBTS], collapse = "#")
}

write.csv2(finVeAssignment, file = paste0("./result_detail_v3/", "FinVe-BTS_Assignment_v01", ".csv"), row.names = F)

tempFinVe <- finVeAssignment[with(finVeAssignment, order(-NUM_STA)), ]
uniqueBTS <- integer(0)
coveredSTA <- integer(0)
for(i in 1:length(tempFinVe$BTS)){
    tempSTAs <- unlist(strsplit(tempFinVe$STAs[i], "#"))
    if(all(tempSTAs %in% coveredSTA)){next()}
    uniqueBTS <- unique(c(uniqueBTS, tempFinVe$BTS[i]))
    coveredSTA <- unique(c(coveredSTA, tempSTAs))
}


for(i in 1:length(staAssignment$STA)){
    tempSTA <- staAssignment$STA[i]
    staFinVeBTS <- unlist(strsplit(staAssignment$BTSn[i], "#"))
    a <- integer(0)
    for(b in staFinVeBTS){
        a <- c(a, which(finVeAssignment$BTS == b))
    }
    a <- unique(a)
    otherBTS <- unique(c(unlist(strsplit(finVeAssignment$OTHER_BTS[a], "#")), staFinVeBTS))


    a <- integer(0)
    for(j in 1:length(otherBTS)){
        a <- c(a, which(otherBTS[j] == finVeAssignment$BTS))
    }
    a <- unique(a)

    otherSTA <- unique(unlist(strsplit(finVeAssignment$STAs[a], "#")))
    otherSTA <- otherSTA[otherSTA != tempSTA]
    staAssignment$OTHER_STA[i] <- paste(otherSTA, collapse = "#")


    staBTS <- sta$BSTID[sta$SYSTEMTRASSENANFORDERUNGID == tempSTA]
    a <- integer(0)
    for(j in 1:length(uniqueBTS)){
        if(uniqueBTS[j] %in% staBTS){a <- c(a,j)}
    }
    staAssignment$UNIQUE_BTSn[i] <- paste(uniqueBTS[a], collapse = "#")

}

write.csv2(staAssignment, file = paste0("./result_detail_v3/", "STA_Assignment_v01", ".csv"), row.names = F)

################### Statistics #######################
t90 <- df$T_90[df$T_90>0]
t80 <- df$T_80[df$T_80>0]

print(paste("Anz Tfz, die 80 km/h nicht erreichen:", sum(df$T_80<0), "von", length(df$T_80)))
print(paste("Anz Tfz, die 90 km/h nicht erreichen:", sum(df$T_90<0), "von", length(df$T_90)))

print(paste("Anz Tfz, die bis 80 km/h mehr als 1000 Sec ben?tigen:", sum(df$T_80>1000), "von", length(t80)))
quantile(t80[t80<1000], probs = c(seq(0,1,0.1)))



sum(t80>43 & t80<=72)
sum(t80>72 & t80<=118)
sum(t80>118 & t80<=161)
sum(t80>161 & t80<=185)
sum(t80>185 & t80<=225)
sum(t80>225 & t80<=272)
sum(t80>272 & t80<=380)
sum(t80>380 & t80<=1000)

df[df$T_80 >224 & df$T_80 < 226, ]

# Gruppe 1
sum(df$T_80 >0 & df$T_80 <= 43)
quantile(df$VMAX[df$T_80 >0 & df$T_80 <= 43], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >0 & df$T_80 <= 43], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >0 & df$T_80 <= 43])
quantile(df$TOTALLENGTH[df$T_80 >0 & df$T_80 <= 43], probs = c(seq(0.85,1,0.01)))

# Gruppe 2
sum(df$T_80 >43 & df$T_80 <= 72)
quantile(df$VMAX[df$T_80 >43 & df$T_80 <= 72], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >43 & df$T_80 <= 72], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >43 & df$T_80 <= 72])
quantile(df$TOTALLENGTH[df$T_80 >43 & df$T_80 <= 72], probs = c(seq(0.85,1,0.01)))

# Gruppe 3
sum(df$T_80 >72 & df$T_80 <= 118)
quantile(df$VMAX[df$T_80 >72 & df$T_80 <= 118], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >72 & df$T_80 <= 118], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >72 & df$T_80 <= 118])
quantile(df$TOTALLENGTH[df$T_80 >72 & df$T_80 <= 118], probs = c(seq(0.85,1,0.01)))

# Gruppe 4
sum(df$T_80 >118 & df$T_80 <= 161)
quantile(df$VMAX[df$T_80 >118 & df$T_80 <= 161], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >118 & df$T_80 <= 161], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >118 & df$T_80 <= 161])
quantile(df$TOTALLENGTH[df$T_80 >118 & df$T_80 <= 161], probs = c(seq(0.85,1,0.01)))

# Gruppe 5
sum(df$T_80 >161 & df$T_80 <= 185)
quantile(df$VMAX[df$T_80 >161 & df$T_80 <= 185], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >161 & df$T_80 <= 185], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >161 & df$T_80 <= 185])
quantile(df$TOTALLENGTH[df$T_80 >161 & df$T_80 <= 185], probs = c(seq(0.85,1,0.01)))

# Gruppe 6
sum(df$T_80 >185 & df$T_80 <= 226)
quantile(df$VMAX[df$T_80 >185 & df$T_80 <= 225], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >185 & df$T_80 <= 225], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >185 & df$T_80 <= 225])
quantile(df$TOTALLENGTH[df$T_80 >185 & df$T_80 <= 225], probs = c(seq(0.85,1,0.01)))

# Gruppe 6a
sum(df$T_80 >185 & df$T_80 <= 226 & df$BRAKECLASS != "G")
quantile(df$VMAX[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS != "G"])
quantile(df$TOTALLENGTH[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS != "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 6b
sum(df$T_80 >185 & df$T_80 <= 226 & df$BRAKECLASS == "G")
quantile(df$VMAX[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)))
quantile(df$BRAKEPERCENTAGE[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS == "G"])
quantile(df$TOTALLENGTH[df$T_80 >185 & df$T_80 <= 225 & df$BRAKECLASS == "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 7
sum(df$T_80 >226 & df$T_80 <= 272)
quantile(df$VMAX[df$T_80 >225 & df$T_80 <= 272], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >225 & df$T_80 <= 272], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >225 & df$T_80 <= 272])
quantile(df$TOTALLENGTH[df$T_80 >225 & df$T_80 <= 272], probs = c(seq(0.85,1,0.01)))

# Gruppe 7a
sum(df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS != "G")
quantile(df$VMAX[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS != "G"])
quantile(df$TOTALLENGTH[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS != "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 7b
sum(df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS == "G")
quantile(df$VMAX[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS == "G"])
quantile(df$TOTALLENGTH[df$T_80 >225 & df$T_80 <= 272 & df$BRAKECLASS == "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 8
sum(df$T_80 >272 & df$T_80 <= 380)
quantile(df$VMAX[df$T_80 >272 & df$T_80 <= 380], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >272 & df$T_80 <= 380], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >272 & df$T_80 <= 380])
quantile(df$TOTALLENGTH[df$T_80 >272 & df$T_80 <= 380], probs = c(seq(0.85,1,0.01)))

# Gruppe 8a
sum(df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS != "G")
quantile(df$VMAX[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS != "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS != "G"])
quantile(df$TOTALLENGTH[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS != "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 8a
sum(df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS == "G")
quantile(df$VMAX[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)), na.rm = T)
quantile(df$BRAKEPERCENTAGE[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS == "G"], probs = c(seq(0,1,0.05)))
table(df$BRAKECLASS[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS == "G"])
quantile(df$TOTALLENGTH[df$T_80 >272 & df$T_80 <= 380 & df$BRAKECLASS == "G"], probs = c(seq(0.85,1,0.01)))

# Gruppe 9
sum(df$T_80 >380 & df$T_80 <= 679)

# Find a_v-Values

elem <- tfzNames[tfzNames$name == "140-1", ]
avModel <- getAVModel(elem$i, elem$j, 1600, 1)
avModel[41,]
avModel[81,]

} # if False
