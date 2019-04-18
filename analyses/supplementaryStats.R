" Author: Caitlyn McColeman & Tyrus Tracey
  Date Created: Apr 8 2019 
Last Edit:

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes Supplementary Material 

Reviewed: 
Verified: 

INPUT: requires ultraTable.csv.zip is in the data directory

OUTPUT: "



# read data


# 1. raw, PAC level table for supplementary
unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

ultraTab = read.table('../data/ultraTable.csv', header = T, sep=',')
ultraTabViable = ultraTab[ultraTab$in_analysis == 1,]

# 2. additional statistics for reported measures in DigitEyes manuscript
masterTab = read.table('../data/masterTable.csv', header = TRUE, sep = ",")


leagueObs = aggregate(ultraTabViable$gameid ~ leagueidx, data = ultraTabViable, FUN = function(x) c(nParticipants = length(unique(x))))

## ============================ # ============================ # ============================ # ============================ ##
##      Section 1: Statistical overview of the raw data for Fixation durations and Perception Action Cycle variables
## ============================ # ============================ # ============================ # ============================ ##


# ============================== Fixation, PAC Duration, low level details from raw data ============================== # 
ultraTabViable$FixDuration = as.numeric(as.character(ultraTabViable$FixDuration))
PACTable = ultraTabViable[ultraTabViable$PACidx == 1, ]
FixTable = ultraTabViable[ultraTabViable$PACidx == 0, ]

PACSummary = aggregate(FixDuration ~ leagueidx, data = PACTable, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x), nObs = length(x)))
FixSummary = aggregate(FixDuration ~ leagueidx, data = FixTable, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x), nObs = length(x)))

names(PACSummary) <- c("leagueIdx", "PACDuration")

FixPACDuration <- cbind(PACSummary,FixSummary, leagueObs[,2])
PACSummary = round(PACSummary, digits = 2)

# ============================== PAC Variables, low level details from raw data ============================== # 

# manage data types
ultraTabViable$ActionLatency = as.numeric(as.character(ultraTabViable$ActionLatency))/88.5347*1000
ultraTabViable$NewViewCost = as.numeric(as.character(ultraTabViable$NewViewCost))/88.5347*1000
ultraTabViable$BetweenActionLatency = as.numeric(as.character(ultraTabViable$BetweenActionLatency))/88.5347*1000

# PAC variables to summarize from ultra table
doSummary = c('ActionLatency', 'NewViewCost', "BetweenActionLatency")

PACVariables = aggregate(cbind(ultraTabViable$ActionLatency, ultraTabViable$NewViewCost, ultraTabViable$BetweenActionLatency) ~ leagueidx, data = ultraTabViable, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x), nObs = length(x)) )

# make human readable
names(PACVariables) <- c("leagueIdx", doSummary)

# sample size
PACVariable = cbind(PACVariables, leagueObs[,2])

PACVariables = round(PACVariables, digits = 2)

## ============================ # ============================ # ============================ # ============================ ##
##                    Section 2: Statistical supplement for all reported data in the DigitEyes manuscript
## ============================ # ============================ # ============================ # ============================ ##

# A. Fixation Duration                (via fixMedianNonPAC; one observation per participant)
FixDurData = read.delim("../data/fixMedianNonPAC.txt", sep = ',')
FixDurData$grandMediansOut = FixDurData$grandMediansOut/88.5347*1000
FixDurStats = aggregate(FixDurData$grandMediansOut ~ grandLeaguesOut, data = FixDurData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(FixDurStats) <- c("leagueIdx", "FixationDuration")

# B. Perception Action Cycle Duration (via fixMedianPAC; one observation per participant)
PACDurData = read.delim("../data/fixMedianPAC.txt", sep = ",")
PACDurData$grandMediansOut = PACDurData$grandMediansOut/88.5347*1000
PACDurStats = aggregate(PACDurData$grandMediansOut ~ grandLeaguesOut, data = PACDurData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(PACDurStats) <- c("leagueIdx", "PAC Duration")

# C. First Action Latency             (via Master table; one observation per participant)
FALData <- data.frame("leagueIdx" = masterTab$leagueidx, "FAL" = masterTab$pacactionlatencymean)
FALData$FAL = FALData$FAL/88.5347*1000

FALData = FALData[complete.cases(FALData$FAL),]
FALData = FALData[is.finite(FALData$FAL),]

FALStats = aggregate(FALData$FAL ~ leagueIdx, data = FALData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(FALStats)[2] <- ("FAL")

# D. Between Action Latency           (via Ultra table; one observation per participant)
BALData <- data.frame("leagueIdx" = ultraTabViable$leagueidx, "BAL" = ultraTabViable$BetweenActionLatency)
BALData = BALData[!is.na(BALData$BAL),]

BALStats = aggregate(BALData$BAL ~ leagueIdx, data = BALData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(BALStats)[2] <- ("BAL")

# E. New View Cost                    (via NVC.csv)
NVCData = read.delim("../data/NVC.csv", sep = ',')
NVCStats = aggregate(NVCData$NVC ~ AllLeagueRec_NVC, data = NVCData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(NVCStats) <- c("leagueIdx", "NewViewCost")

# F. Fixation Rate                    (via SC2FixRate.csv)
# G. Fixation rate                    (via EyeTrackFixRate.csv)

# H. Distance between fixations       (via saccadeAmplitude.csv)
amplitudeData = read.delim("../data/saccadeAmplitude.csv", sep = ',')
amplitudeData = amplitudeData[is.finite(amplitudeData$Scouts),]

amplitudeStats = aggregate(amplitudeData$Scouts ~ AllLeagueRec_Scouts, data = amplitudeData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))

names(amplitudeStats) <- c("League", "Amplitude")

# I. HK:Select                        (via hkVSSel.csv)
HKtoSelData = read.delim("../data/hkVSSel.csv", sep = ',')
HKtoSelData = HKtoSelData[is.finite(HKtoSelData$ratioRec),]

HKtoSelStats = aggregate(ratioRec ~ LeagueIdx.x, data = HKtoSelData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))

names(HKtoSelStats) <- c("League", "HotkeyToSelectRatio")

# J. OffScreen Production             (via playerOnOffProduction.csv)
OffScreenProdData = read.delim("../data/playerOnOffProduction.csv", sep = ',')

OffScreenPercentStats = aggregate(OffScreenPercent ~ LeagueNum, data = OffScreenProdData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))

names(OffScreenPercentStats) <- c("League", "OffScreenPercent")


# L. Mini-Map Ability                 (via Master table; one observation per participant)
MapAblStats = aggregate(masterTab$MapAblPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapAblStats)[2] <- ("MapAblPerMin")

# M. Mini-Map Attacks                 (via Master table; one observation per participant)
MapAtkStats = aggregate(masterTab$MapAtkPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapAtkStats)[2] <- ("MapAtkPerMin")

# N. Mini-Map Right Clicks            (via Master table; one observation per participant)
MapRCStats = aggregate(masterTab$MapRCPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapRCStats)[2] <- ("MapRCPerMin")


