"Description: This script runs analyses on a wide variety of variables that are considered to be supplementary to the primary analyses included in the paper.
The list of Variables analyzed here is as follows: 
  PAC Durations, Fixation Durations, 
  First Action Latency Durations, Between Action Latency Durations, New View Cost Durations, 
  Fixation Rates Within Starcraft, Fixation rates during eyetracking experiments, 
  Distances Between Fixations, Ratio of Hotkeys to Regular Select Actions, Offscreen Production, 
  MiniMap Abilities, MiniMap Right Clicks, and MiniMap Attacks"

" Author: Caitlyn McColeman & Tyrus Tracey
  Date Created: Apr 8 2019 
Last Edit:

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes Supplementary Material 

Reviewed: [Robin]
Verified: 

INPUT: requires ultraTable.csv.zip is in the data directory

OUTPUT: "



# Load data from file


# 1. raw, PAC level table for supplementary
unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

ultraTab = read.table('../data/ultraTable.csv', header = T, sep=',')
ultraTabViable = ultraTab[ultraTab$in_analysis == 1,]

# 2. additional statistics for reported measures in DigitEyes manuscript
masterTab = read.table('../data/masterTable_backup.csv', header = TRUE, sep = ",") # RCAB: Someone changed the name of this file to mastertable_backup. Is there a new file we should be using?


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

FixPACDuration <- cbind(PACSummary,FixSummary, leagueObs[,2]) # RCAB: May want to just use "FixSummary[,2]" for readability.
PACSummary = round(PACSummary, digits = 2)

# ============================== PAC Variables, low level details from raw data ============================== # 

# manage data types
ultraTabViable$ActionLatency = as.numeric(as.character(ultraTabViable$ActionLatency))/88.5347*1000 # RCAB: Perhaps comment as to where the 88.5347 comes from
ultraTabViable$NewViewCost = as.numeric(as.character(ultraTabViable$NewViewCost))/88.5347*1000
ultraTabViable$BetweenActionLatency = as.numeric(as.character(ultraTabViable$BetweenActionLatency))/88.5347*1000

# PAC variables to summarize from ultra table
doSummary = c('ActionLatency', 'NewViewCost', "BetweenActionLatency") # RCAB: As th data under these headers is transformed, the names should reflect whether these are the means or the medians, etc.

PACVariables = aggregate(cbind(ultraTabViable$ActionLatency, ultraTabViable$NewViewCost, ultraTabViable$BetweenActionLatency) ~ leagueidx, data = ultraTabViable, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x), nObs = length(x)) )

# make human readable
names(PACVariables) <- c("leagueIdx", doSummary)

# sample size
PACVariables = cbind(PACVariables, leagueObs[,2])

PACVariables = round(PACVariables, digits = 2)

## ============================ # ============================ # ============================ # ============================ ##
##                    Section 2: Statistical supplement for all reported data in the DigitEyes manuscript
## ============================ # ============================ # ============================ # ============================ ##

# A. Fixation Duration                (via fixMedianNonPAC; one observation per participant)
FixDurData = read.delim("../data/fixMedianNonPAC.txt", sep = ',')
FixDurData$grandMediansOut = FixDurData$grandMediansOut/88.5347*1000
FixDurStats = do.call(data.frame, aggregate(FixDurData$grandMediansOut ~ grandLeaguesOut, data = FixDurData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(FixDurStats) <- c('League', 'Fixation Duration Mean', 'Fixation Duration SD', 'Fixation Duration Median') 

# B. Perception Action Cycle Duration (via fixMedianPAC; one observation per participant)
PACDurData = read.delim("../data/fixMedianPAC.txt", sep = ",")
PACDurData$grandMediansOut = PACDurData$grandMediansOut/88.5347*1000
PACDurStats = do.call(data.frame, aggregate(PACDurData$grandMediansOut ~ grandLeaguesOut, data = PACDurData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(PACDurStats) <- c('League', 'PAC Duration Mean', 'PAC Duration SD', 'PAC Duration Median') 

# C. First Action Latency             (via Master table; one observation per participant)
FALData <- data.frame("leagueIdx" = masterTab$leagueidx, "FAL" = masterTab$pacactionlatencymean)
FALData$FAL = FALData$FAL/88.5347*1000

FALData = FALData[complete.cases(FALData$FAL),]
FALData = FALData[is.finite(FALData$FAL),]

FALStats = do.call(data.frame, aggregate(FALData$FAL ~ leagueIdx, data = FALData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(FALStats) <- c('League', 'FAL Mean', 'FAL SD', 'FAL Median')

# D. Between Action Latency           (via Ultra table; one observation per participant)
BALData = data.frame(ultraTabViable$gameid, ultraTabViable$leagueidx, ultraTabViable$BetweenActionLatency)
colnames(BALData) = c('gameid', 'leagueidx', 'BAL')
BALData = BALData[!is.na(BALData$BAL),]
BALData = BALData[order(BALData$leagueidx),]
BALData = aggregate(as.numeric(as.character(BALData$BAL)), by = list(BALData$gameid, BALData$leagueidx), FUN = mean)
colnames(BALData) = c('gameid', 'leagueidx', 'BAL')

BALStats = do.call(data.frame, aggregate(BALData$BAL, list(BALData$leagueidx), FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(BALStats) = c('League', 'BAL Mean', 'BAL SD', 'BAL Median')

# E. New View Cost                    (via NVC.csv)
NVCData = read.delim("../data/NVC.csv", sep = ',')
NVCStats = aggregate(NVCData$NVC ~ AllLeagueRec_NVC, data = NVCData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(NVCStats) <- c("leagueIdx", "NewViewCost")

# F. Distance between fixations       (via saccadeAmplitude.csv)
amplitudeData = read.delim("../data/saccadeAmplitude.csv", sep = ',')
amplitudeData = amplitudeData[is.finite(amplitudeData$Scouts),]

amplitudeStats = do.call(data.frame, aggregate(amplitudeData$Scouts ~ AllLeagueRec_Scouts, data = amplitudeData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(amplitudeStats) <- c('League', 'Amplitude Mean', 'Amplitude SD', 'Amplitude Median')

# G. HK:Select                        (via hkVSSel.csv)
HKtoSelData = read.delim("../data/hkVSSel.csv", sep = ',')
HKtoSelData = HKtoSelData[is.finite(HKtoSelData$ratioRec),]

HKtoSelStats = do.call(data.frame, aggregate(ratioRec ~ LeagueIdx.x, data = HKtoSelData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(HKtoSelStats) <- c('League', 'Hotkey Select Mean', 'Hotkey Select SD', 'Hotkey Select Median')

# H. OffScreen Production             (via playerOnOffProduction.csv)
OffScreenProdData = read.delim("../data/playerOnOffProduction.csv", sep = ',')

OffScreenPercentStats = do.call(data.frame, aggregate(OffScreenPercent ~ LeagueNum, data = OffScreenProdData, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x))))
colnames(OffScreenPercentStats) <- c('League', 'Offscreen Mean', 'Offscreen SD', 'Offscreen Median')

# I. Mini-Map Ability                 (via Master table; one observation per participant)
MapAblStats = aggregate(masterTab$MapAblPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapAblStats)[2] <- ("MapAblPerMin")

# J. Mini-Map Attacks                 (via Master table; one observation per participant)
MapAtkStats = aggregate(masterTab$MapAtkPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapAtkStats)[2] <- ("MapAtkPerMin")

# K. Mini-Map Right Clicks            (via Master table; one observation per participant)
MapRCStats = aggregate(masterTab$MapRCPerMin ~ leagueidx, data = masterTab, FUN = function(x) c(meanVal = mean(x), SD = sd(x), medianVal = median(x)))
names(MapRCStats)[2] <- ("MapRCPerMin")


