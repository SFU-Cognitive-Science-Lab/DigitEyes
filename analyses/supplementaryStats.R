" Author: Caitlyn McColeman
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
ultraTab = read.table('../data/ultraTable.csv', header = T, sep=',')

unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

ultraTab = read.table('../data/ultraTable.csv', header = T, sep=',')
ultraTabViable = ultraTab[ultraTab$in_analysis == 1,]

# 2. additional statistics for reported measures in DigitEyes manuscript
masterTab = read.table('masterTable.csv')


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

# A. Fixation Duration                (via Master table; one observation per participant)
# B. Perception Action Cycle Duration (via Master table; one observation per participant)
# C. First Action Latency             (via Master table; one observation per participant)
# D. Between Action Latency           (via Master table; one observation per participant)
# E. New View Cost                    (via NVC.csv)
# F. Fixation Rate                    (via SC2FixRate.csv)
# G. Fixation rate                    (via EyeTrackFixRate.csv)
# H. Distance between fixations       (via saccadeAmplitude.csv)
# I. HK:Select                        (via hkVSSel.csv)
# J. OffScreen Production             (via playerOnOffProduction.csv)
# H. Mini-Map Ability                 (via Mastesr table; one observation per participant)
# I. Mini-Map Attacks                 (via Mastesr table; one observation per participant)
# J. Mini-Map Right Clicks            (via Mastesr table; one observation per participant)
