# author: Joe Thompson
# June 2019

# edited by Caitlyn McColeman to include PAC variables
# originally for DigitEyes, July 2019 submission

# Review: Jordan
# - Adding document outline headers
# - note: Basically 4 components: script, TukeyFoundationANOVA function, dataload from skilldependence Master Table, and data load for variables with their own csvs.
# - added "ordered = True" to Tukey test call
# - Note that "nSmallest" changes slightly based on data source
# - suggest including BANOVA support from Jasp for each measure:
#    - Pros: visuals, robust to distributional assumption violations and sample size differences between groups.
#    - Cons: No ability to order League for post-hoc test, and not exactly what was asked for.
#
#   Review action items: Joe
#
# Verified: Robin 


# FIRST: ensure directory is the same structure as https://github.com/SFU-Cognitive-Science-Lab/DigitEyes, with a subfolder for each of data, analyses and figures


## PAC variables

data = read.csv("../data/skilldependencemastertable_final.csv", header = TRUE, sep = ",")

data$leagueidx=as.factor(data$leagueidx)
### Bootstrap Tukey

#should = 167 for the skilldependencemastertable
nSmallest = min(summary(data$leagueidx))

set.seed(1) # to make sampling reproducible

library(dplyr)

#set number of samples

Replications=50

#initialize, choose(6,2) = 15 rows
output=matrix(, nrow = 15, ncol = Replications)

TukeyFoundationANOVA=function(data, colNum, nSmallestGroup){
for (i in 1:Replications){

  #subsample
  new_data <- data %>% group_by(leagueidx) %>% sample_n(nSmallestGroup, replace=FALSE)
  new_data=data.frame(new_data)
  #aov and tukey
  Anova=aov(new_data[,colNum]~new_data$leagueidx)
  TUKEY=TukeyHSD(Anova,ordered = TRUE)$`new_data$leagueidx`[,4]
  #TUKEY=TukeyHSD(Anova)$`new_data$leagueidx`[,4]
  
  #record pvalues
  output[1:length(TUKEY),i]=TUKEY

}

#mean pvalues, and produce output
RowMEANZ=rowMeans(output)
FinalOutput=data.frame(as.factor(row.names(TukeyHSD(Anova)$`new_data$leagueidx`)),RowMEANZ, RowMEANZ<0.05)
names(FinalOutput)=c('Comparison','Mean PValue', 'significant')
FinalOutput
}

#### Skill Dependence Master Table data ####

#NVC
nvcPostHoc = TukeyFoundationANOVA(data,4,nSmallest)
write.table(nvcPostHoc, file = "NVCTukey", sep = "\t")
#FAL
FALPostHoc = TukeyFoundationANOVA(data,3,nSmallest)
write.table(FALPostHoc, file = "FALTukey", sep = "\t")
#BAL
BALPostHoc = TukeyFoundationANOVA(data,5,nSmallest)
write.table(BALPostHoc, file = "BALTukey", sep = "\t")
#fixdurationsmean
fixDurPostHoc = TukeyFoundationANOVA(data,2,nSmallest)
write.table(fixDurPostHoc, file = "fixDurTukey", sep = "\t")
#pacdurationsmean
PACPostHoc= TukeyFoundationANOVA(data,1,nSmallest)
write.table(PACPostHoc, file = "PACTukey", sep = "\t")

#### Efficiency measures  ####

#1) hotkey vs select
HKVsSel = read.csv("../data/hkVSSel.csv", header = TRUE, sep = ",")

# clean up HKVsSel
names(HKVsSel) <- c("X", "gameid", "row", "leagueidx", "HKSelCount", "X2", "leagueRedundant", "selCount", "ratioRec")
HKVsSel <- HKVsSel[HKVsSel$leagueidx != 7,]

HKVsSel$leagueidx <- factor(HKVsSel$leagueidx)
HKVsSel$leagueidx <- droplevels(HKVsSel$leagueidx)

# check counts: HKVsSel
grpSize = aggregate(ratioRec ~ leagueidx, data = HKVsSel, FUN = length)
nSmallest = min(grpSize$ratioRec)

#HKVsSelect ratio
HKVsSelPostHoc = TukeyFoundationANOVA(HKVsSel,9,nSmallest) # 9th column should be ratioRec; please check before reporting each time.
write.table(HKVsSelPostHoc, file = "HKSelTukey", sep = "\t")

#2) Off screen production
OffScreenProd = read.csv("../data/playerOnOffProduction.csv", header = TRUE, sep = ",")
OffScreenProd$leagueidx = factor(OffScreenProd$LeagueNum)

OffScreenProd = OffScreenProd[OffScreenProd$leagueidx != 7,]
grpSize = aggregate(OffScreenPercent ~ leagueidx, data = OffScreenProd, FUN = length)
nSmallest = min(grpSize$OffScreenPercent)

#Off screen ratio
OffScreenPostHoc = TukeyFoundationANOVA(OffScreenProd,8, nSmallest) # 8th column should be ratioRec; please check before reporting each time.
write.table(OffScreenPostHoc, file = "OffScreenTukey", sep = "\t")

#3) between fixation amplitude

SaccadeAmplitude = read.csv("../data/saccadeAmplitude.csv", header = TRUE, sep = ",")

SaccadeAmplitude$leagueidx = factor(SaccadeAmplitude$AllLeagueRec_Scouts)

SaccadeAmplitude = SaccadeAmplitude[SaccadeAmplitude$leagueidx != 7,]

SaccadeAmplitude = SaccadeAmplitude[is.finite(SaccadeAmplitude$Scouts),]

grpSize = aggregate(Scouts ~ leagueidx, data = SaccadeAmplitude, FUN = length) # check counts
nSmallest = min(grpSize$Scouts)

amplitudePostHoc = TukeyFoundationANOVA(SaccadeAmplitude,2, nSmallest) # 2nd column should be Scouts; please check before reporting each time.

write.table(amplitudePostHoc, file = "AmplitudeTukey", sep = "\t")

#4) between fixation amplitude

#get fixrate data
SCFixRate = read.table('../data/SC2FixRate.csv', header = TRUE, sep = ",")
names(SCFixRate) = c("leagueID", "fixRate")

#drop gm
SCFixRate=SCFixRate[SCFixRate$leagueID<7,]

# Specify that the League Column is a Factor
SCFixRate$leagueidx = factor(SCFixRate$leagueID)

# Add an Indexing variable
gameIDRecord = 1:length(SCFixRate$leagueID)
SCFixRate = cbind(gameIDRecord, SCFixRate)

grpSize = aggregate(fixRate ~ leagueidx, data = SCFixRate, FUN = length) # check counts
nSmallest = min(grpSize$fixRate)

fixRatePostHoc = TukeyFoundationANOVA(SCFixRate,3,nSmallest)

write.table(fixRatePostHoc, file = "AmplitudeTukey", sep = "\t")

## mini-map measures

#minimap attack
# this uses master table again. Therefore hte smallest sample size will be 167.

nSmallest = min(summary(data$leagueidx))

mmAttackPostHoc = TukeyFoundationANOVA(data,7, nSmallest)
write.table(mmAttackPostHoc, file = "mmAttackPostHoc", sep = "\t")
#minimap right click
mmRCPostHoc = TukeyFoundationANOVA(data,8, nSmallest)
write.table(mmRCPostHoc, file = "mmRCPostHoc", sep = "\t")
#minimap abilities
mmAbilitiesPostHoc =TukeyFoundationANOVA(data,9, nSmallest)
write.table(mmAbilitiesPostHoc, file = "mmAbilitiesPostHoc", sep = "\t")
