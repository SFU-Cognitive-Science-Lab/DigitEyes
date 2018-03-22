" Author: Robin C. A. Barrett, Caitlyn McColeman
  Date Created: 15 Feb, 2018
     Last Edit: 19 Mar, 2018 - [C.M.] made public friendly, added eyetracking analysis and prepared for GitHub export

Cognitive Science Lab, Simon Fraser University 
Originally Created For: StarTrak

Reviewed: [] 
Verified: [] 

INPUT:                     

OUTPUT: 

Additional Scripts Used: requires that FixationRate_BarGraphsByLeague.m was run in MATLAB. Or that its output is available in the ../data folder (EyeTrackFixRate.csv, SC2FixRate.csv)


"

require('psych')
require('reshape')
require('vioplot')



require('ez')
require('ggplot2')

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder



#################### StarCraft2 data ####################

SCFixRate = read.table('SC2FixRate.csv', header = TRUE, sep = ",")
names(SCFixRate) = c("leagueID", "fixRate")

# Specify that the League Column is a Factor
SCFixRate$leagueID = factor(SCFixRate$leagueID)

# Add an Indexing variable
gameIDRecord = 1:length(SCFixRate$leagueID)
SCFixRate = cbind(gameIDRecord, SCFixRate)

fixRateImg = ggplot(SCFixRate, aes(SCFixRate$leagueID,fixRate))
fixRateImg = fixRateImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
fixRateImg = fixRateImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
fixRateImg = fixRateImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
fixRateImg = fixRateImg + labs(x = "League")
fixRateImg = fixRateImg + labs(y = "Fixation Rate")
fixRateImg = fixRateImg + ggtitle('Screen Fixation Rate by League')
ggsave('../figures/SCFixRateImg.pdf', width = 7, height = 5, units = c("in"))


kresult=kruskal.test(SCFixRate$fixRate~SCFixRate$leagueID)

diffBetwenSilverAndMaster = wilcox.test(SCFixRate$fixRate[SCFixRate$leagueID == 2],SCFixRate$fixRate[SCFixRate$leagueID == 6])


# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(SCFixRate$gameIDRecord));
etaSq = (H - k + 1)/(n-k)


# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(SCFixRate$fixRate[SCFixRate$leagueID == 2],SCFixRate$fixRate[SCFixRate$leagueID == 6])

# 3. mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from fixRateAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
  # t-test
  pairCompare = t.test(SCFixRate$fixRate[SCFixRate$leagueID == 1],SCFixRate$fixRate[SCFixRate$leagueID == leagueNum])
  
  bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
  bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
  bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
  
  # effect size
  bronzeVsLater[leagueNum-1,4] = cohensD(SCFixRate$fixRate[SCFixRate$leagueID == 1],SCFixRate$fixRate[SCFixRate$leagueID == leagueNum])
}


#################### EyeTracking data ####################

ETFixRate = read.table('EyeTrackFixRate.csv', header = TRUE, sep = ",")
names(ETFixRate) <- c("participantID", "fixRateFirst2", "fixRateLast2")

# the formatting is different for the eye tracking data. reshape it so it's long format. 
ETFixRateLong <- melt(ETFixRate, id=c("participantID"))
names(ETFixRateLong) <- c("participantID", "whichTrials", "fixationRate")

ETFixRateImg = ggplot(ETFixRateLong, aes(ETFixRateLong$whichTrials,ETFixRateLong$fixationRate))
ETFixRateImg = ETFixRateImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
ETFixRateImg = ETFixRateImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
ETFixRateImg = ETFixRateImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
ETFixRateImg = ETFixRateImg + labs(x = "Temporal Range (mins)")
ETFixRateImg = ETFixRateImg + labs(y = "Fixation Rate")
ETFixRateImg = ETFixRateImg + ggtitle('Eye Fixation Rate: First, Last Minutes')
ETFixRateImg = ETFixRateImg + scale_x_discrete(labels=c("fixRateFirst2" = "First 2", "fixRateLast2" = "Last 2"))
ggsave('../figures/ETFixRateImg.pdf', width = 7, height = 5, units = c("in"))

kresultET = kruskal.test(ETFixRateLong$fixationRate~ETFixRateLong$whichTrials)

diffBetwenFirstAndLastET = wilcox.test(ETFixRateLong$fixationRate[ETFixRateLong$whichTrials == "fixRateFirst2"], ETFixRateLong$fixationRate[ETFixRateLong$whichTrials == "fixRateLast2"])


# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
HET = kresultET$statistic
kET = kresultET$parameter + 1
nET = length(unique(ETFixRateLong$participantID));
etaSqET = (HET - kET + 1)/(nET-kET)

