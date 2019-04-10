
" Author: Judi Azmand, Romanos Byliris, YuYing Mak
  Date Created: 5 April 2017 
  Last Edit: 5 April 2017
  
  Cognitive Science Lab, Simon Fraser University 
  Originally Created For: [DigitEyes] 
  
  Reviewed: [Ximin & Joe] 
  Verified: [Alex & Neda] Nov.15/17
  
  INPUT:                     
  
  OUTPUT: 
  "
 
 # This is a good reference for implementing high level ANOVA ideas in R code:
 # http://www.unh.edu/halelab/BIOL933/labs/lab5.pdf The comments that follow apply
 # mostly to our analysis with the DigitEyes project.
 
require('ez')
require('lsr')
library('ggplot2')

# allows Quartz to work in windows
if(.Platform$OS.type=="windows") { 
  quartz<-function() windows()
}

# Move to this directory, & use it as a reference point for finding the data folder.
this.dir <- dirname(parent.frame(2)$ofile)

setwd(this.dir)

# Move up and into data folder.
setwd('../data/') 

masterTable = read.table('masterTable.csv', header = TRUE, sep = ",")
 
             FAL = masterTable$pacactionlatencymean/88.5347*1000
AllLeagueRec_FAL = masterTable$leagueidx
          subrec = masterTable$gameid

FALTable = cbind(AllLeagueRec_FAL, FAL, subrec)

FALTable = as.data.frame(FALTable)

# Eliminate missing values from first action latency.
noNaNFAL = FALTable[complete.cases(FALTable$FAL),] 

noNaNFAL = noNaNFAL[is.finite(FALTable$FAL),] 

noNaNFAL$AllLeagueRec_FAL = factor(noNaNFAL$AllLeagueRec_FAL)
          noNaNFAL$subrec = factor(noNaNFAL$subrec) 

# Runs the anova to prep test assumptions.
rt_anova = ezANOVA(
  noNaNFAL
  , dv = FAL
  , wid = subrec
  , between = AllLeagueRec_FAL
  , type = 3
  , return_aov = T
)

# 1a. Runs normality test.
stFAL = shapiro.test(residuals(rt_anova$aov))

# 1b. If normality test fails; then drop grandmaster & repeat analysis with only sufficiently large groups, as follows in 1c.
league7Idx = noNaNFAL$AllLeagueRec_FAL == 7
noGMFAL = noNaNFAL[!league7Idx,]

# 1c. Runs ANOVA without small sample group. In our case grandmaster.
FALTest = ezANOVA(
  noGMFAL
  , dv = FAL
  , wid = subrec
  , between = AllLeagueRec_FAL
  , type = 3
  , return_aov = T
)

# Creates histogram of residuals.
hist(residuals(FALTest$aov))

quartz()

FALImg = ggplot(noNaNFAL, aes(AllLeagueRec_FAL,FAL))
FALImg = FALImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
FALImg = FALImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
FALImg = FALImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
FALImg = FALImg + labs(x = "League")
FALImg = FALImg + labs(y = "First Action Latency (ms)")
FALImg = FALImg + ggtitle('First Action Latency by League')

# Move up and into figures folder.
setwd('../figures/')

ggplot_build(FALImg)

ggsave("FALImg.pdf", width = 7, height = 5, units = c("in"))

# 2a. If using noGMScoutingTestLog & the script still fails Levene's test; then ANOVA is not viable in this instance.
diffBetwenSilverAndMaster = wilcox.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 2],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 6])

# 2b. If so, then go with Kruskal-Wallis for easier comparision between measures in StarTrak & to be safe with parametric test assumptions. So run a non-parametric Kruskal Wallis test.
kresult=kruskal.test(noNaNFAL$FAL~noNaNFAL$AllLeagueRec_FAL)

# Now get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(noNaNFAL$subrec));
etaSq = (H - k + 1)/(n-k)

# 2c. Determine the difference between the opposite ends of the possible leagues.
diffBetwenSilverAndMaster = wilcox.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 2],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 6])

# 2d. Our idea to look at bronze vs. subsequent leagues; based on a pairwise test from NVCAnalysis.R was helpful for more typical learning curve distributions.
# For example we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis, being that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7)
  {
    # T-test calculated here.
    pairCompare = t.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 1],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # Effect size calculated here.
    bronzeVsLater[leagueNum-1,4] = cohensD(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 1],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == leagueNum])
}

## LMER
# reviewed: [Joe]
# verified: []
require('lme4')

# read data
unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

ultraTab = read.table('ultraTable.csv', header = T, sep=',')
ultraTabViable = ultraTab[ultraTab$in_analysis == 1,]

# specify data class
ultraTabViable$ActionLatency = as.numeric(as.character(ultraTabViable$ActionLatency))
ultraTabViable$leagueidx = as.factor(ultraTabViable$leagueidx)

# fit model
lmeBaseMod=lmer(ActionLatency~(1|gameid),data=ultraTabViable)
lmeLeagueMod=lmer(ActionLatency~leagueidx+(1|gameid),data=ultraTabViable)

anova(lmeBaseMod,lmeLeagueMod)

## Number of observations histograms
# reviewed: [Joe]
# verified: []
ObsHistDat = aggregate(ActionLatency~leagueidx, data = ultraTabViable[!is.na(ultraTabViable$ActionLatency),], FUN = length)
histImg = ggplot(data = ObsHistDat, aes(x=leagueidx, y=ActionLatency)) + geom_bar(stat='identity') 
histImg = histImg + labs(title = "Number of FAL Observations by League", x = "League", y = "Count")

ggsave('../figures/FALRawHist.pdf', width = 7, height = 5, units = c("in"))


