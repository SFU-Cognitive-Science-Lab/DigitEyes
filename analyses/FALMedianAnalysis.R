
" Author: Caitlyn McColeman
Date Created: July 31 2018 
Last Edit:

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes' supplementary material

Reviewed: 
Verified: 

INPUT:                     

OUTPUT: 

This function expects that it exists in a folder called 'analysis' in the DigitEyes directory and that there is another folder containing data. 
Recommended use: download or clone the repository from our GitHub page, and run by setting the working directory to DigitEyes/analysis.
"

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

ultraTab = read.table('ultraTable.csv', header = TRUE, sep = ",")

FAL = (as.numeric(as.character(ultraTab$ActionLatency))/88.5347)*1000 # convert from timestamps to milliseconds
AllLeagueRec_FAL = ultraTab$leagueidx
subrec = ultraTab$gameid

FALTable = cbind(AllLeagueRec_FAL, FAL, subrec)

FALTable = as.data.frame(FALTable)

# Eliminate missing values from first action latency.
noNaNFAL = FALTable[complete.cases(FALTable$FAL),]  # if it's not a PAC, then there will be no first action latency

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
