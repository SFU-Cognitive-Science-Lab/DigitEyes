" Author: Yue Chen & Caitlyn McColeman
  Date Created: Feb 21 2017 
  Last Edit: March 6 2018

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes 

Reviewed: [Ximin & Joe] 
Verified: [Alex & Neda] Nov.17/17 

INPUT: fixMedianPAC.txt, fixMedianNonPAC.txt                     

OUTPUT: Plots

Additional Scripts Used: pacDurationMedians.m (preprocessing to create .txt files)

a good reference for implementing high level ANOVA ideas in R code:
http://www.unh.edu/halelab/BIOL933/labs/lab5.pdf

Additional Comments:

Check the toggle variable called isPAC. If it's set to 1, you'll get Perception Action Cycle durations. If it's set to 0, non-PAC fixation durations.
"

require('lsr')
require('ez')
require('ggplot2')

isPAC = 1

# Move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # Move up and into data folder

# Load data first.
if (isPAC == 1){ # PAC durations
  pacDurationMedians = read.table('fixMedianPAC.txt', header = TRUE, sep = ",")  
  
  # Details for labelling plots
  mainTitle ="PAC Duration by League"
  yLabel = "Median PAC Duration (ms)" 
  
  # Save plot by this name:
  fileNamePlot = 'PACDuration'
  
} else # isPac != 1, so these will be screen fixations.
{
  pacDurationMedians = read.table('fixMedianNonPAC.txt', header = TRUE, sep = ",")
  
  # Details for labelling plots
  mainTitle ="Fixation Duration by League"
  yLabel = "Median Fixation Duration (ms)" 
  
  # Save plot by this name:
  fileNamePlot = 'FixDuration'
}

# Check to make sure there are no NaNs or infinite values
noNaNMedians = pacDurationMedians[complete.cases(pacDurationMedians$grandMediansOut),]

noNaNMedians = noNaNMedians[is.finite(noNaNMedians$grandMediansOut),]

subRec = 1:length(noNaNMedians$grandMediansOut)
noNaNMediansComplete = cbind(noNaNMedians, subRec)

noNaNMediansComplete$grandLeaguesOut = factor(noNaNMediansComplete$grandLeaguesOut)
noNaNMediansComplete$subRec = factor(noNaNMediansComplete$subRec)

# Run ANOVA to prepare for looking into assumptions
rt_anova = ezANOVA(
  noNaNMediansComplete
  , dv = grandMediansOut
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# 1 . Normality test
shapiro.test(residuals(rt_anova$aov))

# 1b. Because the normality test fails, drop grandmaster (~30 data points) and repeat analysis with only sufficiently large groups
league7Idx = noNaNMediansComplete$grandLeaguesOut == 7

noGMMedians = noNaNMediansComplete[!league7Idx,]

# 1c. Run ANOVA without grandmaster league
noGMMediansTest = ezANOVA(
  noGMMedians
  , dv = grandMediansOut
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# Look at histogram of residuals
hist(residuals(noGMMediansTest$aov))

# Run normality test 
shapiro.test(residuals(noGMMediansTest$aov))


# Box plots to examine variance between groups
boxplot(grandMediansOut~grandLeaguesOut,data=noNaNMediansComplete)

# (check noGMMediansTest Levene's results )
quartz()
boxplot(grandMediansOut~grandLeaguesOut,data=noNaNMediansComplete, notch=FALSE, 
        col=rgb(0, 50/255, 130/255), border = rgb(.5, .5, .5), medcol = "white",
        main="Durations of Perception Action Cycles", xlab="League", ylab = "Fixation Duration")


# Variance fails ANOVA assumptions. Check Levene's test again with log-transformation.
noGMMedians$grandMediansOutLog=log(noGMMedians$grandMediansOut)

noGMMediansTestLog = ezANOVA(
  noGMMedians
  , dv = grandMediansOutLog
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# Levene's test still failed to uphold homeogeneity of variances assumption.  ANOVA is not viable.

# 2. Run a non-parametric Kruskal Wallis test
kresult=kruskal.test(noNaNMediansComplete$grandMediansOut~noNaNMediansComplete$grandLeaguesOut)

# Get effect size (as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf)
H = kresult$statistic 
k = kresult$parameter + 1
n = length(unique(noNaNMediansComplete$subRec)); 
etaSq = (H - k + 1)/(n-k)


# 2b. Run Wilcox test to examine differences between Silver and Master leagues (which lie towards opposite ends of the spectrum of possible leagues).
diffBetwenSilverAndMaster = wilcox.test(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 2],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 6])

# 3. Look at bronze vs. subsequent leagues. 
# Note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
  # T-test
  pairCompare = t.test(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 1],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == leagueNum])
  
  bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
  bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
  bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
  
  # Effect size
  bronzeVsLater[leagueNum-1,4] = cohensD(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 1],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == leagueNum])
}


###### Violin plot #####
# Variance fails assumption too; better upon transformation

quartz()

pacDurImg = ggplot(noNaNMediansComplete, aes(grandLeaguesOut,grandMediansOut))
pacDurImg = pacDurImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
pacDurImg = pacDurImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
pacDurImg = pacDurImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
pacDurImg = pacDurImg + labs(x = "League")
pacDurImg = pacDurImg + labs(y = yLabel)
pacDurImg = pacDurImg + ggtitle(mainTitle)

setwd('../figures/') # Move into figures folder
saveAsName = paste(fileNamePlot, '.pdf', sep="")

ggsave(saveAsName, width = 7, height = 5, units = c("in"))
