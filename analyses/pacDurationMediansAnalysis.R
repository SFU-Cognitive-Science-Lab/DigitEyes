" Author: Yue Chen & Caitlyn McColeman
  Date Created: Feb 21 2017 
  Last Edit: March 6 2018

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes 

Reviewed: [Ximin Zhang, Joe] 
Verified: [Alex & Neda] Nov.17/17 

INPUT:                     

OUTPUT: 

Additional Scripts Used: requires that pac/fix durations were run in MATLAB. 

a good reference for implementing high level ANOVA ideas in R code
http://www.unh.edu/halelab/BIOL933/labs/lab5.pdf

Additional Comments:

Check the toggle variable called isPAC. If it's set to 1, you'll get Perception Action Cycle durations. If it's set to 0, non-PAC fixation durations.

Changes: 
added in effect size calcuation, and a for loop to save some cut & paste - C.M. [Nov 4 2017]
updated the data calls, added a conditional to improve usability - C.M. [Mar 6 2018]
"

require('lsr')
require('ez')
require('ggplot2')

isPAC = 1

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

# load data first. Check isPac. 
if (isPAC == 1){
  pacDurationMedians = read.table('fixMedianPAC.txt', header = TRUE, sep = ",")  
  
  # details for labelling plots
  mainTitle ="PAC Duration by League"
  yLabel = "Median PAC Duration (ms)" 
  
  # save plot by this name:
  fileNamePlot = 'PACDuration'
} else # it's fixation data 
{
  pacDurationMedians = read.table('fixMedianNonPAC.txt', header = TRUE, sep = ",")
  
  # details for labelling plots
  mainTitle ="Fixation Duration by League"
  yLabel = "Median Fixation Duration (ms)" # yes, they're the same. helping reader compare values.
  
  # save plot by this name:
  fileNamePlot = 'FixDuration'
}

#We just check to make sure there are no nans or inf values
noNaNMedians = pacDurationMedians[complete.cases(pacDurationMedians$grandMediansOut),]

noNaNMedians = noNaNMedians[is.finite(noNaNMedians$grandMediansOut),]

subRec = 1:length(noNaNMedians$grandMediansOut)
noNaNMediansComplete = cbind(noNaNMedians, subRec)

noNaNMediansComplete$grandLeaguesOut = factor(noNaNMediansComplete$grandLeaguesOut)
noNaNMediansComplete$subRec = factor(noNaNMediansComplete$subRec)

# run the anova to prep for looking into assumptions
rt_anova = ezANOVA(
  noNaNMediansComplete
  , dv = grandMediansOut
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# 1 . normality test
shapiro.test(residuals(rt_anova$aov))

# 1b. normality fails; drop grandmaster and repeat analysis with only sufficiently large groups
league7Idx = noNaNMediansComplete$grandLeaguesOut == 7

noGMMedians = noNaNMediansComplete[!league7Idx,]

# 1c. run ANOVA without small group (grandmaster)
noGMMediansTest = ezANOVA(
  noGMMedians
  , dv = grandMediansOut
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# look at histogram of residuals
hist(residuals(noGMMediansTest$aov))

# run test
shapiro.test(residuals(noGMMediansTest$aov))

# test fails; residuals generally okay for such a large sample size.

# take a look at variance between groups via box plots
boxplot(grandMediansOut~grandLeaguesOut,data=noNaNMediansComplete)

# (check noGMMediansTest Levene's results )
quartz()
boxplot(grandMediansOut~grandLeaguesOut,data=noNaNMediansComplete, notch=FALSE, 
        col=rgb(0, 50/255, 130/255), border = rgb(.5, .5, .5), medcol = "white",
        main="Durations of Perception Action Cycles", xlab="League", ylab = "Fixation Duration", ylim=c(0,125))

# variance is a mess and fails the assumption test too; look better upon transfomration
# transformed data into the ANOVA to see if that is an acceptable homogeneity of variance
##### bug here. can't log in the scope of the ezANOVA #####
noGMMedians$grandMediansOutLog=log(noGMMedians$grandMediansOut)

noGMMediansTestLog = ezANOVA(
  noGMMedians
  , dv = grandMediansOutLog
  , wid = subRec
  , between = grandLeaguesOut
  , type = 3
  , return_aov = T
)

# noGMMediansTestLog still fails Levene's test; ANOVA is not viable

# 2. Run a non-parametric Kruskal Wallis test
kresult=kruskal.test(noNaNMediansComplete$grandMediansOut~noNaNMediansComplete$grandLeaguesOut)

# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic 
k = kresult$parameter + 1
n = length(unique(noNaNMediansComplete$subRec)); 
etaSq = (H - k + 1)/(n-k)


# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 2],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 6])

# 3. mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
  # t-test
  pairCompare = t.test(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 1],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == leagueNum])
  
  bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
  bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
  bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
  
  # effect size
  bronzeVsLater[leagueNum-1,4] = cohensD(noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == 1],noNaNMediansComplete$grandMediansOut[noNaNMediansComplete$grandLeaguesOut == leagueNum])
}

##### NOTE TO VERIFIER: MAKE SURE THAT IN THE PAPER We only are calling p values less than 0.008 as significant for this test.



##### NOTE TO VERIFIER: labels on the graph generated by the code below should also match what are on the paper
###### Violin plot #####
# variance is a mess and fails the assumption test too; look better upon transfomration
# (check noGMMediansTest Levene's results )
quartz()
#boxplot(grandMediansOut~grandLeaguesOut,data=noNaNMediansComplete, notch=FALSE, 
#        col=rgb(0, 50/255, 130/255), border = rgb(.5, .5, .5), medcol = "white",
#        main="Durations of Fixations without Actions", xlab="League", ylab = "Fixation Duration", ylim=c(0,125))
pacDurImg = ggplot(noNaNMediansComplete, aes(grandLeaguesOut,grandMediansOut))
pacDurImg = pacDurImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
pacDurImg = pacDurImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
pacDurImg = pacDurImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
pacDurImg = pacDurImg + labs(x = "League")
pacDurImg = pacDurImg + labs(y = yLabel)
pacDurImg = pacDurImg + ggtitle(mainTitle)

setwd('../figures/') # move up and into figures folder
saveAsName = paste(fileNamePlot, '.pdf', sep="")

ggsave(saveAsName, width = 7, height = 5, units = c("in"))
