' 
  This runs a kruskal wallis test between leagues, compares the relative fixation distances from other leagues to bronze and graphs the data.
  
  Author: Yue Chen & Caitlyn McColeman
  Date Created: Feb 21 2017 
  Last Edit: 
  
  Cognitive Science Lab, Simon Fraser University 
  Originally Created For: [StarTrak] 
  
  Reviewed: [Tyrus Tracey & Davey Mac, Nov 7th, 2017] 
  Verified: [Joe Feb 1] 
  
  INPUT:  requires saccadeAmplitude.csv is on the path (via table_builder.m); read it as input.                   
  
  OUTPUT: 
  
  Additional Scripts Used: requires that Scouting_CellBuilder.m was run in MATLAB. The analysis is conducted on RelativeScouts_cells.
  
 a good reference for implementing high level ANOVA ideas in R code
 http://www.unh.edu/halelab/BIOL933/labs/lab5.pdf
 
  Additional Comments: 
         Changelog 
           - Added kruskal test, edited figures for greyscale, added effect size calculations. C.M. [Nov 4 2017]


'
require('ez')
require('ggplot2')
require('lsr')

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

# load data first (assumes you're on a shared lab machine. check the directory structure)
relativeScouting = read.table('saccadeAmplitude.csv', header = TRUE, sep = ",")

### CHANGED BY JOE/NATHAN made dataframe relative scouting here...
noNaNScouts = relativeScouting[complete.cases(relativeScouting$AllLeagueRec_Scouts),] ##Nathan: input is good as above
noNaNScouts = noNaNScouts[is.finite(noNaNScouts$Scouts),] ##Nathan: infs successfully removed
noNaNScouts$AllLeagueRec_Scouts = factor(noNaNScouts$AllLeagueRec_Scouts) ##Nathan: turns the vector into a factor success!

# need to be careful her ebecause these are made up gameid values
gameIDRecord = 1:length(noNaNScouts$AllLeagueRec_Scouts)
noNaNScouts = cbind(gameIDRecord, noNaNScouts) ##Nathan: does nothing
noNaNScouts$gameIDRecord = factor(noNaNScouts$gameIDRecord) 


# run the anova to prep for looking into assumptions
rt_anova = ezANOVA(
  noNaNScouts
  , dv = Scouts
  , wid = gameIDRecord
  , between = AllLeagueRec_Scouts
  , type = 3
  , return_aov = T
)

# 1 . normality test
shapiro.test(residuals(rt_anova$aov))

### WTF is league zero???
boxplot(Scouts~AllLeagueRec_Scouts, data=noNaNScouts, xlab='League', ylab='Relative Scouting Distance')
title('Relative Scouting Analaysis')
##Nathan: ignoring graphs

# additional plots added for poster (Caitlyn)
quartz()

fixDistImg = ggplot(noNaNScouts[!(noNaNScouts$AllLeagueRec_Scouts == 0),], aes(AllLeagueRec_Scouts,Scouts))
fixDistImg = fixDistImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
fixDistImg = fixDistImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
fixDistImg = fixDistImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
fixDistImg = fixDistImg + labs(x = "League")
fixDistImg = fixDistImg + labs(y = "Normalized Distance") + scale_y_continuous(limits = c(0, 1))
fixDistImg = fixDistImg + ggtitle('Distance Between Fixations by League')

setwd('../figures/') # move up and into figures folder

ggsave('fixDistImg.pdf', width = 7, height = 5, units = c("in"))

'
# 1b. normality fails; drop grandmaster and repeat analysis with only sufficiently large groups
league7Idx = noNaNScouts$AllLeagueRec_Scouts == 7

noGMScouts = noNaNScouts[!league7Idx,]

# 1c. run ANOVA without small group (grandmaster)
noGMScoutingTest = ezANOVA(
  noGMScouts
  , dv = Scouts
  , wid = gameIDRecord
  , between = AllLeagueRec_Scouts
  , type = 3
  , return_aov = T
)

# look at histogram of residuals
hist(residuals(noGMScoutingTest$aov))

# run test
shapiro.test(residuals(noGMScoutingTest$aov))

# test fails; residuals generally okay for such a large sample size.

# take a look at variance between groups via box plots
boxplot(Scouts~AllLeagueRec_Scouts,data=noNaNScouts)

# variance is a mess and fails the assumption test too; look better upon transformation
# (check noGMScoutingTest Levenes results )
boxplot(log(Scouts)~AllLeagueRec_Scouts,data=noNaNScouts)

# transformed data into the ANOVA to see if that is an acceptable homogeneity of variance
noGMScoutingTestLog = ezANOVA(
  noGMScouts
  , dv = log(Scouts)
  , wid = gameIDRecord
  , between = AllLeagueRec_Scouts
  , type = 3
  , return_aov = T
)
'
# 2. Run a non-parametric Kruskal Wallis test
kresult=kruskal.test(noNaNScouts$Scouts~noNaNScouts$AllLeagueRec_Scouts)

# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter +1
n = length(unique(noNaNScouts$gameIDRecord));
etaSq = (H - k + 1)/(n-k)

# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 2],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 6])

# 3. mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 1],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value 
    
    # effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 1],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == leagueNum])
}

