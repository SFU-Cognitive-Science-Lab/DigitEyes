' 
  This runs a kruskal wallis test between leagues, compares the relative fixation distances from other leagues to bronze and graphs the data.
  
  Author: Yue Chen & Caitlyn McColeman
  Date Created: Feb 21 2017 
  Last Edit: 
  
  Cognitive Science Lab, Simon Fraser University 
  Originally Created For: [StarTrak] 
  
  Reviewed: [Tyrus Tracey & Davey Mac, Nov 7th, 2017] 
  Verified: [Joe Feb 1] 
  
  INPUT:  requires that saccadeAmplitude.csv is on the path (via table_builder.m); read it as input.                   
  
  OUTPUT: 
'

require('ez')
require('ggplot2')
require('lsr')

# allows Quartz to work in windows
if(.Platform$OS.type=="windows") { 
  quartz<-function() windows()
}

# Move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

# Load data (assumes csv is on path)
relativeScouting = read.table('saccadeAmplitude.csv', header = TRUE, sep = ",")

# Filter data to exclude incomplete data and NaN values, then converts into a factor
noNaNScouts = relativeScouting[complete.cases(relativeScouting$AllLeagueRec_Scouts),]
noNaNScouts = noNaNScouts[is.finite(noNaNScouts$Scouts),]
noNaNScouts$AllLeagueRec_Scouts = factor(noNaNScouts$AllLeagueRec_Scouts)

# Assigns a unique identifaction code to each game
gameIDRecord = 1:length(noNaNScouts$AllLeagueRec_Scouts)
noNaNScouts = cbind(gameIDRecord, noNaNScouts)
noNaNScouts$gameIDRecord = factor(noNaNScouts$gameIDRecord) 


# Run the anova to prep for looking into assumptions
rt_anova = ezANOVA(
  noNaNScouts
  , dv = Scouts
  , wid = gameIDRecord
  , between = AllLeagueRec_Scouts
  , type = 3
  , return_aov = T
)

# 1 . Normality test
shapiro.test(residuals(rt_anova$aov))

boxplot(Scouts~AllLeagueRec_Scouts, data=noNaNScouts, xlab='League', ylab='Relative Scouting Distance')
title('Relative Scouting Analaysis')

# Additional plots
quartz()

fixDistImg = ggplot(noNaNScouts[!(noNaNScouts$AllLeagueRec_Scouts == 0),], aes(AllLeagueRec_Scouts,Scouts))
fixDistImg = fixDistImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
fixDistImg = fixDistImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
fixDistImg = fixDistImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
fixDistImg = fixDistImg + labs(x = "League")
fixDistImg = fixDistImg + labs(y = "Normalized Distance") + scale_y_continuous(limits = c(0, 1))
fixDistImg = fixDistImg + ggtitle('Distance Between Fixations by League')

setwd('../figures/') # Set directory to "figures" folder

ggplot_build(fixDistImg)
ggsave('fixDistImg.pdf', width = 7, height = 5, units = c("in"))

# 2. Run a non-parametric Kruskal Wallis test
kresult=kruskal.test(noNaNScouts$Scouts~noNaNScouts$AllLeagueRec_Scouts)

# Get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter +1
n = length(unique(noNaNScouts$gameIDRecord));
etaSq = (H - k + 1)/(n-k)

# 2b. Run Wilcox test to explore differences between the "Silver" and the "Master" groups which lie towards the opposite end of the possible league spectrum
diffBetwenSilverAndMaster = wilcox.test(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 2],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 6])

# 3. Looks at differences between the bronze league games and each subsequent league. Helpful for visualizing more typical learning curves.
# Note: we use a family-wise error Bonferroni correction to produce an alpha of .05/6 = 0.008.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 1],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value 
    
    # Calculate Effect Size
    bronzeVsLater[leagueNum-1,4] = cohensD(noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == 1],noNaNScouts$Scouts[noNaNScouts$AllLeagueRec_Scouts == leagueNum])
}


## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: []
# verified: []
#### RCAB: Ran this on my computer to review it. Runs fine, but the y-axis start below zero for some odd reason. If this can be fixed, I'd call this reviewed.
#### RCAB: Also note Jordan's observation that these values should perhaps be scaled by league to account for different group sizes
ggplot(data = noNaNScouts) + geom_histogram(aes(x = noNaNScouts$AllLeagueRec_Scouts), stat="count") + labs(title = "Number of Observations in Analysis: Saccade amplitude") + 
  labs(x="League", y="Count")

ggsave('../figures/saccadeAmplitudeHist.pdf', width = 7, height = 5, units = c("in"))
