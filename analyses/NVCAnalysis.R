" Author: Yue Chen & Robin C. A. Barrett
  Date Created: Apr 5 2017 
  Last Edit: Apr 7 2019 - McColeman added LMER, histograms for supplementary

Cognitive Science Lab, Simon Fraser University 
Originally Created For: [StarTrak] 

Reviewed: [Kayla and Nathan] 
Verified: [Joe] 

INPUT:                     

OUTPUT: 
"


require('ez')
require('vioplot')
require('ggplot2')
require('lsr')



# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

NVC_Values = read.table('NVC.csv', header = TRUE, sep = ",")


noNaN_NVC = NVC_Values[complete.cases(NVC_Values$NVC),]

noNaN_NVC = NVC_Values[is.finite(noNaN_NVC$NVC),]

noNaN_NVC$AllLeagueRec_NVC = factor(noNaN_NVC$AllLeagueRec_NVC)

# Add in a column specifying within groups identifier
gameIDRecord = 1:length(NVC_Values$AllLeagueRec_NVC)
noNaN_NVC = cbind(gameIDRecord, noNaN_NVC)
noNaN_NVC$gameIDRecord = factor(noNaN_NVC$gameIDRecord)

# run the anova to prep for looking into assumptions
rt_anova = ezANOVA(
  noNaN_NVC
  , dv = NVC
  , wid = gameIDRecord
  , between = AllLeagueRec_NVC
  , type = 3
  , return_aov = T
)

# 1 . normality test
shapiro.test(residuals(rt_anova$aov))

# 1b. normality fails; drop grandmaster and repeat analysis with only sufficiently large groups
league7Idx = noNaN_NVC$AllLeagueRec_NVC == 7

noGMNVC = noNaN_NVC[!league7Idx,]

# 1c. run ANOVA without small group (grandmaster)
noGMNVCTest = ezANOVA(
  noGMNVC
  , dv = NVC
  , wid = gameIDRecord
  , between = AllLeagueRec_NVC
  , type = 3
  , return_aov = T
)

# look at histogram of residuals
hist(residuals(noGMNVCTest$aov))

# run test
shapiro.test(residuals(noGMNVCTest$aov))


NVCImg = ggplot(noNaN_NVC, aes(AllLeagueRec_NVC,NVC))
NVCImg = NVCImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
NVCImg = NVCImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
NVCImg = NVCImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
NVCImg = NVCImg + labs(x = "League")
NVCImg = NVCImg + labs(y = "NVC (ms)")
NVCImg = NVCImg + ggtitle('New View Cost by League')
ggsave('../figures/NVCImg.pdf', width = 7, height = 5, units = c("in"))



kresult=kruskal.test(noNaN_NVC$NVC~noNaN_NVC$AllLeagueRec_NVC)

diffBetwenSilverAndMaster = wilcox.test(noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 2],noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 6])


# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(noNaN_NVC$gameIDRecord));
etaSq = (H - k + 1)/(n-k)


# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 2],noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 6])

# 3. Mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 1],noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == 1],noNaN_NVC$NVC[noNaN_NVC$AllLeagueRec_NVC == leagueNum])
}



## LMER
# reviewed: []
# verified: []
require('lme4')
# read data
unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

ultraTab = read.table('ultraTable.csv', header = T, sep=',')
ultraTabViable = ultraTab[ultraTab$in_analysis == 1,]

# specify data class
ultraTabViable$NVC = as.numeric(as.character(ultraTabViable$NewViewCost))
ultraTabViable$leagueidx = as.factor(ultraTabViable$leagueidx)
# fit model
lmeMod=lmer(NVC~leagueidx+(1|gameid),data=ultraTabViable, na.rm = T)

## Number of observations histograms
# reviewed: [Joe]
# verified: []
ObsHistDat = aggregate(NVC~leagueidx, data = ultraTabViable[!is.na(ultraTabViable$NVC),], FUN = length)
histImg = ggplot(data = ObsHistDat, aes(x=leagueidx, y=NVC)) + geom_bar(stat='identity') 
histImg = histImg + labs(title = "Number of New View Cost Observations by League", x = "League", y = "Count")

ggsave('../figures/NVCRawHist.pdf', width = 7, height = 5, units = c("in"))
