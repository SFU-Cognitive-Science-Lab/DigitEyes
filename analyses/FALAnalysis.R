" Author: Judi Azmand, Romanos Byliris, YuYing Mak
  Date Created: 5 April 2017 
  Last Edit: 5 April 2017
  
  Cognitive Science Lab, Simon Fraser University 
  Originally Created For: [StarTrak] 
  
  Reviewed: [Ximin & Joe] 
  Verified: [Alex & Neda] Nov.15/17
  
  INPUT:                     
  
  OUTPUT: 
  
  a good reference for implementing high level ANOVA ideas in R code
 http://www.unh.edu/halelab/BIOL933/labs/lab5.pdf
 
  Additional Comments: 
         Changelog 
            
            Nov 4 2017 - added in effect size, for loop; edited figures for greyscale production C.M.
            Jan 8 2017 - removed duplicate games C.M.
            Mar 7 2017 - removed SQL calls in preparation to share 
"

require('ez')
require('lsr')
library('ggplot2')

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

masterTable = read.table('masterTable.csv', header = TRUE, sep = ",")
 
             FAL = masterTable$pacactionlatencymean/88.5347*1000
AllLeagueRec_FAL = masterTable$leagueidx
          subrec = masterTable$gameid

FALTable = cbind(AllLeagueRec_FAL, FAL, subrec)

FALTable = as.data.frame(FALTable)

noNaNFAL = FALTable[complete.cases(FALTable$FAL),] # eliminate missing values from first action latency

noNaNFAL = noNaNFAL[is.finite(FALTable$FAL),] 

noNaNFAL$AllLeagueRec_FAL = factor(noNaNFAL$AllLeagueRec_FAL)
          noNaNFAL$subrec = factor(noNaNFAL$subrec) 

# run the anova to prep for looking into assumptions
rt_anova = ezANOVA(
  noNaNFAL
  , dv = FAL
  , wid = subrec
  , between = AllLeagueRec_FAL
  , type = 3
  , return_aov = T
)

# 1 . normality test
stFAL = shapiro.test(residuals(rt_anova$aov))

# 1b. normality fails; drop grandmaster and repeat analysis with only sufficiently large groups
league7Idx = noNaNFAL$AllLeagueRec_FAL == 7

noGMFAL = noNaNFAL[!league7Idx,]

# 1c. run ANOVA without small group (grandmaster)
FALTest = ezANOVA(
  noGMFAL
  , dv = FAL
  , wid = subrec
  , between = AllLeagueRec_FAL
  , type = 3
  , return_aov = T
)


# look at histogram of residuals
hist(residuals(FALTest$aov))


quartz()


FALImg = ggplot(noNaNFAL, aes(AllLeagueRec_FAL,FAL))
FALImg = FALImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
FALImg = FALImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
FALImg = FALImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
FALImg = FALImg + labs(x = "League")
FALImg = FALImg + labs(y = "First Action Latency (ms)")
FALImg = FALImg + ggtitle('First Action Latency by League')

setwd('../figures/') # move up and into figures folder

ggsave("FALImg.pdf", width = 7, height = 5, units = c("in"))

# noGMScoutingTestLog still fails Levene's test; ANOVA is not viable
diffBetwenSilverAndMaster = wilcox.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 2],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 6])

# 2 Go with Kruskal-Wallis for easier comparision between measures in StarTrak and to be safer about parametric test assumptions. Run a non-parametric Kruskal Wallis test
kresult=kruskal.test(noNaNFAL$FAL~noNaNFAL$AllLeagueRec_FAL)

# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(noNaNFAL$subrec));
etaSq = (H - k + 1)/(n-k)

# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 2],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 6])

# 3. mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7)
  {
    # t-test
    pairCompare = t.test(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 1],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == 1],noNaNFAL$FAL[noNaNFAL$AllLeagueRec_FAL == leagueNum])
}




