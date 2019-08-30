" Author: Caitlyn McColeman
  Date Created: June 2017 
Last Edit: 

Cognitive Science Lab, Simon Fraser University 
Originally Created For: StarTrak

Reviewed: [Kayla & Nathan] 
Verified: [Tyrus Tracey] 

  

INPUT: ultraTable.csv                    

OUTPUT: Plots

Additional Comments: 
Changelog

 Nov 4 2017 - effect size calculations
 Jan 3 2018 - duplicate removal
 Mar 7 2018 - removing SQL calls, making shareable 
"

require('ggplot2')
require('lsr')

# Move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

unzip('../data/ultraTable.csv.zip', 'ultraTable.csv', exdir = '../data')

UltraTable = read.table('../data/ultraTable.csv', header = TRUE, sep = ",")

# Make factor easier analysis
UltraTable$leagueidx  = factor(UltraTable$leagueidx)
UltraTable$BetweenActionLatency = as.numeric(as.character(UltraTable$BetweenActionLatency))/88.5347*1000 # convert to ms 

CompleteUltraTable = UltraTable[!is.na(UltraTable$BetweenActionLatency),]
CompleteUltraTable = CompleteUltraTable[CompleteUltraTable$in_analysis == 1,]

meansByLeague = aggregate(as.numeric(as.character(CompleteUltraTable$BetweenActionLatency)), by = list(CompleteUltraTable$gameid, CompleteUltraTable$leagueidx), FUN=mean, na.rm=TRUE)
colnames(meansByLeague)=c('player', 'league', 'BAL')

# Visualize
balImg = ggplot(meansByLeague, aes(league,BAL))
balImg = balImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
balImg = balImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
balImg = balImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
balImg = balImg + labs(x = "League")
balImg = balImg + labs(y = "BAL (ms)")
balImg = balImg + ggtitle('Between Action Latency by League')


setwd('../figures/') # Move up and into figures folder

ggsave("BALImg.pdf", width = 7, height = 5, units = c("in"))



# Run non-parametric alternative to one-way ANOVA to see if there's a difference between groups
kresult=kruskal.test(meansByLeague$BAL~meansByLeague$league)

# Get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(meansByLeague$player));
etaSq = (H - k + 1)/(n-k)

# 2b. Determine the difference between the "novice-ish" (League 2 - Silver) and the "expert-ish" (League 6 - Master) toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(meansByLeague$BAL[meansByLeague$league == 2],meansByLeague$BAL[meansByLeague$league == 6])

# 3. Look at Bronze vs. subsequent leagues
# Note: Used a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(meansByLeague$BAL[meansByLeague$league == 1],meansByLeague$BAL[meansByLeague$league == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # Effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(meansByLeague$BAL[meansByLeague$league == 1],meansByLeague$BAL[meansByLeague$league == leagueNum])
}

## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: [Joe]
# verified: [Tyrus]
ggplot(data = meansByLeague) + geom_histogram(aes(x = meansByLeague$league), stat="count") + labs(title = "Number of Observations in Analysis: BAL") + 
  labs(x="League", y="Count")

ggsave('../figures/BALHist.pdf', width = 7, height = 5, units = c("in"))

# number of observations in the raw data 

ggplot(data = CompleteUltraTable) + geom_histogram(aes(x = CompleteUltraTable$leagueidx), stat="count") + labs(title = "Number of Observations in Dataset: BAL") + 
  labs(x="League", y="Count")
# league seven has 12462 observations 
# >summary(CompleteUltraTable)

ggsave('../figures/BALRawHist.pdf', width = 7, height = 5, units = c("in"))

## LMER
# reviewed: [Joe]
# verified: [Jordan]
require('lme4')
# read data

# specify data class
CompleteUltraTable$BetweenActionLatency = as.numeric(as.character(CompleteUltraTable$BetweenActionLatency))
CompleteUltraTable$leagueidx = as.factor(CompleteUltraTable$leagueidx)

# fit model
lmeBaseMod=lmer(BetweenActionLatency~(1|gameid),data=CompleteUltraTable)
lmeLeagueMod=lmer(BetweenActionLatency~leagueidx+(1|gameid),data=CompleteUltraTable)

anova(lmeBaseMod, lmeLeagueMod)

## Number of observations histograms
# reviewed: [Joe]
# verified: [Jordan]
ObsHistDat = aggregate(BetweenActionLatency~leagueidx, data = CompleteUltraTable, FUN = length)
histImg = ggplot(data = ObsHistDat, aes(x=leagueidx, y=BetweenActionLatency)) + geom_bar(stat='identity') 
histImg = histImg + labs(title = "Number of Between Action Latency Observations by League", x = "League", y = "Count")

## They'll show up individually in RStudio but the first one gets overwritten when you look from the figures folder
ggsave('../figures/BALRawHist.pdf', width = 7, height = 5, units = c("in"))

### Bootstrap Tukey

# reviewed by Caitlyn [Jun 19 2019]

set.seed(1) # to make sampling reproducible

library(dplyr)

#drop league 7
ToRemove=which(CompleteUltraTable$leagueidx=='7')
CompleteUltraTable=CompleteUltraTable[-ToRemove,]
CompleteUltraTable$leagueidx=droplevels(CompleteUltraTable$leagueidx)

#set number of samples
Replications=50
#initialize
output=matrix(, nrow = 15, ncol = Replications)

for (i in 1:Replications){
  
  #subsample
  new_data <- CompleteUltraTable %>% group_by(leagueidx) %>% sample_n(167, replace=FALSE)
  #aov and tukey
  Anova=aov(new_data$BetweenActionLatency ~new_data$leagueidx)
  TUKEY=TukeyHSD(Anova)$`new_data$leagueidx`[,4]
  
  #record pvalues
  output[1:length(TUKEY),i]=TUKEY
}

#mean pvalues, and produce output
RowMEANZ=rowMeans(output)
FinalOutput=data.frame(as.factor(row.names(TukeyHSD(Anova)$`new_data$leagueidx`)),RowMEANZ)
names(FinalOutput)=c('Comparison','Mean PValue')
FinalOutput
