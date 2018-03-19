" Author: Caitlyn McColeman
  Date Created: June 2017 
Last Edit: 

Cognitive Science Lab, Simon Fraser University 
Originally Created For: StarTrak

Reviewed: [Kayla Nathan] 
Verified: [] 

INPUT:                     

OUTPUT: 

Additional Comments: 
Changelog

 Nov 4 2017 - effect size calculations
 Jan 3 2018 - duplicate removal
 Mar 7 2018 - removing SQL calls, making shareable 
"

require('ggplot2')
require('lsr')

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

UltraTable = read.table('ultraTable.csv', header = TRUE, sep = ",")

# make factor easier analysis
UltraTable$leagueidx  = factor(UltraTable$leagueidx)

CompleteUltraTable = UltraTable[complete.cases(UltraTable$betweenactionlatency),]

meansByLeague = aggregate(as.numeric(CompleteUltraTable$betweenactionlatency), by = list(CompleteUltraTable$gameid, CompleteUltraTable$leagueidx), FUN=mean, na.rm=TRUE)
colnames(meansByLeague)=c('player', 'league', 'BAL')

# visualize
balImg = ggplot(meansByLeague, aes(league,BAL))
balImg = balImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
balImg = balImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
balImg = balImg + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
balImg = balImg + labs(x = "League")
balImg = balImg + labs(y = "BAL (ms)")
balImg = balImg + ggtitle('Between Action Latency by League')


setwd('../figures/') # move up and into figures folder

ggsave("BALImg.pdf", width = 7, height = 5, units = c("in"))

# run non-parametric alternative to one-way ANOVA to see if there's a difference between groups
kresult=kruskal.test(meansByLeague$BAL~meansByLeague$league)

# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter + 1
n = length(unique(meansByLeague$player));
etaSq = (H - k + 1)/(n-k)

# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league
diffBetwenSilverAndMaster = wilcox.test(meansByLeague$BAL[meansByLeague$league == 2],meansByLeague$BAL[meansByLeague$league == 6])

# 3. mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(meansByLeague$BAL[meansByLeague$league == 1],meansByLeague$BAL[meansByLeague$league == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(meansByLeague$BAL[meansByLeague$league == 1],meansByLeague$BAL[meansByLeague$league == leagueNum])
}
