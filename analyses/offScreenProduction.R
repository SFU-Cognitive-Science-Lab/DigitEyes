" Author: Caitlyn McColeman
  Date Created: 18 Sept 2017
Last Edit: Nov 6 2017

Cognitive Science Lab, Simon Fraser University 
Originally Created For: StarTrak 

Reviewed: [Yue Chen, Tyrus Tracey - October 3rd, 2017] 
Modified: [Yue Chen - Oct 6th, 2017;
            Caitlyn McColeman - Nov 4 2017: edited figure; added effect size calculation for overall (not by league)]
Verified: []
Modified: [Yue Chen - Oct 6th, 2017]
Verified: [Rollin Poe, Scott Harrison - Oct 13, 2017] 

Modified: [Caitlyn McColeman - Nov 6 2017]
Re-reviewed: [Tyrus Tracey, Davey Mac - Nov 7th, 2017]

INPUT: requires playerOnOffProduction.csv, which comes from OnScreenDictionaryCompare.m run in MATLAB                     

OUTPUT: 

"

# load required packages
require('ez')
require('ggplot2')
require('lsr')

# allows Quartz to work in windows
if(.Platform$OS.type=="windows") { 
  quartz<-function() windows()
}

# move to this directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

# load the data to be analysed
productionTab = read.table('playerOnOffProduction.csv', header = TRUE, sep = ",")

# subset data by race
zerg = subset(productionTab, PlayerRace == "Zerg", select = c(LeagueNum, OffScreenPercent))
terran = subset(productionTab, PlayerRace == "Terran", select = c(LeagueNum, OffScreenPercent))
protoss = subset(productionTab, PlayerRace == "Protoss", select = c(LeagueNum, OffScreenPercent))

# direct test of novice-expert differences per race
diffBetwenSilverAndMaster_z = wilcox.test(zerg$OffScreenPercent[zerg$LeagueNum == 2],zerg$OffScreenPercent[zerg$LeagueNum == 6])
diffBetwenSilverAndMaster_t = wilcox.test(terran$OffScreenPercent[terran$LeagueNum == 2],terran$OffScreenPercent[terran$LeagueNum == 6])
diffBetwenSilverAndMaster_p = wilcox.test(protoss$OffScreenPercent[protoss$LeagueNum == 2],protoss$OffScreenPercent[protoss$LeagueNum == 6])

# visualize to match violin plots in paper: overall
quartz()

offScreenProdImg = ggplot(productionTab, aes(factor(as.character(LeagueNum)), OffScreenPercent))
offScreenProdImg = offScreenProdImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
offScreenProdImg = offScreenProdImg + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
offScreenProdImg = offScreenProdImg + geom_violin(alpha = .2, fill = "#C0C0C0", colour = "#C0C0C0")
offScreenProdImg = offScreenProdImg + labs(x = "League")
offScreenProdImg = offScreenProdImg + labs(y = "Off Screen Production (%)")
offScreenProdImg = offScreenProdImg + ggtitle("Off Screen Production by League")

ggplot_build(offScreenProdImg)
ggsave('../figures/offScreenProdImg.pdf', width = 7, height = 5, units = c("in"))

# visualize to match violin plots in paper: zerg
quartz()
# zerg
offScreenProdImg_z = ggplot(zerg, aes(factor(as.character(LeagueNum)), OffScreenPercent))
offScreenProdImg_z = offScreenProdImg_z + geom_jitter(height = 0, width = 0.2, alpha = .1)
offScreenProdImg_z = offScreenProdImg_z + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
offScreenProdImg_z = offScreenProdImg_z + geom_violin(alpha = .2, fill = "#fc8d59", colour = "#fc8d59")
offScreenProdImg_z = offScreenProdImg_z + labs(x = "League")
offScreenProdImg_z = offScreenProdImg_z + labs(y = "Off Screen Production (%)")
offScreenProdImg_z = offScreenProdImg_z + ggtitle("Zerg Off Screen Production by League")

ggplot_build(offScreenProdImg_z)
ggsave('../figures/supplementary/offScreenProdImg_z.pdf')

# visualize to match violin plots in paper: terran
quartz()
# terran
offScreenProdImg_t = ggplot(terran, aes(factor(as.character(LeagueNum)), OffScreenPercent))
offScreenProdImg_t = offScreenProdImg_t  + geom_jitter(height = 0, width = 0.2, alpha = .1)
offScreenProdImg_t = offScreenProdImg_t  + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
offScreenProdImg_t = offScreenProdImg_t  + geom_violin(alpha = .2, fill = "#fc8d59", colour = "#fc8d59")
offScreenProdImg_t = offScreenProdImg_t  + labs(x = "League")
offScreenProdImg_t = offScreenProdImg_t  + labs(y = "Off Screen Production (%)")
offScreenProdImg_t = offScreenProdImg_t  + ggtitle("Terran Off Screen Production by League")

ggplot_build(offScreenProdImg_t)
ggsave('../figures/supplementary/offScreenProdImg_t.pdf')

# visualize to match violin plots in paper: protoss
quartz()
# protoss
offScreenProdImg_p = ggplot(protoss, aes(factor(as.character(LeagueNum)), OffScreenPercent))
offScreenProdImg_p = offScreenProdImg_p + geom_jitter(height = 0, width = 0.2, alpha = .1)
offScreenProdImg_p = offScreenProdImg_p + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
offScreenProdImg_p = offScreenProdImg_p + geom_violin(alpha = .2, fill = "#fc8d59", colour = "#fc8d59")
offScreenProdImg_p = offScreenProdImg_p + labs(x = "League")
offScreenProdImg_p = offScreenProdImg_p + labs(y = "Off Screen Production (%)")
offScreenProdImg_p = offScreenProdImg_p + ggtitle("Protoss Off Screen Production by League")

ggplot_build(offScreenProdImg_p)
ggsave('../figures/supplementary/offScreenProdImg_p.pdf')


# omnibus non-parametric tests: is any one group showing a higher percentage of offscreen production than the others?
kresult=kruskal.test(productionTab$OffScreenPercent~productionTab$LeagueNum)

# get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
H = kresult$statistic
k = kresult$parameter +1 
n = length(unique(productionTab$GameID));
etaSq = (H - k + 1)/(n-k)


# direct test of novice-expert differences
diffBetwenSilverAndMaster = wilcox.test(productionTab$OffScreenPercent[productionTab$LeagueNum == 2],productionTab$OffScreenPercent[productionTab$LeagueNum == 6])

# 3. Mark's idea to look at bronze vs. subsequent leagues; based on pairwise test from NVCAnalysis.R. Helpful for more typical learning curve distributions too.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.
bronzeVsLater = data.frame()
for (leagueNum in 2:7){
    # t-test
    pairCompare = t.test(productionTab$OffScreenPercent[productionTab$LeagueNum == 1],productionTab$OffScreenPercent[productionTab$LeagueNum == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # effect size
    bronzeVsLater[leagueNum-1,4] = cohensD(productionTab$OffScreenPercent[productionTab$LeagueNum == 1],productionTab$OffScreenPercent[productionTab$LeagueNum == leagueNum])
}
