" Author: Caitlyn McColeman
  Date Created: April 2017 
  Last Edit: 

Cognitive Science Lab, Simon Fraser University 
Originally Created For: StarTrak

Reviewed: [Kayla and Nathan 11/7/17] 
Verified: [Yue Chen 21/7/17] 

INPUT:                     

OUTPUT: 

Changes
    August 17 17  - updated violin plots for internal consistency on the startrak paper -CM
    November 4 17 - added in effect size calculation, edited figures for greyscale production, function for easier operation -CM
    January 11 18 - little tweaks for image sizes
"


require('ez')
require('ggplot2')

# allows Quartz to work in windows
if(.Platform$OS.type=="windows") { 
  quartz<-function() windows()
}

# move to this directory, and use it as a reference point to find the data folder

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

setwd('../data/') # move up and into data folder

MasterTable = read.table('masterTable_backup.csv', header = TRUE, sep = ",")

# hotkey vs. select

HKVsSel = read.table('hkVSSel.csv', header = TRUE, sep = ",")

CompleteMasterTable = MasterTable[complete.cases(MasterTable$MapRCPerMin),]

CompleteMasterTable = CompleteMasterTable[is.finite(CompleteMasterTable$MapRCPerMin),]

subRec = 1:length(CompleteMasterTable$MapRCPerMin)
CompleteMasterTable = cbind(CompleteMasterTable, subRec)

CompleteMasterTable$LeagueIdx = factor(CompleteMasterTable$leagueidx)
CompleteMasterTable$subRec = factor(CompleteMasterTable$subRec)


# map right clicks

quartz()

mapRCImg = ggplot(CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),], aes(LeagueIdx,MapRCPerMin))
mapRCImg = mapRCImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
mapRCImg = mapRCImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
mapRCImg = mapRCImg + geom_violin(alpha = .2, fill = "#C0C0C0", colour = "#C0C0C0")
mapRCImg = mapRCImg + labs(x = "League")
mapRCImg = mapRCImg + labs(y = "Right Clicks per Minute")
mapRCImg = mapRCImg + ggtitle("Mini-Map Right Clicks")

ggplot_build(mapRCImg)
ggsave('../figures/mapRC.pdf', width = 7, height = 5, units = c("in"))

## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: [Robin]
# verified: []
ggplot(data = CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),]) + geom_histogram(aes(x = CompleteMasterTable$LeagueIdx), stat="count") + labs(title = "Number of Observations in Analysis: Map Right Clicks Per Minute") + 
  labs(x="League", y="Count")

ggsave('../figures/MapAttacksHist.pdf', width = 7, height = 5, units = c("in"))

# map attacks

quartz()

mapAtkImg = ggplot(CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),], aes(LeagueIdx,MapAtkPerMin))
mapAtkImg = mapAtkImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
mapAtkImg = mapAtkImg + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
mapAtkImg = mapAtkImg + geom_violin(alpha = .2, fill = "#C0C0C0", colour = "#C0C0C0")
mapAtkImg = mapAtkImg + labs(x = "League")
mapAtkImg = mapAtkImg + labs(y = "Attacks Per Minute")
mapAtkImg = mapAtkImg + ggtitle("Mini-Map Attack Actions")

ggplot_build(mapAtkImg)
ggsave('../figures/mapAtk.pdf', width = 7, height = 5, units = c("in"))

## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: [Robin]
# verified: []
ggplot(data = CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),]) + geom_histogram(aes(x = CompleteMasterTable$LeagueIdx), stat="count") + labs(title = "Number of Observations in Analysis: Map Attacks Per Minute") + 
  labs(x="League", y="Count")

ggsave('../figures/MapAttacksHist.pdf', width = 7, height = 5, units = c("in"))

# map abilities

quartz()

mapAbilImg = ggplot(CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),], aes(LeagueIdx,MapAblPerMin))
mapAbilImg = mapAbilImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
mapAbilImg = mapAbilImg + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
mapAbilImg = mapAbilImg + geom_violin(alpha = .2, fill = "#C0C0C0", colour = "#C0C0C0")
mapAbilImg = mapAbilImg + labs(x = "League")
mapAbilImg = mapAbilImg + labs(y = "Ability Actions Per Minute")
mapAbilImg = mapAbilImg + ggtitle("Mini-Map Ability Actions")

ggplot_build(mapAbilImg)
ggsave('../figures/mapAbility.pdf', width = 7, height = 5, units = c("in"))

## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: [Robin]
# verified: []
ggplot(data = CompleteMasterTable[!(CompleteMasterTable$LeagueIdx == 0),]) + geom_histogram(aes(x = CompleteMasterTable$LeagueIdx), stat="count") + labs(title = "Number of Observations in Analysis: Map Abilities Per Minute") + 
  labs(x="League", y="Count")

ggsave('../figures/MapAbilityHist.pdf', width = 7, height = 5, units = c("in"))

# hot key vs select

# [] updated from Joe's discovery about dropped selects
hkSel = read.table('../data/hkSelectCounts.csv', sep = ',', header = T)
sels = read.table('../data/selectCounts.csv', sep = ',', header = T)

HKVsSel = merge(hkSel, sels, by=c("gameid", "gameid"));

HKVsSel$ratioRec = HKVsSel$HKselCount / HKVsSel$selCount

quartz()

HKVsSel$LeagueIdx.x <- as.factor(HKVsSel$LeagueIdx.x)

hkRatioImg = ggplot(HKVsSel[!(HKVsSel$LeagueIdx.x == 0),], aes(factor(LeagueIdx.x),ratioRec))
hkRatioImg = hkRatioImg + geom_jitter(height = 0, width = 0.2, alpha = .1)
hkRatioImg = hkRatioImg + theme_bw() + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
hkRatioImg = hkRatioImg + geom_violin(alpha = .2, fill = "#C0C0C0", colour = "#C0C0C0")
hkRatioImg = hkRatioImg + labs(x = "League")
hkRatioImg = hkRatioImg + labs(y = "Hot Keys:Select Ratio")
hkRatioImg = hkRatioImg + ggtitle("Hot Key:Select Action Types by League")

ggplot_build(hkRatioImg)
ggsave('../figures/hkToSelect.pdf', width = 7, height = 5.5, units = c("in")) # making this a little taller for the title to fit

## in response to reviewer request, a histogram of the number of observations that went into analysis.
# reviewed: [Robin]
# verified: []
ggplot(data = HKVsSel) + geom_histogram(aes(x = HKVsSel$leagueIdx.x), stat="Count") + labs(title = "Number of Observations in Analysis: HotKey:Select Ratio") + 
  labs(x="League", y="Count")

ggsave('../figures/HKSelHist.pdf', width = 7, height = 5, units = c("in"))
write.csv(HKVsSel, file = "../data/hkVSSel.csv")

# run non-parametric alternative to one-way ANOVA to see if there's any difference between groups

MapRCResult=kruskal.test(CompleteMasterTable$MapRCPerMin~CompleteMasterTable$LeagueIdx)
MapAtkResult=kruskal.test(CompleteMasterTable$MapAtkPerMin~CompleteMasterTable$LeagueIdx)
MapAblResult=kruskal.test(CompleteMasterTable$MapAblPerMin~CompleteMasterTable$LeagueIdx)
HKVsSelResult=kruskal.test(HKVsSel$ratioRec~HKVsSel$LeagueIdx.x)

# get effect size, as per TOMCZAK & TOMCZAK (2014). Reference: http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf

HRC = MapRCResult$statistic # minimap right clicks
kRC = MapRCResult$parameter + 1
nRC = length(unique(CompleteMasterTable$gameid));
etaSqRC = (HRC - kRC + 1)/(nRC-kRC)

HAtk = MapAtkResult$statistic  # minimap attacks
kAtk = MapAtkResult$parameter + 1
nAtk = length(unique(CompleteMasterTable$gameid));
etaSqAtk = (HAtk - kAtk + 1)/(nAtk-kAtk)

HAbl = MapAblResult$statistic  # minimap special abilities
kAbl = MapAblResult$parameter + 1
nAbl = length(unique(CompleteMasterTable$gameid));
etaSqAbl = (HAbl - kAbl + 1)/(nAbl-kAbl)

HHKSel = HKVsSelResult$statistic # hotkey:select ratio
kHKSel = HKVsSelResult$parameter + 1
nHKSel = length(unique(HKVsSel$gameid));
etaSqHKSel = (HHKSel - kHKSel + 1)/(nHKSel-kHKSel)


# 2b. Determine the difference between the "novice-ish" and the "expert-ish" toward the opposite end of the possible league

silverAndMasterRC = wilcox.test(CompleteMasterTable$MapRCPerMin[CompleteMasterTable$LeagueIdx == 2],CompleteMasterTable$MapRCPerMin[CompleteMasterTable$LeagueIdx == 6])

silverAndMasterAtk = wilcox.test(CompleteMasterTable$MapAtkPerMin[CompleteMasterTable$LeagueIdx == 2],CompleteMasterTable$MapAtkPerMin[CompleteMasterTable$LeagueIdx == 6])

silverAndMasterAbl = wilcox.test(CompleteMasterTable$MapAblPerMin[CompleteMasterTable$LeagueIdx == 2],CompleteMasterTable$MapAblPerMin[CompleteMasterTable$LeagueIdx == 6])

silverAndMasterHKvS = wilcox.test(HKVsSel$ratioRec[HKVsSel$LeagueIdx.x == 2], HKVsSel$ratioRec[HKVsSel$LeagueIdx.x == 6])

# 3. Look at bronze vs. subsequent leagues; helpful for more typical learning curve distributions as well.

effectSizeTTest <- function(measuredVariable,leagueVariable) {
    bronzeVsLater = data.frame()
    for (leagueNum in 2:7){
        # t-test
        pairCompare = t.test(measuredVariable[leagueVariable == 1],measuredVariable[leagueVariable == leagueNum])
        
        bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
        bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
        bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
        
        # effect size
        bronzeVsLater[leagueNum-1,4] = cohensD(measuredVariable[leagueVariable == 1],measuredVariable[leagueVariable == leagueNum])

    }
return(bronzeVsLater)
}

# get the tests of later leagues v. bronze.
# note: we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis that the two samples are drawn from the same population.

effectByLeagueRC = effectSizeTTest(CompleteMasterTable$MapRCPerMin,CompleteMasterTable$LeagueIdx)

effectByLeagueAtk = effectSizeTTest(CompleteMasterTable$MapAtkPerMin,CompleteMasterTable$LeagueIdx)

effectByLeagueAbl = effectSizeTTest(CompleteMasterTable$MapAblPerMin,CompleteMasterTable$LeagueIdx)

effectByLeagueHKvS = effectSizeTTest(HKVsSel$ratioRec,HKVsSel$LeagueIdx.x)
