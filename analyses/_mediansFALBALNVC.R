
" Author: Caitlyn McColeman
Date Created: July 31 2018 
Last Edit:

Cognitive Science Lab, Simon Fraser University 
Originally Created For: DigitEyes' supplementary material

Reviewed: 
Verified: 

INPUT:                     

OUTPUT: 

This function expects that it exists in a folder called 'analysis' in the DigitEyes directory and that there is another folder containing data. 
Recommended use: download or clone the repository from our GitHub page, and run by setting the working directory to DigitEyes/analysis.
"

require('ez')
require('lsr')
library('ggplot2')

# allows Quartz to work in windows
if(.Platform$OS.type=="windows") { 
  quartz<-function() windows()
}

# Move to this directory, & use it as a reference point for finding the data folder.
this.dir <- dirname(parent.frame(2)$ofile)

setwd(this.dir)

# Move up and into data folder.
setwd('../data/') 

ultraTab = read.table('ultraTable.csv', header = TRUE, sep = ",")
# eliminate duplicates by excluding in_analysis = 0
ultraTab = ultraTab[ultraTab$in_analysis == 1,]

# prepare critical data
FAL = (as.numeric(as.character(ultraTab$ActionLatency))/88.5347)*1000 # convert from timestamps to milliseconds
BAL = (as.numeric(as.character(ultraTab$betweenactionlatency))/88.5347)*1000 # convert from timestamps to milliseconds
AllLeagueRec = ultraTab$leagueidx
subrec = ultraTab$gameid

dataStorage = cbind(AllLeagueRec, FAL, BAL, subrec)
   FALTable = as.data.frame(dataStorage)

# Eliminate missing values from first action latency, between action latency
noNaNFAL = FALTable[complete.cases(FALTable$FAL),]  # if it's not a PAC, then there will be no first action latency
noNaNBAL = FALTable[complete.cases(FALTable$BAL),]  # if it's not a PAC with 3+ actions then there will be no BAL

noNaNFAL = noNaNFAL[is.finite(FALTable$FAL),] 
noNaNBAL = noNaNBAL[is.finite(FALTable$BAL),] 

# calculate new view cost
noNaNNVC = cbind(noNaNBAL, noNaNBAL$FAL - noNaNBAL$BAL)
names(noNaNNVC)<-c('AllLeagueRec', 'FAL', 'BAL', 'subrec', 'NVC')

# ensure categorical variables are treated as factors
noNaNFAL$AllLeagueRec = factor(noNaNFAL$AllLeagueRec)
noNaNFAL$subrec = factor(noNaNFAL$subrec) 

# the ultra table contains one row per PAC. Aggregate to get the median per player for their whole game.
FALmedians = aggregate(FAL ~ AllLeagueRec * subrec,
                              data=noNaNFAL, FUN=median)

BALmedians = aggregate(BAL ~ AllLeagueRec * subrec,
                       data=noNaNBAL, FUN=median)

NVCmedians = aggregate(NVC ~ AllLeagueRec * subrec,
                       data=noNaNNVC, FUN=median)

# create a function for analysis and visualization of the three measures
visualAndResults <- function(mdnDataFrame, measureName) {
  # mdnDataFrame = the aggregated data frame that we wish to plot from
  # measureName  = the name of the column the data frame that is the dependent variable 
  quartz()
  
  Img = ggplot(mdnDataFrame, aes(as.factor(AllLeagueRec),mdnDataFrame[[measureName]]))
  Img = Img + geom_jitter(height = 0, width = 0.2, alpha = .1)
  Img = Img + theme_bw() + theme(text = element_text(size=25), panel.grid.major = element_blank(), plot.title = element_text(size=25)) 
  Img = Img + geom_violin(alpha = .15, fill = "#C0C0C0", colour = "#C0C0C0")
  Img = Img + labs(x = "League")
  Img = Img + labs(y = measureName)
  Img = Img + ggtitle(paste(measureName, ' by League', sep = ''))
  
  print(Img)
  
  dependentVar = mdnDataFrame[[measureName]]

  # Kruskal-Wallis for easier comparision between measures in StarTrak & to be safe with parametric test assumptions
  kresult=kruskal.test(dependentVar~mdnDataFrame$AllLeagueRec)
  
  # Now get effect size, as per TOMCZAK & TOMCZAK (2014) http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
  H = kresult$statistic
  k = kresult$parameter + 1
  n = length(unique(noNaNFAL$subrec));
  etaSq = (H - k + 1)/(n-k)
  
  # 2c. Determine the difference between the opposite ends of the possible leagues.
  diffBetwenSilverAndMaster = wilcox.test(dependentVar[mdnDataFrame$AllLeagueRec == 2],dependentVar[mdnDataFrame$AllLeagueRec == 6])
  
  # 2d. Our idea to look at bronze vs. subsequent leagues; based on a pairwise test from NVCAnalysis.R was helpful for more typical learning curve distributions.
  # For example we use a family-wise error correction of .05/6 = 0.008 to reject the null hypothesis, being that the two samples are drawn from the same population.
  bronzeVsLater = data.frame()
  for (leagueNum in 2:7)
  {
    # T-test calculated here.
    pairCompare = t.test(dependentVar[mdnDataFrame$AllLeagueRec == 1],dependentVar[mdnDataFrame$AllLeagueRec == leagueNum])
    
    bronzeVsLater[leagueNum-1,1]=pairCompare$statistic
    bronzeVsLater[leagueNum-1,2]=pairCompare$parameter
    bronzeVsLater[leagueNum-1,3]=pairCompare$p.value
    
    # T-test effect size 
    bronzeVsLater[leagueNum-1,4] = cohensD(dependentVar[mdnDataFrame$AllLeagueRec == 1],dependentVar[mdnDataFrame$AllLeagueRec == leagueNum])
  }
  
  # save image
  ggsave(paste('../figures/', measureName, sep = ""), width = 7, height = 5, units = c("in"))
  
  # output
  return(list(mdnDataFrame, kresult, pairCompare, bronzeVsLater, diffBetwenSilverAndMaster, Img))
}


# call function for each of the three measures (BAL, FAL, NVC)
FALOut = visualAndResults(FALmedians, 'FAL')
BALOut = visualAndResults(BALmedians, 'BAL')
NVCOut = visualAndResults(NVCmedians, 'NVC')
