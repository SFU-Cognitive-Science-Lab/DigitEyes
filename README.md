# DigitEyes
A study of visuo-spatial information sampling beyond the eyes

These are the data and materials to recreate analyses for our forthcoming paper. We'll include citation information when we have it, and ask you to please cite our work if you use it. Detailed motivation for these measures and description of our data collection is available in the paper. 

## Overview
We analyse the data from 3307 StarCraft 2 players. In the game, players move their screen around to gather information, much like how we move our eyes around to gather information from our everyday environment. 

We have a collection of measures to capture properties of visuo-spatial information sampling in the game, and find convincing parallels between the players' screen movements and what we already know about eye movements. 

If you'd like to recreate the analyses, we recommend that you clone/download the whole repository. 

## Measures reported

### Fixation Duration
Typically, a fixation is when the eye essentially pauses to extract information from subset of the environment. In StarCraft 2, a fixation is when the screen essentially pauses. Fixation duration is how long that pause lasts. 

#### Screen Fixations
When the player pauses their screen, but takes no action, we call it a screen fixation. 

**Finding:** Fixation durations decrease with skill; better players pause for less time **Data:** fixMedianNonPAC.txt

##### Scripts & Functions: #####
- PACDurationMedians.m: pre-processing to get fixMedianNonPAC.txt 
- pacDurationMediansAnalysis.R: analysis and visualization

#### Perception Action Cycles
When a player pauses the screen, and takes an action, it's called a Perception Action Cycle (or PAC). 

**Finding:** PACs are longer than screen fixations, but here again, better players do them more quickly. **Data:** fixMedianPAC.txt

##### Scripts & Functions:
- PACDurationMedians.m: pre-processing to get fixMedianPAC.txt 
- pacDurationMediansAnalysis.R: analysis and visualization

### Fixation Rate
**Finding:** Better players fixate more often than novices. We find parallels between the eye fixation rates of participants in category learning task where later in learning, people make more eye movements per minute. 

**Data:** EyeTrackFixRate.csv

##### Scripts & Functions:
- FixationRateAnalysis.R

### Saccade Amplitude
**Finding:** With experience, better players' fixations are further apart than novices. **Data:** saccadeAmplitude.csv

##### Scripts & Functions:
- saccadeAmplitude.R: analysis and visualization

### First Action Latency
In a PAC, players can make one or more actions. The time it takes them to make the first action is the First Action Latency.

**Finding:** Better players make their first action more quickly than novices. **Data:** masterTable.csv
##### Scripts & Functions:
- FALAnalysis.R

### Between Action Latency
In a PAC, players can make one or more actions. In PACs with >1 action, we calculate the Between Action Latency, the average time it takes to take an action after the first action.

**Finding:**
Better players perform actions more quickly than novices. Their average duration difference between actions is lower as skill improves. **Data:**
ultraTable.csv

##### Scripts & Functions:
- betweenActionLatencyAnalysis.R

### New View Cost
In a PAC, players can make one or more actions. In PACs with >1 we can compare the latency of the first action versus the latency of the remanining actions to discern whether moving a new part of the environment impacts how quickly a player can act. The New View Cost is a measure that subtracts the Between Action Latency from the First Action Latency.

**Finding:** 
Better players have less new view cost that novices. The difference between group is not noticable until relatively late, which is an interesting departure from most learning curves where most change occurs early in skill development and diminishing returns are observed thereafter. **Data:** 
NVC.csv

##### Scripts & Functions:
NVCAnalysis.R: visualization and analysis

### Hotkey Use
There are multiple ways to select a unit/building to use it. Usually the most efficient way to select something is to set it to a hotkey/shortcut so rather than having to search for the target, it is automatically selected using the interface. 

**Finding:** 
Better players are more likely to select a unit using a hotkey. **Data:** 
hkVSSel.csv

##### Scripts & Functions:
- mapAbilitiesAnalysis.R: analysis and visualization  

### Minimap Use
Another way to use the interface efficiently is to use the mini-map (a preview space) to gather information and take actions instead of using the more detailed, but slower, screen map move. We have three different measures that capture the role that using the mini map has in making efficient use of the interface. 

#### Special Abilities
Some units are afforded special abilities to impact the play environment or cast spells on opponents. They can be enacted by using the minimap. 

**Finding:**
There is no apparent difference between skill levels in players' use of special abilities by minimap. **Data:**
mastertable.csv

##### Scripts & Functions:
- mapAbilitiesAnalysis.R: analysis and visualization  

#### Attacks
Players can direct their units to attack using the minimap. 

**Finding:**
Better players use minimap attacks more often.  **Data:**
mastertable.csv

##### Scripts & Functions:
- mapAbilitiesAnalysis.R: analysis and visualization  

#### Right Clicks
Players can move their units with right clicks to the minimaps. This is a more efficient way to move, albeit less precise. 
**Finding:** Better players use right clicks to the minimap more than novices. 
**Data:** mastertable.csv
##### Scripts & Functions:
- mapAbilitiesAnalysis.R: analysis and visualization  

### Offscreen Production
Other way to efficiently leverage the play environment is to produce units in buildings without actually fixating that building. When players train units without looking, we call it Offscreen Production. 

**Finding:** Better players produce more without looking at the production site than novices. 
**Data:** onOffProduction.csv
##### Scripts & Functions:
- offScreenProduction.R
