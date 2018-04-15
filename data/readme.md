## overview ##

The are the data used for our DigitEyes submission. 

Some required some pre-processing that provided sensitive information about player identity, and in those cases we provide measure-specific .csv files.

### master table ###

The master table contains one row per player and aggregate measures about that players' performance. 
[summary forthcoming]

| gameid               | leagueidx     | in_analysis           | pacactionlatencymean       |MapRCPerMin         |
| -------------        | ------------- |-------------          |-------------               |-------------       |
| identify individual  | skill level   | met inclusion criteria|latency to first PAC action |minimap right clicks|

| mapAtkPerMin         | mapAblPerMin      |
| -------------        | -------------     |
| minimap attacks      | special abilities |


### ultra table ###


|leagueidx    |gameid             |PACidx                       |in_analysis          |
|---------    |---------          |---------                    |---------            |
|skill level  |identify individual|the order of PAC instances   |met inclusion critera|


|FixDuration             |betweenactionlatency   |FixX                               |FixY                               |
|---------               |---------              |---------                          |---------                          |
|length of PAC/fixaiton  |latency between actions|Centroid of fixation, X dimension  |Centroid of fixation, Y dimension  |

[summary forthcoming]

### playerOnOffProduction.csv ###

This file is required by analyses/offScreenProduction.R.

The PlayerID field is a random numeric identifier of each player. It will not have any relationship with similar fields in other tables. Some players are represented more than once in this data set.

