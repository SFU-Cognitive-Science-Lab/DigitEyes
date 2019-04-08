% sample code

% assumes we're starting in the DigitEyes directory in the "analyses"
% folder.

% add data directory to path
addpath(genpath('../data'))

% unzip ultraTable (had to be compressed to fit on GitHub) 
unzip('../data/ultraTable.csv.zip')

% load ultra table data
ultraTab = readtable('ultraTable.csv');

% rename variables to preserve remaining General_Output_RVT.m code
     gameid_ultra = ultraTab.gameid;
  leagueidx_ultra = ultraTab.leagueidx;
  PACEndTimestamp = ultraTab.PACEndTimestamp;
PACStartTimestamp = ultraTab.PACStartTimestamp;
        row_names = ultraTab.row_names;
             Fixx = ultraTab.Fixx;
             Fixy = ultraTab.Fixy;
             
% rather than using master table, get the list of players to iterate
% through using game id from ultra table 
player = unique(ultraTab.gameid);