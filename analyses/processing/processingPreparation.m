%  If the user clones or forks the whole DigitEyes directory, we'll know a
%  local subset of the directory structure. We can rely on this to make
%  some decisions about where to access scripts, save data/figures. 
%
%  This function orients the user to the right part of the directory space
%  and loads the master and ultra tables for use in the processing scripts.
%  
function [masterTable, ultraTable] = processingPreparation
%
%  Author: Caitlyn McColeman
%  Date Created: March 6 2018
%  Last Edit: 
%  
%  Cognitive Science Lab, Simon Fraser University 
%  Originally Created For: DigitEyes
%  
%  Reviewed: [] 
%  Verified: [] 
%  
%  INPUT: 
%  
%  OUTPUT: the data from the lab SQL tables de-identified and ready for analysis. 
%       masterTable, table; One row per game.
%        ultraTable, table; One row per fixation.
%  
%  Additional Scripts Used: 
%  
%  Additional Comments: 

% move to directory containing this file. It'll be easier to get to the
% data directory as we know this part of the directory structure on any
% GitHub fork.
tmp = matlab.desktop.editor.getActive;
cd(fileparts(tmp.Filename));

% change to the data directory and read in the data tables
cd ../../data

masterTable = readtable('masterTable.csv');
ultraTable = readtable('ultraTable.csv');
