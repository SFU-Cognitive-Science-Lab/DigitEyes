% This provides fixMedianNonPAC.txt and fixMedianPAC.txt for analysis, 
% based on user input ("Fix" or "PAC" for the varargin). It calculates
% the median fixation durations for PAC and non-PAC fixations by
% league and player. 

function PACDurationMedians(varargin)

% Authors (Original): Adam Bignell, Michael Knie, Judi Azmand, Ruilin Z.,
%                     and Neda A.
% Refactored (New Author): Alex Volkanov (February 3, 2017)
%
% Date Created: October 6, 2016
% 
% 
% Cognitive Science Lab, Simon Fraser University 
% Originally Created For: DigitEyes
%
% Reviewed: Ruilin Zhang (February 7, 2017)
% Verified: Sebastian M., Ruilin Z., Neda A. (February 7, 2017)
%
% Edited (April 5 2017) by Caitlyn to output data for stats
% Edited (Jan 3 2018) by Caitlyn to eliminate duplicate games via
% in_analysis column.
% Edited (Mar 6 2018) by Caitlyn to improve usability for public facing
% version. [formally pacdurationmedians_new] Pulled out SQL calls, unused
% figures, and replaced with data imports of a .csv I exported externally.
%
% INPUT: "PAC" for Perception Action Cycle duration medians. Otherwise,
%         enter "Fix" for non-PAC fixation duration medians.
%
% OUTPUT: a .txt file that saves to the data directory. It is
%         used for analysis in pacDurationMediansAnalysis.R, where analysis and
%         visualization occur. 
%

isPac = strcmpi('PAC', varargin{1});

% move to directory containing this file. It'll be easier to get to the
% data directory as we know this part of the directory structure on any
% GitHub fork.
tmp = matlab.desktop.editor.getActive;
cd(fileparts(tmp.Filename));

% change to the data directory and read in the data tables
cd ../../data

masterTable = readtable('masterTable.csv');
ultraTable = readtable('ultraTable.csv');

% Bronze League
[mediansOut1, leaguesOut1]= runLeague(1, isPac); % 'Bronze League'

% Silver League
[mediansOut2, leaguesOut2]= runLeague(2, isPac); % 'Silver League' 

% Gold League
[mediansOut3, leaguesOut3]= runLeague(3, isPac); % 'Gold League'

% Platinum League
[mediansOut4, leaguesOut4]= runLeague(4, isPac); % 'Platinum League'

% Diamond League
[mediansOut5, leaguesOut5]= runLeague(5, isPac); % 'Diamond League'

% Master League
[mediansOut6, leaguesOut6]= runLeague(6, isPac); % 'Master League'

% Grand Master League
[mediansOut7, leaguesOut7]= runLeague(7, isPac); % 'Grand Master League'

% prepare major output vectors
grandMediansOut = [mediansOut1; mediansOut2; mediansOut3; mediansOut4; mediansOut5; mediansOut6; mediansOut7];
grandLeaguesOut = [leaguesOut1; leaguesOut2; leaguesOut3; leaguesOut4; leaguesOut5; leaguesOut6; leaguesOut7];

% prepare variables to save in .csv
saveDat = table(grandMediansOut, grandLeaguesOut);

% save as .csv
if isPac
    writetable(saveDat, 'fixMedianPAC');
else
    writetable(saveDat, 'fixMedianNonPAC');
end


    function [mediansOut, leaguesOut]= runLeague(leagueId, isPac) % updated by Caitlyn Apr 5 2017 so we had data output for analysis
        mediansOut = []; % intialize storage vector for data output
        leaguesOut = []; % intialize storage vector for data output
        
        leagueGames = masterTable.gameid(leagueId == masterTable.leagueidx);
        
        medians = []; % initialize storage
        
        for game = leagueGames.'
            
            if (isPac) % Perception Action Cycle Durations
                gameDurations = ultraTable.FixDuration(ultraTable.PACidx == 1 & ultraTable.gameid == game);
            else       % Fixation Durations
                gameDurations = ultraTable.FixDuration(ultraTable.PACidx == 0 & ultraTable.gameid == game);
            end
            
            gameDurations = TimeStampToRealTime(gameDurations)*1000; % convert to milliseconds -Kat & Rollin
            
            % append to existing data
            medians = [medians, median(gameDurations)];
        end
        
        % reshape
        mediansOut = medians.';
        leaguesOut = repmat(leagueId, length(mediansOut), 1);
        
    end
end
