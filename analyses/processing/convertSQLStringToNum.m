%  [Insert a description of the script's intended function here] 
%  
function numOut = convertSQLStringToNum(veiledNumberArray)
%
%  Author: Caitlyn McColeman
%  Date Created: Mar 6 2018
%  Last Edit: 
%  
%  Cognitive Science Lab, Simon Fraser University 
%  Originally Created For: DigitEyes
%  
%  Reviewed: [] 
%  Verified: [] 
%  
%  INPUT: a cell array of numbers in string's clothing; e.g. {'2', '1', '5'}
%  
%  OUTPUT: a vector of numbers
%  
%  Additional Scripts Used: 
%  
%  Additional Comments: heavily inspired by: https://www.mathworks.com/matlabcentral/answers/43244-selecting-only-the-numbers-from-a-string-variable

B = regexp(veiledNumberArray,'\d*','Match');
for ii= 1:length(B)
  if ~isempty(B{ii})
      numOut(ii,1)=str2double(B{ii}(end));
  else
      numOut(ii,1)=NaN;
  end 
end