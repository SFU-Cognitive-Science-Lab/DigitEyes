%  In the event that data is parsed in a table/cell array as a set of
%  strings rather than digits, as we intend, this quick function will
%  convert a cell array of strings to a vector of numbers.
%
%  The function may also be a way to strip out digits from a mixed set of
%  strings and numbers to save only the digits in vector form.
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

% use regular expression to get digits
B = regexp(veiledNumberArray,'\d*','Match');
for ii= 1:length(B)
  if ~isempty(B{ii})
      % extract the "ii"th digit, save it in the output vector
      numOut(ii,1)=str2double(B{ii}(end));
  else
      % there is no "ii"th digit. Set the output for this row to NaN.
      numOut(ii,1)=NaN;
  end 
end