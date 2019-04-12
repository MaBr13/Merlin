%% Cleaning environment
clear
close all
clc

% bird data csv to mat
year=2008;
% first load radar data
Birds=readtable('Birds_rh2008-03-27.csv');
Birds=table2cell(Birds);


Dates = datetime(Birds(:,2));
Day = (datenum([num2str(year),'-01-01 00:00:00'])-datenum([num2str(year),'-01-01 00:00:00']))+(datenum(Dates)-datenum([num2str(year),'-01-01 00:00:00'])+1);
Headings=cell2mat(Birds(:,3));
Airspeed=cell2mat(Birds(:,4));

save('Oneday_925.mat', 'Day', 'Headings','Airspeed')