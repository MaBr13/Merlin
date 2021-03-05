%% Cleaning environment
clear
close all
clc

% bird data csv to mat
% first load radar data
Birds=readtable('Birds_hr2010-03-26.csv');
date=table2array(Birds(1,2));
Dates = datetime(table2array(Birds(:,3)));
year=year(table2array(Birds(1,2)));
Birds=table2cell(Birds);


Day = (datenum([num2str(year),'-01-01 00:00:00'])-datenum([num2str(year),'-01-01 00:00:00']))+(datenum(Dates)-datenum([num2str(year),'-01-01 00:00:00'])+1);
Headings=cell2mat(Birds(:,12));
Airspeed=cell2mat(Birds(:,10));

save(['One_day_' datestr(date,'yyyymmdd'),'.mat'], 'Day', 'Headings','Airspeed')