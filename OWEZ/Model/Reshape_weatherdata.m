%% Cleaning environment
clear
close all
clc

% weatherdata csv to mat
year=2008;
% first load .csv 
MeteoData2008=readtable('march_2008.csv');
MeteoData2008=table2cell(MeteoData2008);

lat=length(unique(cell2mat(MeteoData2008(:,2))));
long=length(unique(cell2mat(MeteoData2008(:,3))));

Dates = datetime(MeteoData2008(:,1), 'Format', 'yyyy-MM-dd HH:mm:ss');
Days = datenum(Dates)-datenum([num2str(year),'-01-01 00:00:00'])+1;
MTime = reshape(Days,long,lat,[]);
Mlat = cell2mat(reshape(MeteoData2008(:,2),long,lat,[]));
Mlong = cell2mat(reshape(MeteoData2008(:,3),long,lat,[]));
Mu = str2double(reshape(MeteoData2008(:,4),long,lat,[]));
Mv = str2double(reshape(MeteoData2008(:,5),long,lat,[]));


save('MeteoMatrix2008_925_reshape.mat', 'MTime', 'Mlong','Mlat', 'Mu', 'Mv')

MTime = rot90(MTime);
Mlat = rot90(Mlat);
Mlong = rot90(Mlong);
Mu = rot90(Mu);
Mv = rot90(Mv);

save('MeteoMatrix2008_925.mat', 'MTime', 'Mlong','Mlat', 'Mu', 'Mv')