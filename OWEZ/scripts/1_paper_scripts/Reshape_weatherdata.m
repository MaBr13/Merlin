%% Cleaning environment
clear
close all
clc

% weatherdata csv to mat
% first load .csv 
MeteoData2008=readtable('sept_2008.csv');
MeteoData2008=table2cell(MeteoData2008);

year=year(MeteoData2008(1,1));
month=month(MeteoData2008(1,1));

lat=length(unique(cell2mat(MeteoData2008(:,2))));
long=length(unique(cell2mat(MeteoData2008(:,3))));

Dates = datetime(MeteoData2008(:,1), 'Format', 'yyyy-MM-dd HH:mm:ss');
Days = datenum(Dates)-datenum([num2str(year),'-01-01 00:00:00'])+1;
MTime = reshape(Days,long,lat,[]);
Mlat = cell2mat(reshape(MeteoData2008(:,2),long,lat,[]));
Mlong = cell2mat(reshape(MeteoData2008(:,3),long,lat,[]));
Mu = str2double(reshape(MeteoData2008(:,4),long,lat,[]));
Mv = str2double(reshape(MeteoData2008(:,5),long,lat,[]));


MTime = rot90(MTime);
Mlat = rot90(Mlat);
Mlong = rot90(Mlong);
Mu = rot90(Mu);
Mv = rot90(Mv);

save(['MeteoMatrix',num2str(year),'_',num2str(month), '_1000'], 'MTime', 'Mlong','Mlat', 'Mu', 'Mv')