%% combining weather data

clc
close all
clear



year=2008;


marchMeteo=['MeteoMatrix',num2str(year), 'march_925'];
sepMeteo=['MeteoMatrix',num2str(year), 'sep_925'];
octMeteo=['MeteoMatrix',num2str(year), 'oct_925'];

load(march_2008);
augMlat = Mlat;
augMlong = Mlong;
augMTime = MTime;
augMu = Mu;
augMv= Mv;

load(sepMeteo);
sepMlat = Mlat;
sepMlong = Mlong;
sepMTime = MTime;
sepMu = Mu;
sepMv= Mv;

load(octMeteo);
octMlat = Mlat;
octMlong = Mlong;
octMTime = MTime;
octMu = Mu;
octMv= Mv;

Mlat = cat(3,augMlat,sepMlat,octMlat);
Mlong = cat(3,augMlong,sepMlong,octMlong);
MTime = cat(3,augMTime,sepMTime,octMTime);
Mu = cat(3,augMu,sepMu,octMu);
Mv = cat(3,augMv,sepMv,octMv);

save ('MeteoMatrix',num2str(year), '_925','Mlat', 'Mlong', 'MTime', 'Mu', 'Mv')