clear
close all
clc

year=2008;
month=3;
pl=700;

Europe=shaperead('W_Europe.shp');


Meteo=['MeteoMatrix',num2str(year),'_',num2str(month), '_',num2str(pl)];
load (Meteo);


for k = 108:110
    scrsz = get(0,'ScreenSize');
    f= figure('Position',[30 scrsz(4)/20 scrsz(4) scrsz(3)]);
    %f= figure('Position',[5 scrsz(4)/40 scrsz(3)/2 scrsz(4)/1.2]);
    xlim([-6 15]); ylim([50 62])
    xlabel('Longitude')
    ylabel('Latitude')
    hold on
    for j=1:101
        h1=plot(Europe(j).X,Europe(j).Y,'k');
    end
    
    quiver(Mlong(1:49,35:120,k)/100,Mlat(1:49,35:120,k)/100,Mu(1:49,35:120,k),Mv(1:49,35:120,k));
    Tdum= MTime(1,1,k)-1+datenum([num2str(year),'-01-01 00:00:00']);
    TT=datestr(Tdum, 'yyyymmddTHHMM');
    title([TT,'PL',num2str(pl)],'FontSize',12)
    saveas(f,['winds', TT,'_',num2str(pl),'.png']);
end

