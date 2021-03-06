%%% Theoretical bird migration model
% Maja Bradaric
% 2019

% University of Amsterdam

%% Notes
% making the shapefile consist of less parts will improve code performance


%% Initialization %%
%% Cleaning environment
clear
close all
clc

%% Performance check
tic
profile on
%% Loading data and assigning parameters
%Load bird data
birds='One_day_20100326.mat';
load(birds);
year=2010;
month=3;
pl=1000;



%% Loading data and assigning parameters


%% Loading data
% Load basemap
%load('ShapeOfEurope.mat');
%load('Europemap.mat');

Europe=shaperead('W_Europe.shp');

% Load weather data
Meteo=['MeteoMatrix',num2str(year),'_',num2str(month),'_', num2str(pl)];
load (Meteo);





%% Control variables
nDays=length(unique(birds(:,1)));  %length(unique(birds(:,1)))
nTracks=length(Headings);    %length(Headings); %height(birds)
nSteps=30;
dt=0.5;         % timestep in hours
as=Airspeed;          % airspeed [m s-1]
winf=1;       % wind influence multiplier
DayS=Day;   %Days(1,:);       % startday DayS=birds
DispAmount=floor(nTracks/2);  % amount of individuals to visualize per group
% note: nTrack/nDays should be more than DispAmount


sizeGroups=floor(nTracks/nDays);        % quantity of birds per group every day


HD=Headings;



%% Initiation of matrices
Time(1:nTracks,1:nSteps,1:nDays)=NaN;
Lat(1:nTracks,1:nSteps,1:nDays)=NaN;
Long(1:nTracks,1:nSteps,1:nDays)=NaN;
wu(1:nTracks,1:nSteps,1:nDays)=NaN;
wv(1:nTracks,1:nSteps,1:nDays)=NaN;
gu(1:nTracks,1:nSteps,1:nDays)=NaN;
gv(1:nTracks,1:nSteps,1:nDays)=NaN;
au(1:nTracks,1:nSteps,1:nDays)=NaN;
av(1:nTracks,1:nSteps,1:nDays)=NaN;
GS(1:nTracks,1:nSteps,1:nDays)=NaN;
tr(1:nTracks,1:nSteps,1:nDays)=NaN;
trdir(1:nTracks,1:nSteps,1:nDays)=NaN;
trdir_u(1:nTracks,1:nSteps,1:nDays)=NaN;
trdir_v(1:nTracks,1:nSteps,1:nDays)=NaN;
tr_angle(1:nTracks,1:nSteps,1:nDays)=NaN;
trdir_angle(1:nTracks,1:nSteps,1:nDays)=NaN;
        
TDist(1:nTracks,1:nSteps,1:nDays)=0;
tt(1:nTracks,1:nDays)=NaN;
sunsetT(1:nTracks,1:nDays)=NaN;
hr1(1:nTracks,1:nDays)=NaN;
hr2(1:nTracks,1:nDays)=NaN;
stp(1:nTracks,1:nDays)=NaN;
fLat(1:nTracks,1:nDays)=NaN;
fLong(1:nTracks,1:nDays)=NaN;
fwu(1:nTracks,1:nDays)=NaN;
fwv(1:nTracks,1:nDays)=NaN;
ffwu(1:nTracks,1:nDays)=NaN;
ffwv(1:nTracks,1:nDays)=NaN;
ftd(1:nTracks,1:nDays)=NaN;
fftd(1:nTracks,1:nDays)=NaN;
fgs(1:nTracks,1:nDays)=NaN;
ffgs(1:nTracks,1:nDays)=NaN;
fhour(1:nTracks,1:nDays)=NaN;
fhour2(1:nTracks,1:nDays)=NaN;
%HD(1:nTracks,1:nSteps,1:nDays)=30


Distu_unit(1:nTracks,1:nSteps,1:nDays)=NaN;
Distv_unit(1:nTracks,1:nSteps,1:nDays)=NaN;

% Suncycle interpolation
SSLong= -16:10:34 ; SSLong(2,:)=SSLong(1,:);
SSLat=[65 65 65 65 65 65 ; 30 30 30 30 30 30];
SSTime(2,6)=NaN;



%% Visualisation
% image width and height
imgWidthPix = 1900;
imgHeightPix = 1050;
% set the image sizes, resolution and units; also see print option -r100
imgResolution = 100; % 100 dpi
imgWidth = imgWidthPix / imgResolution;
imgHeight = imgHeightPix / imgResolution;
% this line is necessary for rendering without openGL drivers/physical screen
set(0, 'DefaultFigureRenderer', 'zbuffer');
scrsz = get(0,'ScreenSize');

% create vector of the tracks that are plotted
plotTracks(1:DispAmount*nDays)=NaN;
AboveLand=zeros(nTracks,nDays);
s=rng(1);%control random number sampling
plotTracks(1:DispAmount)=datasample(1:nTracks,DispAmount);
for i=1:nDays-1
    plotTracks(i*DispAmount+1:i*DispAmount+DispAmount)= (i*sizeGroups+1):(i*sizeGroups+DispAmount);
end
[~,plotDelete]=size(plotTracks);

% Weather data bounding box
minLong = min(Mlong(1,:,1))/100;
maxLong = max(Mlong(1,:,1))/100;
minLat = min(Mlat(:,1,1))/100;
maxLat = max(Mlat(:,1,1))/100;




%% Dynamic calculations %%
%% Days
for ii=1:nDays
    %change
    % Setting bird starting locations
    for i=sizeGroups*ii-sizeGroups+1:sizeGroups*ii
        Long(i,1,ii)=4.389639;
        Lat(i,1,ii)=52.60636;
    end
    
    
    % Title time
    Tdum=DayS(1,1)-ii+datenum([num2str(year),'-01-01 00:00:00']);
    TT=datestr(Tdum, 'yyyy mm dd'); TT(5)=[]; TT(7)=[];
    
    % Setting the location at first timestep
    if ii ~= 1  % days
        Long(1:sizeGroups*ii-sizeGroups,1,ii)=Long(1:sizeGroups*ii-sizeGroups,nSteps,ii-1);
        Lat(1:sizeGroups*ii-sizeGroups,1,ii)=Lat(1:sizeGroups*ii-sizeGroups,nSteps,ii-1);
    end
    
  
    %% Steps
    for i=1:nSteps      %24 steps = 0.5 day ; 60 steps max
        %% Setting the time
        if i==1   % starttime
            for ic=1:6
                for ir=1:2  % suncycle strting points
                    DUM(1:2)=suncycle(SSLat(ir,ic), SSLong(ir,ic), [str2double(TT(1:4)), str2double(TT(5:6)), str2double(TT(7:8))], 2880)/24;
                    SSTime(ir,ic)=DUM(2);
                end
            end
            DumTime=interpn(SSLat,SSLong,SSTime,Lat(1:nTracks,1,ii),Long(1:nTracks,1,ii),'linear', -999);
            Time(1:nTracks,1,ii)=DayS+1-ii;
            tt(1:nTracks,ii)=DayS-ii+datenum([num2str(year),'-01-01 00:00:00']);
            sunsetT(1:nTracks,ii)=DumTime+datenum([str2double(TT(1:4)), str2double(TT(5:6)), str2double(TT(7:8))]);
            %hr1(1:nTracks,ii)=hms(datetime(tt,'ConvertFrom','datenum'));
            %hr2(1:nTracks,ii)=hms(datetime(sunsetT,'ConvertFrom','datenum'));
            [ht,mt,st]=hms(datetime(tt,'ConvertFrom','datenum'));
            [hs,ms,ss]=hms(datetime(sunsetT,'ConvertFrom','datenum'));
            
            
            %             Time(1:nTracks,i,ii)=DayS-1+ii+1800/2400;     % start at 18.00 instead of sunset
        else        % add timestep to time
            Time(1:nTracks,i,ii)=Time(1:nTracks,i-1,ii)-dt/24;      % [day]
            
        end
  
        %% Calculating new bird locations
       
        % Calculating ground speed in m s^-1
        au(1:nTracks,i,ii)=as.*sin((HD(1:nTracks,ii))/180*pi);
        av(1:nTracks,i,ii)=as.*cos((HD(1:nTracks,ii))/180*pi);
        wu(1:nTracks,i,ii)=interpn(Mlat/100,Mlong/100,MTime, Mu, Lat(1:nTracks,i,ii),Long(1:nTracks,i,ii),Time(1:nTracks,i,ii),'linear', 0);
        wv(1:nTracks,i,ii)=interpn(Mlat/100,Mlong/100,MTime, Mv, Lat(1:nTracks,i,ii),Long(1:nTracks,i,ii),Time(1:nTracks,i,ii),'linear', 0);
        gu(1:nTracks,i,ii)=au(1:nTracks,i,ii)+winf*wu(1:nTracks,i,ii); % groundspeed u
        gv(1:nTracks,i,ii)=av(1:nTracks,i,ii)+winf*wv(1:nTracks,i,ii); % groundspeed v
        GS(1:nTracks,i,ii)=sqrt(gu(1:nTracks,i,ii).*gu(1:nTracks,i,ii)+gv(1:nTracks,i,ii).*gv(1:nTracks,i,ii)); %ground speed [m s^-1]
        tr(1:nTracks,i,ii)=atan2(gu(1:nTracks,i,ii),gv(1:nTracks,i,ii));
        tr_angle(1:nTracks,i,ii)=tr(1:nTracks,i,ii)*(180/pi);
        trdir_angle(1:nTracks,i,ii)=wrapTo360(tr_angle(1:nTracks,i,ii)+180);
        trdir(1:nTracks,i,ii)=trdir_angle(1:nTracks,i,ii)/180*pi;
        trdir_u(1:nTracks,i,ii)=GS(1:nTracks,i,ii).*sin(trdir(1:nTracks,i,ii));
        trdir_v(1:nTracks,i,ii)=GS(1:nTracks,i,ii).*cos(trdir(1:nTracks,i,ii));
        
        % Add distance traveled to total distance
        if i==1
            TDist(1:nTracks,i,ii)=GS(1:nTracks,i,ii)*3600*dt/1000;   %[km]
        else
            TDist(1:nTracks,i,ii)=TDist(1:nTracks,i-1,ii)+GS(1:nTracks,i,ii)*3600*dt/1000; % total route distance [km]
        end
        
        % km per degree
        Distu_unit(1:nTracks,i,ii)=distWBvector([Lat(1:nTracks,i,ii) Long(1:nTracks,i,ii)-0.5] ,[Lat(1:nTracks,i,ii) Long(1:nTracks,i,ii)+0.5]);
        Distv_unit(1:nTracks,i,ii)=distWBvector([Lat(1:nTracks,i,ii)-0.5 Long(1:nTracks,i,ii)] ,[Lat(1:nTracks,i,ii)+0.5 Long(1:nTracks,i,ii)]);
        
        % Write new bird locations
        if i<nSteps
            Lat(1:nTracks,i+1,ii)=Lat(1:nTracks,i,ii)+trdir_v(1:nTracks,i,ii).*3600*dt./1000./Distv_unit(1:nTracks,i,ii);
            Long(1:nTracks,i+1,ii)=Long(1:nTracks,i,ii)+trdir_u(1:nTracks,i,ii).*3600*dt./1000./Distu_unit(1:nTracks,i,ii);
        end
    
    end %for i=1:nSteps
    %% steps to plot
   % for iii=1:nTracks
    %    if hr1(iii)>hr2(iii)
     %       stp(iii,ii)=2*(hr1(iii)-hr2(iii));
      %  elseif hr1(iii)==hr2(iii)
       %     stp(iii,ii)=1;
        %else
         %   stp(iii,ii)=2*(24-hr2(iii)+hr1(iii));
        %end
        
        
    %end
    
    for iii=1:nTracks
        if ht(iii)*60+mt(iii)>hs(iii)*60+ms(iii)
            stp(iii,ii)=round(((ht(iii)*60+mt(iii))-(hs(iii)*60+ms(iii)))/30);
        elseif ht(iii)*60+mt(iii)==hs(iii)*60+ms(iii)
            stp(iii,ii)=1;
        else
            stp(iii,ii)= round(((1440-(hs(iii)*60+ms(iii)))+(ht(iii)*60+mt(iii)))/30);
        end
        
        
    end
   
    %% Visualization
    %% Make figure
    if ii==1
        scrsz = get(0,'ScreenSize');
        f= figure('Position',[30 scrsz(4)/20 scrsz(4) scrsz(3)]);
        %f= figure('Position',[5 scrsz(4)/40 scrsz(3)/2 scrsz(4)/1.2]);
        xlim([-14 24]); ylim([48 67])
        ax = gca;
        ax.FontSize = 18; 
      %  xlabel('Longitude','Fontsize',18)
      %  ylabel('Latitude','Fontsize',18)
        hold on
    end
    
    %% Background
    if ii==1    % draw map of Europe
     %   for i=1:6
     %       h1=plot(CoastLon(i).X,CoastLat(i).Y,'k');
     %   end
           %      for j=1:62    % other shapefile
           %          h1=plot(ShapeOfEurope(j).X,ShapeOfEurope(j).Y,'k');
           %      end
           
           for j=1:101
               h1=plot(Europe(j).X,Europe(j).Y,'k');
           end
        % draw bounding box weather data
        BoundBox = [minLat,minLong; minLat,maxLong; ...
            maxLat,maxLong; maxLat,minLong; minLat,minLong];
    %    h2=plot(BoundBox(:,2),BoundBox(:,1),'r-');
    end
    
    if ii~=1
        children = get(gca, 'children');
        delete(children(1:plotDelete*3));
    end
    %% Make birds fly
    for j=plotTracks
        
            h3=plot(Long(j,1:stp(j,ii),ii),Lat(j,1:stp(j,ii),ii),'-','color',rgb('Silver'));      % tracks
            h3.Color(4) = 0.01;
            h4=plot(Long(j,1,ii),Lat(j,1,ii),'*','color',rgb('DarkMagenta'),'Markersize', 10);% beginpoints every day
            h5=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',rgb('Teal'), 'Markersize', 3);    % endpoints every day
            h5.Color(4) = 0.01;
            %if 1<=stp(j,ii) && stp(j,ii)<=8
            %h5=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',rgb('DimGray'), 'Markersize', 3);    % endpoints every day
            %h5.Color(4) = 0.01;
            %else
            %    h5=plot(NaN,NaN,'.','color',rgb('DimGray'), 'Markersize', 3);
            %end
            %if 9<=stp(j,ii) && stp(j,ii)<=16
            %h6=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',rgb('SlateGray'), 'Markersize', 3);    % endpoints every day
            %h6.Color(4) = 0.01;
            %else
            %    h6=plot(NaN,NaN,'.','color',rgb('SlateGray'), 'Markersize', 3);
            %end
            %if 17<=stp(j,ii) && stp(j,ii)<=24
            %h7=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',rgb('DarkSlateGray'), 'Markersize', 3);    % endpoints every day
            %h7.Color(4) = 0.01;
            %else
            %    h7=plot(NaN,NaN,'.','color',rgb('DarkSlateGray'), 'Markersize', 3);
            %end
            %if 25<=stp(j,ii) && stp(j,ii)<=32
            %h8=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',rgb('Teal'), 'Markersize', 3);    % endpoints every day
            %h8.Color(4) = 0.01;
            % else
            %    h8=plot(NaN,NaN,'.','color',rgb('Teal'), 'Markersize',3);
            %end
            
        
        %       for l=1:6
        %           AboveLand(j,ii)=inpolygon(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii), CoastLon(l).X, CoastLat(l).Y);
        %           if AboveLand(j,ii)==0
        %           h5=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',[1 1 1], 'Markersize', 3);    % endpoints every day
        %           h5.Color(4) = 0.5;
        %           else
        %           h5=plot(Long(j,stp(j,ii),ii),Lat(j,stp(j,ii),ii),'.','color',[0.5 0.8 0.5], 'Markersize', 3);    % endpoints every day
        %           h5.Color(4) = 0.01;
        %           end
        %       end
   
    end
   % for j=1:62    % other shapefile
   %     h1=plot(ShapeOfEurope(j).X,ShapeOfEurope(j).Y,'k');
   % end
    
    for j=1:101
        h1=plot(Europe(j).X,Europe(j).Y,'k');
    end
    
    
    %% Formatting
 %  if ii==1    % add legend
  %      [leg,icons]=legend([h4,h1,h3,h5],{'Radar','Coastline','Trajectories','Departure locations'},'Location','northwest','Fontsize',24);
   %     set(leg, 'Position', [0.2841 0.7457 0.1146 0.1160]);
    %    hlt = text(...
     %       'Parent', leg.DecorationContainer, ...
      %      'String','Legend', ...
       %     'HorizontalAlignment', 'center', ...
        %    'VerticalAlignment', 'bottom', ...
         %   'Position', [0.5, 1.05, 0], ...
          %  'Units', 'normalized','FontSize',30,'Fontweight','bold');
        
        
        %icons = findobj(icons, 'type', 'line'); %// objects of legend of type line
     %   set(icons, 'Markersize',20); %// set marker size as desired
    %end
    
   % title([TT, ', AS=',num2str(mean(as)),' EndoDir=',num2str(mean(Headings)),' Number of birds=',num2str(sum(nTracks))
   %     ],'FontSize',14)
   % drawnow
    
   
  
    
    
    
end %for ii= nDays
%% Video
   % v = VideoWriter(['stp', num2str(DayS), '_winf', num2str(winf), '_as', num2str(as), '.mp4']);
   %open(v)%% Video
   % for k=stp(j)
   %     writeVideo(v,getframe(gcf))
   % end
    
% close(v)
%% Analysis
SummedDist(1,1:nTracks)=0; %initialization
for j=1:nDays
    SummedDist=nansum([SummedDist;TDist(:,1,j)']);
end


%% Semivariogram
%RadarArea=100;  % n*n km    n will be halved to obtain radar reach
%RadarLat=[55*ones(1,16),53*ones(1,16),51*ones(1,16),49*ones(1,16),...
%    47*ones(1,16),45*ones(1,16),43*ones(1,16),41*ones(1,16),39*ones(1,16),37*ones(1,16),35*ones(1,16)];
%RadarLong=[-5:2:25,-5:2:25,-5:2:25,-5:2:25,-5:2:25,-5:2:25,-5:2:25,...
%    -5:2:25,-5:2:25,-5:2:25,-5:2:25];
% [~,AmountRadars]=size(RadarLat);
% RadarOcc(1:AmountRadars)=0;
% 
% % km degree-1
% RadarDistu_unit=distWBvector([RadarLat(:) RadarLong(:)-0.5] ,[RadarLat(:) RadarLong(:)+0.5]);
% RadarDistv_unit=distWBvector([RadarLat(:)-0.5 RadarLong(:)] ,[RadarLat(:)+0.5 RadarLong(:)]);
% 
% % radar reach in lat long degrees
% DegLatRadar=0.5*RadarArea./RadarDistv_unit;
% DegLongRadar=0.5*RadarArea./RadarDistu_unit;
% 
% % RadarOcc contains the amount of birds that are within reach
% for j=1:AmountRadars
%     RadarOcc(j) = sum(Lat(:,i,ii)>RadarLat(j)-DegLatRadar(i) & Lat(:,i,ii)<RadarLat(j)+ DegLatRadar(j) ...
%         & Long(:,i,ii)>RadarLong(j)-DegLongRadar(j)  & Long(:,i,ii)<RadarLong(j)+DegLongRadar(j));
% end
% 
% 
% % Show locations of radars
% plot(RadarLong,RadarLat,'b+')

%% Saving
% Figure
Filename=['M' TT '_' num2str(nanmean(as)) '_'  num2str(pl) '.png'];
print ('-dpng', '-r100', Filename)

Time_new=Time-1+datenum([num2str(year),'-01-01 00:00:00']);
TTime=datetime(Time_new,'ConvertFrom','datenum','Format', 'yyyy-MM-dd HH:mm:ss');
THours=string(TTime);

    for iii=1:nTracks
     
      fLat(iii,ii)=Lat(iii,stp(iii,ii),ii);
      fLong(iii,ii)=Long(iii,stp(iii,ii),ii);
      fwu(iii,ii)=wu(iii,stp(iii,ii),ii);
      fwv(iii,ii)=wv(iii,stp(iii,ii),ii);
      ffwu(iii,ii)=nanmean(wu(iii,1:stp(iii,ii),ii));
      ffwv(iii,ii)=nanmean(wv(iii,1:stp(iii,ii),ii));
      ftd(iii,ii)=wrapTo360(tr_angle(iii,stp(iii,ii),ii));
      fftd(iii,ii)=wrapTo360(nanmean(tr_angle(iii,1:stp(iii,ii),ii)));
      fgs(iii,ii)=GS(iii,stp(iii,ii),ii);
      ffgs(iii,ii)=nanmean(GS(iii,1:stp(iii,ii),ii));
      fhour2(iii,ii)=THours(iii,1,ii);
    end
 
% time=array2table(fTime,'VariableNames',{'Time'});
 new=array2table([fLong,fLat,fhour2,fwu,fwv,ffwu,ffwv,ftd,fftd,fgs,ffgs,HD,as,stp],'VariableNames',{'Long_start','Lat_start','timestamp_radar','wu_start','wv_start','wu_mean','wv_mean','td_start','td_mean','gsms_start','gsms_mean','heading','airspeed','step'});
 writetable(new,['Departures_',TT,'_',num2str(pl),'.csv']);

% Analysis
%save(['RadarData',num2str(year),'_day',num2str(DayS(1,1)+ii-1),'_winf',num2str(winf),'_as',num2str(nanmean(as)),'_',num2str(pl)]);
%save(['AllResults',num2str(year)]);

%% Performance check
%toc
%profile viewer
