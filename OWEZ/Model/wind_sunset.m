%run the model_changed script before this


wind_sp(1:nTracks,1:nDays)=NaN;
winddir_rad(1:nTracks,1:nDays)=NaN;
winddir_deg1(1:nTracks,1:nDays)=NaN;
winddir_deg(1:nTracks,1:nDays)=NaN;
u_sun(1:nTracks,1:nDays)=NaN;
v_sun(1:nTracks,1:nDays)=NaN;

for ii=nDays
    for k=1:nTracks
        u_sun(k,ii)=wu(k,stp(k,ii),ii);
        v_sun(k,ii)=wv(k,stp(k,ii),ii);    
    end
    wind_sp(1:nTracks,ii)=sqrt(u_sun(1:nTracks,ii).*u_sun(1:nTracks,ii)+v_sun(1:nTracks,ii).*v_sun(1:nTracks,ii));
    winddir_rad(1:nTracks,ii)=atan2(u_sun(1:nTracks,ii),v_sun(1:nTracks,ii));
    winddir_deg1(1:nTracks,ii)=winddir_rad(1:nTracks,ii)*(180/pi);
    winddir_deg(1:nTracks,ii)=wrapTo360(winddir_deg1(1:nTracks,ii));
end


winds=[winddir_deg wind_sp];
winds=array2table(winds,'VariableNames',{'winddir','wind_sp'});
writetable(winds,'winds_spring.csv');