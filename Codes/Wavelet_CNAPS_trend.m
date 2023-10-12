clear all;
close all;
% For plotting coastline
addpath /projects/rsmas/parislab/lab_common_database/aux_files/gshhg-bin-2;
addpath ./mfiles;
% cmocean is for color scheme
addpath ./mfiles/cmocean/;
% cdt contains scripts for data processing, use for the anomaly plots here
addpath ./mfiles/cdt;
% my input files are on the mat directory
addpath ./mat;
%
anomColor = cmocean('balance',50);
%
lon0 = -82;
lonf = -75;
lat0 = 28.5;
latf = 35;
%
S = gshhs('gshhs_h.b',[lat0 latf],[lon0 lonf]);
%
file_path = '/nethome/avaz/SAtlantic/data/cnaps/';

% load the lat and lon of the files

lon = ncread([file_path,'CNAPS2_SAB_1993.nc'],'lon_rho');
lat = ncread([file_path,'CNAPS2_SAB_1993.nc'],'lat_rho');
landmask = ncread([file_path,'CNAPS2_SAB_1993.nc'],'mask_rho');
bathy = ncread([file_path,'CNAPS2_SAB_1993.nc'],'h');
bathy(bathy <= 0) = 0;

landmask(landmask == 0) = NaN;

% Compute the interpolant
depthsplot=[100; 400; 600];
%----------------------------------------------------
% get "lines" of bathymetry
hFig=figure;
set(hFig,'Visible','off');
for quallayer=1:3
  idepth=depthsplot(quallayer);
  [c,h]=contour(lon,lat,bathy,[idepth idepth]);
  iind=find(c(1,:)==idepth);
  eval(['lon',num2str(idepth),'=c(1,:);']);
  eval(['lon',num2str(idepth),'(iind)=NaN;']);
  eval(['lat',num2str(idepth),'=c(2,:);']);
  eval(['lat',num2str(idepth),'(iind)=NaN;']);
end
%
iaux = 1;
% create mm for monthly averages
for iyear = 1993:2021
  for imonth = 1:12
    datesMonth(iaux) = datenum(iyear,imonth,1,0,0,0);
    iaux = iaux + 1;
  end
end

% Initialize variables
startYear = 1993;
endYear = 2021;
datesMonthT = datetime('1900-01-01 00:00:00') + calmonths(0:(endYear-startYear)*12);

% Loop to assign the correct dates
for iyear = startYear:endYear
  for imonth = 1:12
    datesMonthT((iyear - startYear)*12 + imonth) = datetime(iyear, imonth, 1);
  end
end
load timeAllCNAPS;
load  Ilandpositions_CNAPS;
latplot = [30,32,32,34];
lonplot = [-80.5,-80,-79,-77];
namepoint = {'South','CBIn','CBOut','North'};

% Loop through each point and find the closest matching index
for i = 1:length(latplot)
    latDiff = abs(lat(Ibathy) - latplot(i));
    lonDiff = abs(lon(Ibathy) - lonplot(i));
    % Calculate the distance metric (e.g., Euclidean distance)
    distance = sqrt(latDiff.^2 + lonDiff.^2);
    % Find the index with the minimum distance
    [~, index] = min(distance);
    % Store the index for this point
    pointIndices(i) = index;
end

for ivar = 1:5
   switch logical(true)
      case ivar == 1
          varname = 'SSH';
          myname  = 'ssh';
          colorplot = cmocean('balance',30);
       case ivar == 2
          varname = 'T_bottom';
          myname  = 'bottomT';
          colorplot = cmocean('thermal',30);
      case ivar == 3
          varname = 'SST';
          myname  = 'sst';
          colorplot = cmocean('thermal',30);
      case ivar == 4
          varname = 'SSS';
          myname  = 'Salinity';
          colorplot = cmocean('haline',30);
      case ivar == 5
          varname = 'mld'
          myname = 'mixedlayer'
          colorplot = cmocean('deep',30);””
    end
    %
    numComponents = 6;

    eval(['load ',myname,'_CNAPS_Monthly;']);

    for ipoint = 1:5
        % mean scenario
       if ipoint == 1
          blaux = mean(varM,2);
          nameP = 'Mean';
       else
         blaux = varM(:,pointIndices(ipoint-1));
         nameP = namepoint(ipoint-1);
      end

      blaux2 = detrend(blaux,'constant');

      % plot anomaly with seasonality
      hFig = figure('pos',[100 100 1500 500]);
      set(hFig,'Visible','off');
      anomaly(datesMonth, blaux2);
      axis tight;
      datetick('x','yy','keeplimits');
      eval(['print -dpng ',myname,'_',char(nameP),'_anomaly_Monthly_withseasonality.png']);
      close all;

      T = table(blaux);
      T.Properties.VariableNames = {varname};
      D = trenddecomp(T,"ssa",length(blaux)/2,NumSeasonal=numComponents);
      D = addvars(D,T);
      D.datesMonth = datesMonthT';

      hFig = figure('pos',[100 100 1500 500]);
      set(hFig,'Visible','off');
      stackedplot(D, 'XVariable', 'datesMonth','LineWidth',2);
      eval(['print -dpng ',myname,'_',char(nameP),'_trendcomp_Monthly_withseasonality.png']);
      close all;

      tablesave = splitvars(D);
      writetable(tablesave,[myname,'_',char(nameP),'_trendcomp_Monthly_withseasonality.csv']);
      clear T;
      clear D;
      clear tablesave;
  end
end

% trend for the _anomaly
%wavelet for anomaly example
% First do the trend for each variable
for ivar = 1:1
   switch logical(true)
      case ivar == 1
          varname = 'SSH';
          myname  = 'ssh';
          colorplot = cmocean('balance',30);
       case ivar == 2
          varname = 'T_bottom';
          myname  = 'bottomT';
          colorplot = cmocean('thermal',30);
      case ivar == 3
          varname = 'SST';
          myname  = 'sst';
          colorplot = cmocean('thermal',30);
      case ivar == 4
          varname = 'SSS';
          myname  = 'Salinity';
          colorplot = cmocean('haline',30);
      case ivar == 5
          varname = 'mld'
          myname = 'mixedlayer'
          colorplot = cmocean('deep',30);””
    end
    % variables saved with wrangling code
    % timeAll contains matlab formatted date for entire time series (daily)
    % Ilandpositions_CNAPS contains where the water points are located (Ibathy)
    load timeAllCNAPS;
    load  Ilandpositions_CNAPS;
    [yy,mm,dd,hh,mi,ss] = datevec(double(datesMonth));
    I = Ibathy;
    eval(['load ',myname,'_CNAPS_Monthly;']);
    Ntime  = size(varM,1);
    points = size(varM,2);
    Nyears = length(unique(yy));

    eval(['load seasonal_sp_avg_',myname,'_CNAPS']);

    % For Spawning Seasonality
    for iseason = 1:2
      switch iseason
        case 1
          sName = 'Spawning_Winter';
        case 2
          sName = 'Spawning_Summer';
      end
      Ntime  = size(seasonal_avg_sp,1);
      points = size(seasonal_avg_sp,2);

      toplot = landmask*NaN;
      toplot(I)= v(:,i);
      %
        hFig=figure;
        set(hFig,'Visible','off');
        hFig.Resize = 'off';
        set(hFig, 'Position',  [491, 241, 600, 800]);
        worldmap([lat0 latf],[lon0 lonf]);
        axis tight;
        %------------ plot land ------------------
        geoshow(S, 'FaceColor', [200./255 200./255 200./255], 'DefaultEdgeColor', [200./255 200./255 200./255]);
        hold on;
        %------------ plot trend -------------------
        contourfm(double(lat),double(lon),toplot, 100, 'linestyle','none');
        lim = max(max(abs(toplot)));
        clim([-lim lim]);
        cmocean('balance');
        colorbar;
        %------------ plot bathymetry  -------------------
        plotm(lat400, lon400, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
        plotm(lat100, lon100, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
        plotm(lat600, lon600, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
        set(gca,'fontsize',10,'fontname','arial');
        set(gca,'YTick',29:2:35);
        set(gca,'YTickLabel',{'29^oN','31^oN','33^oN','35^oN'});
        set(gca,'XTick',-80:2:-75);
        set(gca,'XTickLabel',{'80^oW','78^oW'});
        title(['EOF Mode ' num2str(i),' Var Explained: ',num2str(varexplained(i)*100)]);
        eval(['print -dpng ',mynameP,'_',sName,'_mode',num2str(i),'_CNAPS.png;']);
        close all;
        %
      end
    end % end season

    % do SEASONAL
    eval(['load seasonal_avg_',myname,'_CNAPS']);
    for iseason = 1:4
      switch iseason
        case 1
          sName = 'Winter';
        case 2
          sName = 'Spring';
        case 3
          sName = 'Summer';
        case 4
          sName = 'Fall';
      end
      %
      Ntime  = size(seasonal_avg,1);
      points = size(seasonal_avg,2);

         toplot = landmask*NaN;
         toplot(I)= v(:,i);
         %
         hFig=figure;
         set(hFig,'Visible','off');
         hFig.Resize = 'off';
         set(hFig, 'Position',  [491, 241, 600, 800]);
         worldmap([lat0 latf],[lon0 lonf]);
         axis tight;
         %------------ plot land ------------------
         geoshow(S, 'FaceColor', [200./255 200./255 200./255], 'DefaultEdgeColor', [200./255 200./255 200./ 255]);
         hold on;
         %------------ plot trend -------------------
         contourfm(double(lat),double(lon),toplot, 100, 'linestyle','none');
         lim = max(max(abs(toplot)));
         clim([-lim lim]);
         cmocean('balance');
         colorbar;
         %------------ plot bathymetry  -------------------
         plotm(lat400, lon400, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
         plotm(lat100, lon100, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
         plotm(lat600, lon600, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
         set(gca,'fontsize',10,'fontname','arial');
         set(gca,'YTick',29:2:35);
         set(gca,'YTickLabel',{'29^oN','31^oN','33^oN','35^oN'});
         set(gca,'XTick',-80:2:-75);
         set(gca,'XTickLabel',{'80^oW','78^oW'});
         title(['EOF Mode ' num2str(i),' Var Explained: ',num2str(varexplained(i)*100)])
         eval(['print -dpng ',mynameP,'_',sName,'_mode',num2str(i),'_CNAPS.png;']);
         close all;
         %
    end % end season
    %---------------------------------------------------------------------------
    % do montly
    eval(['load ',myname,'_CNAPS_Monthly;']);

    Ntime  = size(varM,1);
    points = size(varM,2);
    Nyears = length(yy);

      toplot = landmask*NaN;
      toplot(I)= v(:,i);
      %
      hFig=figure;
      set(hFig,'Visible','off');
      hFig.Resize = 'off';
      set(hFig, 'Position',  [491, 241, 600, 800]);
      worldmap([lat0 latf],[lon0 lonf]);
      axis tight;
      %------------ plot land ------------------
      geoshow(S, 'FaceColor', [200./255 200./255 200./255], 'DefaultEdgeColor', [200./255 200./255 200./255]);
      hold on;
      %------------ plot trend -------------------
      contourfm(double(lat),double(lon),toplot, 100, 'linestyle','none');
      lim = max(max(abs(toplot)));
      clim([-lim lim]);
      cmocean('balance');
      colorbar;
      %------------ plot bathymetry  -------------------
      plotm(lat400, lon400, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
      plotm(lat100, lon100, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
      plotm(lat600, lon600, 'Color', [150/255 150/255 150/255], 'LineWidth', 1);
      set(gca,'fontsize',10,'fontname','arial');
      set(gca,'YTick',29:2:35);
      set(gca,'YTickLabel',{'29^oN','31^oN','33^oN','35^oN'});
      set(gca,'XTick',-80:2:-75);
      set(gca,'XTickLabel',{'80^oW','78^oW'});
      title(['EOF Mode ' num2str(i),' Var Explained: ',num2str(varexplained(i)*100)])
      eval(['print -dpng ',mynameP,'_mode',num2str(i),'_CNAPS.png;']);
      %
end % end var
