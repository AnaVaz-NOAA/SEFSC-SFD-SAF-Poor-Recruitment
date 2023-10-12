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
iaux  = 1;
iauxm = 1;
% create mm for monthly averages
for iyear = 1993:2021
  for imonth = 1:12
    datesMonth(iaux) = datenum(iyear,imonth,1,0,0,0);
    iaux = iaux + 1;
  end
  for imonth = 1:3
    datesSeason(iauxm) = iyear+(0.33*imonth-1);
    iauxm = iauxm + 1;
  end
end

% set seasons here
seasonBsp = [2,6];
seasonEsp = [4,8];
seasonB   = [1,4,7,10];
seasonE   = [3,6,9,12];

%
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
            colorplot = cmocean('deep',30);
    end
    % variables saved with wrangling code
    % timeAll contains matlab formatted date for entire time series (daily)
    % Ilandpositions_CNAPS contains where the water points are located (Ibathy)
    load timeAllCNAPS;
    load Ilandpositions_CNAPS;

    [yy,mm,dd,hh,mi,ss] = datevec(double(datesMonth));
    I = Ibathy;

    eval(['load ',myname,'_CNAPS_Monthly;']);

    Ntime  = size(varM,1);
    points = size(varM,2);
    Nyears = length(unique(yy));

    eval(['load seasonal_sp_avg_',myname,'_CNAPS']);

    mynameP = [myname, '_constant'];

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
      % remove the mean value with detrend function
      % this is the data we will analyse
      varDetrend = zeros(Ntime,points);
      varDetrendmean = zeros(Ntime,points);

      for i=1:points;
        varDetrend(:,i) = detrend(seasonal_avg_sp(:,i,iseason),'constant');
      end

      blaux = mean(varDetrend,2);
      lim = max([max(abs(mean(varDetrend,2))) max(abs(mean(varDetrendmean,2)))]);
      hFig = figure('pos',[100 100 1000 500]);
      set(hFig,'Visible','off');
      title('Detrend constant');
      anomaly(unique(yy), blaux);
      axis([min(yy) max(yy) -lim lim]);
      eval(['print -dpng ',mynameP,'_',sName,'_anomaly.png']);
      eval(['save ',mynameP,'_',sName,'_anomaly blaux'])
      close all;

      % calculate EOFs

      varSeason = varDetrend;

      % calculate EOFs
      [u,s,v] = svd(varSeason,'econ');

      % If I don't transpose, the spatial mode is in v, the
      % temporal (PCA) is in u. If transposed, spatial is in u and
      % temporal mode is in v. S is the same for both ways.
      % EOF = v
      % PCs = u
      % #columns = modes; eg. mode1 = u(:,1) and v(:,1)
      PC = u;

      varexplained = diag(s).^2/sum(diag(s).^2);

      % To find a and the temporal mean for all grid points
      a = s*v';
      aa = landmask*NaN;
      aa(I) = mean(a(:,:).^2);

      % To compute the temporal mean for the anomalies for each grid point
      med    = landmask*NaN;
      med(I) = mean(varSeason.^2);

      % Eigen values in the diagonal of this matrix
      var_PC = (s.^2)./(Ntime-1);
      var_varDetrend = var(varSeason);

      % check the sum of all variance is equal to the sum of the eigen values
      disp(['Sum of all variances: ' num2str(sum(var_varDetrend))]);
      % the trace of a matrix is the sum of the diagonal elements
      disp(['Sum of all eigen values: ' num2str(trace(var_PC))]);

      % look at the first 4 EOFs, variance explained and scores
      % turn back into 2D matrices and plot EOFs
      for i=1:2
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
        %
        hFig = figure;
        set(hFig,'Visible','off');
        hFig.Resize = 'off';
        set(hFig, 'Position',  [100 100 1000 500]);
        plot(unique(yy),PC(:,i),'LineWidth',2);
        hold on;
        axis tight;
        title(['EOF temporal mode ' num2str(i)]);
        eval(['print -dpng ',mynameP,'_',sName,'_temporal_mode',num2str(i),'_CNAPS.png;']);
        close all;
      end
      EOFtime = PC(:,1:4);
      eval(['save EOF_time_',mynameP,'_',sName,'_CNAPS EOFtime']);
      EOFSpace = v(:,1:2);
      eval(['save EOF_space_',mynameP,'_',sName,'_CNAPS EOFSpace']);
    end % end season

    %---------------------------------------------
    % do SEASONAL ONE VALUE A YEAR
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

      % remove the mean value with detrend function
      % this is the data we will analyse
      % if we use constant, it does not remove the long-term trend
      % varDetrend = detrend(seasonal_avg(:,:,iseason),'constant');

      varDetrend = zeros(Ntime,points);
      varDetrendmean = zeros(Ntime,points);
      for i=1:points;
        varDetrend(:,i) = detrend(seasonal_avg(:,i,iseason),'constant');
      end

      blaux = mean(varDetrend,2);
      lim = max([max(abs(mean(varDetrend,2))) max(abs(mean(varDetrendmean,2)))]);
      hFig = figure('pos',[100 100 1000 500]);
      set(hFig,'Visible','off');
      title('Detrend constant');
      anomaly(unique(yy), blaux);
      axis([min(yy) max(yy) -lim lim]);
      eval(['print -dpng ',myname,'_',sName,'_anomaly_constant.png']);
      eval(['save ',myname,'_',sName,'_anomaly blaux']);
      close all;

      varSeason = varDetrend;
      mynameP = [myname, '_constant'];

      % calculate EOFs
      [u,s,v] = svd(varSeason,'econ');

      % if I don't transpose, the spatial mode is in v, the
      % temporal is in u. S is the same for both ways.
      % EOF = v
      % PC  = u;
      PC = u;

      varexplained = diag(s).^2/sum(diag(s).^2);

      % To find a and the temporal mean for all grid points
      a = s*v';
      aa = landmask*NaN;
      aa(I) = mean(a(:,:).^2);

      % To compute the temporal mean for the anomalies for each grid point
      med    = landmask*NaN;
      med(I) = mean(varSeason.^2);

      % v contains the eigen vectors PCs
      % Eigen values in the diagonal of this matrix
      var_PC = (s.^2)./(Ntime-1);
      var_varDetrend = var(varSeason);

      % check the sum of all variance is equal to the sum of the eigen values
      disp(['Sum of all variances: ' num2str(sum(var_varDetrend))]);
      % the trace of a matrix is the sum of the diagonal elements
      disp(['Sum of all eigen values: ' num2str(trace(var_PC))]);

      % look at the first 4 EOFs, variance explained and scores
      % turn back into 2D matrices and plot EOFs
      for i=1:4
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
         hFig = figure;
         set(hFig,'Visible','off');
         hFig.Resize = 'off';
         set(hFig, 'Position', [100 100 1000 500]);
         plot(unique(yy),PC(:,i),'LineWidth',2);
         hold on;
         axis tight;
         title(['EOF temporal mode ' num2str(i)]);
         eval(['print -dpng ',mynameP,'_',sName,'_temporal_mode',num2str(i),'_CNAPS.png;']);
         close all;
      end
      EOFtime = PC(:,1:4);
      eval(['save EOF_time_',mynameP,'_',sName,'_CNAPS EOFtime']);
      EOFSpace = v(:,1:2);
      eval(['save EOF_space_',mynameP,'_',sName,'_CNAPS EOFSpace']);
    end % end season

    %---------------------------------------------------------------------------
    % do MONTHLY
    eval(['load ',myname,'_CNAPS_Monthly;']);

    Ntime  = size(varM,1);
    points = size(varM,2);
    Nyears = length(yy);

    % remove the mean value with detrend function
    % this is the data we will analyse
    varDetrend = zeros(Ntime,points);

    for i=1:points
      varDetrend(:,i) = detrend(varM(:,i),'constant');
    end

    %% EOF analysis without seasonal cycle
    % create another dataset obtained after removing the seasonal cycle
    % estimated simply as monthly averages

    varSeason     = zeros(Ntime,points);
    varMonth      = zeros(12,points);
    for imonth = 1:12
      Imonth = find(mm == imonth);
      varMonth(imonth,:) = mean(varDetrend(Imonth,:));
      for iaux = 1:length(Imonth)
        varSeason(Imonth(iaux),:) = varDetrend(Imonth(iaux),:) - varMonth(imonth,:);
      end
    end

    % plot the average of bloth series
    blaux =mean(varSeason,2);
    lim  = max(abs(mean(varSeason,2)));
    hFig = figure('pos',[100 100 1000 500]);
    set(hFig,'Visible','off');
    title('Detrend constant');
    anomaly(datesMonth,mean(varSeason,2));
    axis([min(datesMonth) max(datesMonth) -lim lim]);
    axis tight;
    datetick('x','yy','keeplimits');
    eval(['print -dpng ',myname,'_anomaly.png']);
    eval(['save ',myname,'_anomaly blaux'])

    close all;

    %% EOF calculation
    varSeason = varSeason;
    mynameP = [myname, '_constant'];

    [u,s,v] = svd(varSeason,'econ');
    Ntime  = size(varSeason,1);
    points = size(varSeason,2);

    % if I don't transpose, the spatial mode is in v, the
    % temporal is in u. S is the same for both ways.
    % EOF = v
    % PCs = u
    PC = u;

    varexplained = diag(s).^2/sum(diag(s).^2);

    % To find a and the temporal mean for all grid points
    a = s*v';
    aa = landmask*NaN;
    aa(I) = mean(a(:,:).^2);

    %To compute the temporal mean for the anomalies for each grid point
    med    = landmask*NaN;
    med(I) = mean(varSeason.^2);

    % u contains the eigen vectors PCs
    PC = u;

    % Eigen values in the diagonal of this matrix
    var_PC = (s.^2)./(Ntime-1);
    var_varDetrend = var(varSeason);

    % check the sum of all variance is equal to the sum of the eigen values
    disp(['Sum of all variances: ' num2str(sum(var_varDetrend))]);
    % the trace of a matrix is the sum of the diagonal elements
    disp(['Sum of all eigen values: ' num2str(trace(var_PC))]);

    % look at the first 4 EOFs, variance explained and scores
    % turn back into 2D matrices and plot EOFs
    for i = 1:4
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
      %
      hFig = figure('pos',[100 100 1000 500]);
      set(hFig,'Visible','off');
      plot(datesMonth,PC(:,i),'LineWidth',2);
      axis tight;
      title(['EOF temporal mode ' num2str(i)]);
      eval(['print -dpng ',mynameP,'_temporal_mode',num2str(i),'_CNAPS.png;']);
      close all;
    end
    EOFtime = PC(:,1:4);
    eval(['save EOF_time_',mynameP,'_deseason_CNAPS EOFtime']);
    EOFSpace = v(:,1:2);
    eval(['save EOF_space_',mynameP,'_deseason_CNAPS EOFSpace']);
    %
    %-----------------------------------------------------
    % do SEASONAL WITH MONTHS

    % save the months for each season after detrending
    seasonal_month = zeros(3*29, points, 4);
    seasonal_month_sp = zeros(3*29, points, 2);

    % concatenate the months as above (already detrended)
    for iyear = min(yy):max(yy)
      yearsave = (iyear-min(yy))+1;
      for iseason = 1:2
        monthaux = ((yearsave-1)*3)+1
        monthB = seasonBsp(iseason);
        monthE = seasonEsp(iseason);
        for imonthSeason = monthB:monthE
          indaux = find(mm == imonthSeason & yy == iyear);
          for igrid = 1:points
            seasonal_month_sp(monthaux,igrid,iseason) = varM(indaux,igrid);
          end
          monthaux = monthaux + 1
        end
      end
      for iseason = 1:4
        monthaux = ((yearsave-1)*3)+1
        monthB = seasonB(iseason);
        monthE = seasonE(iseason);
        for imonthSeason = monthB:monthE
          indaux = find(mm == imonthSeason & yy == iyear);
          for igrid = 1:points
            seasonal_month(monthaux,igrid,iseason) = varM(indaux,igrid);
          end
          monthaux = monthaux + 1
        end
      end
    end

    eval(['save seasonal_month_',myname,'_CNAPS seasonal_month'])
    eval(['save seasonal_month_sp_',myname,'_CNAPS seasonal_month_sp'])

    seasonal_avg = seasonal_month;

    for iseason = 1:4
      switch iseason
        case 1
          sName = 'Winter_AllM';
        case 2
          sName = 'Spring_AllM';
        case 3
          sName = 'Summer_AllM';
        case 4
          sName = 'Fall_AllM';
      end
      %
      Ntime  = size(seasonal_avg,1);
      points = size(seasonal_avg,2);

      % remove the mean value with detrend function
      % this is the data we will analyse
      % if we use constant, it does not remove the long-term trend
      % varDetrend = detrend(seasonal_avg(:,:,iseason),'constant');

      varDetrend = zeros(Ntime,points);
      for i=1:points;
        varDetrend(:,i) = detrend(seasonal_avg(:,i,iseason),'constant');
      end

      blaux = mean(varDetrend,2);
      lim = max(max(abs(mean(varDetrend,2))));
      hFig = figure('pos',[100 100 1000 500]);
      set(hFig,'Visible','off');
      title('Detrend constant');
      anomaly(datesSeason, blaux);
      axis([min(datesSeason) max(datesSeason) -lim lim]);
      eval(['print -dpng ',myname,'_',sName,'_anomaly_constant.png']);
      eval(['save ',myname,'_',sName,'_anomaly blaux']);
      close all;

      varSeason = varDetrend;
      mynameP = [myname, '_constant'];

      % calculate EOFs
      [u,s,v] = svd(varSeason,'econ');

      % if I don't transpose, the spatial mode is in v, the
      % temporal is in u. S is the same for both ways.
      % EOF = v
      % PC  = u;
      PC = u;

      varexplained = diag(s).^2/sum(diag(s).^2);

      % To find a and the temporal mean for all grid points
      a = s*v';
      aa = landmask*NaN;
      aa(I) = mean(a(:,:).^2);

      % To compute the temporal mean for the anomalies for each grid point
      med    = landmask*NaN;
      med(I) = mean(varSeason.^2);

      % v contains the eigen vectors PCs
      % Eigen values in the diagonal of this matrix
      var_PC = (s.^2)./(Ntime-1);
      var_varDetrend = var(varSeason);

      % check the sum of all variance is equal to the sum of the eigen values
      disp(['Sum of all variances: ' num2str(sum(var_varDetrend))]);
      % the trace of a matrix is the sum of the diagonal elements
      disp(['Sum of all eigen values: ' num2str(trace(var_PC))]);

      % look at the first 4 EOFs, variance explained and scores
      % turn back into 2D matrices and plot EOFs
      for i=1:4
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
         hFig = figure;
         set(hFig,'Visible','off');
         hFig.Resize = 'off';
         set(hFig, 'Position', [100 100 1000 500]);
         plot(datesSeason,PC(:,i),'LineWidth',2);
         hold on;
         axis tight;
         title(['EOF temporal mode ' num2str(i)]);
         eval(['print -dpng ',mynameP,'_',sName,'_temporal_mode',num2str(i),'_CNAPS.png;']);
         close all;
      end
      EOFtime = PC(:,1:4);
      eval(['save EOF_time_',mynameP,'_',sName,'_CNAPS EOFtime']);
      EOFSpace = v(:,1:2);
      eval(['save EOF_space_',mynameP,'_',sName,'_CNAPS EOFSpace']);
    end % end season

    seasonal_avg = seasonal_month_sp;

    for iseason = 1:2
      switch iseason
        case 1
          sName = 'Winter_Sp_AllM';
        case 2
          sName = 'Spring_Sp_AllM';
      end
      %
      Ntime  = size(seasonal_avg,1);
      points = size(seasonal_avg,2);

      % remove the mean value with detrend function
      % this is the data we will analyse
      % if we use constant, it does not remove the long-term trend
      % varDetrend = detrend(seasonal_avg(:,:,iseason),'constant');

      varDetrend = zeros(Ntime,points);
      for i=1:points;
        varDetrend(:,i) = detrend(seasonal_avg(:,i,iseason),'constant');
      end

      blaux = mean(varSeason,2)
      lim = max(max(abs(mean(varDetrend,2))));
      hFig = figure('pos',[100 100 1000 500]);
      set(hFig,'Visible','off');
      title('Detrend constant');
      anomaly(datesSeason, mean(varDetrend,2));
      axis([min(datesSeason) max(datesSeason) -lim lim]);
      eval(['print -dpng ',myname,'_',sName,'_anomaly_constant.png']);
      eval(['save ',myname,'_',sName,'_anomaly blaux'])

      close all;

      varSeason = varDetrend;
      mynameP = [myname, '_constant'];

      % calculate EOFs
      [u,s,v] = svd(varSeason,'econ');

      % if I don't transpose, the spatial mode is in v, the
      % temporal is in u. S is the same for both ways.
      % EOF = v
      % PC  = u;
      PC = u;

      varexplained = diag(s).^2/sum(diag(s).^2);

      % To find a and the temporal mean for all grid points
      a = s*v';
      aa = landmask*NaN;
      aa(I) = mean(a(:,:).^2);

      % To compute the temporal mean for the anomalies for each grid point
      med    = landmask*NaN;
      med(I) = mean(varSeason.^2);

      % v contains the eigen vectors PCs
      % Eigen values in the diagonal of this matrix
      var_PC = (s.^2)./(Ntime-1);
      var_varDetrend = var(varSeason);

      % check the sum of all variance is equal to the sum of the eigen values
      disp(['Sum of all variances: ' num2str(sum(var_varDetrend))]);
      % the trace of a matrix is the sum of the diagonal elements
      disp(['Sum of all eigen values: ' num2str(trace(var_PC))]);

      % look at the first 4 EOFs, variance explained and scores
      % turn back into 2D matrices and plot EOFs
      for i=1:4
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
         hFig = figure;
         set(hFig,'Visible','off');
         hFig.Resize = 'off';
         set(hFig, 'Position', [100 100 1000 500]);
         plot(datesSeason,PC(:,i),'LineWidth',2);
         hold on;
         axis tight;
         title(['EOF temporal mode ', num2str(i)]);
         eval(['print -dpng ',mynameP,'_',sName,'_temporal_mode',num2str(i),'_CNAPS.png;']);
         close all;
      end
      EOFtime = PC(:,1:4);
      eval(['save EOF_time_',mynameP,'_',sName,'_CNAPS EOFtime']);
      EOFSpace = v(:,1:2);
      eval(['save EOF_space_',mynameP,'_',sName,'_CNAPS EOFSpace']);
    end % end season sp all months
end % end var
