% Calculate CMIP6 deltas

% 7/8/2020
% JGM

%%% Bring in CMIP6 projection NetCDFs that I processed in Python. Re-grid
%%% and interpolate to oisst and calculate a monthly delta of scenario - historical. 

%%
clear; clc; close all;
addpath('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\GeneralFunctions\')

%% load OIsst and lat/lon grids

% Global monthly climatology
% load ('C:\Users\jmason\Documents\MATLAB\CMIP6\ESRL_circshift.mat');
% load ('C:\Users\jmason\Documents\MATLAB\CMIP6\ESRL_circshift_lon.mat');
% load('C:\Users\jmason\Documents\MATLAB\CMIP6\ESRL_circshift_lat.mat');
% 
% figure(1); pcolor(ESRL_xx2, ESRL_yy2, ESRL_circshift(:,:,1)); title ('ESRL circshift');
% shading interp
%     colorbar
    
% can I use this or do I need the cropped one...

% Use cropped climatology
 %netcdf2mat('C:\Users\jmason\Documents\Python Scripts\oisst_month_climatology_1982_2011.nc');
 oisst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\oisst_month_climatology_1982_2011.nc.mat');

 oisst_sst = permute(oisst.ncStruct.sst, [2 1 3]); %it's transposed, so switch around
 oisst_lon = oisst.ncStruct.lon;
 oisst_lat = oisst.ncStruct.lat;
 
 
figure(1); pcolor(oisst_lon, oisst_lat, oisst_sst(:,:,1)); title ('oisst');
shading interp
    colorbar
    
% make oisst meshgrid for 2d lat/lon
[oisst_xx2, oisst_yy2] = meshgrid(oisst_lon, oisst_lat);

% save lat and lon 
%save 'C:\Users\jmason\Documents\MATLAB\CMIP6\oisst_lon.mat' oisst_lon;
%save 'C:\Users\jmason\Documents\MATLAB\CMIP6\oisst_lat.mat' oisst_lat;

%% Load GINS

% load file made from code below. varname is gins_months_reshape
load ('C:\Users\jmason\Documents\MATLAB\CMIP6\gins_bt_clim_reshape.mat');
%
figure(2); pcolor(oisst_lon, oisst_lat, gins_months_reshape(:,:,1)); title ('GINS');
shading interp
    colorbar

% have three separate files for each decade. combine and take a mean

% first convert
% netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_85.nc');
% netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_95.nc');
% netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_05.nc');
% 
% gins_85 = load ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_85.nc.mat')
% gins_95 = load ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_95.nc.mat')
% gins_05 = load ('C:\Users\jmason\Documents\Python Scripts\GINS_bt_05.nc.mat')

% t_an is 241 x 141 x 57 x 12

%%
%combine and take average
% gins_all = cat (4, gins_85.ncStruct.t_an, gins_95.ncStruct.t_an, gins_05.ncStruct.t_an);
% size (gins_all) % 241 x 141 x 12 x 3
% %%
% gins_mn = mean (gins_all, 4, 'omitnan');
% size (gins_mn) % 241 x 141 x 12
% %%
% gins_lon = gins_85.ncStruct.lon;
% gins_lat = gins_85.ncStruct.lat;
% 
% figure(1); pcolor(gins_lon, gins_lat, gins_mn(:,:,1)'); title ('GINS');
% shading interp
%     colorbar
    
%% interpolate to match oisst

% gins_reshape = zeros (140, 240, 12);
% 
% for j=1:12
%     % take one month slice at a time
%    gins_temper = gins_mn(:,:,j)';
%    
%    % make meshgrid for 2d lat/lon
%     [gins_xx1, gins_yy2] = meshgrid(gins_lon, gins_lat);
%     
%     % shift code from Kristin 
%     gins_xx2 = gins_xx1.*(gins_xx1>=-180) + mod(gins_xx1,180).*(gins_xx1<-180);
%     shiftamount = -(mod(find(gins_xx2==min(gins_xx2))-1,size(gins_xx2,1))+0);
%     
%     for i=1:size(gins_xx2,2)
%     gins_xx2(:,i) = circshift(gins_xx2(:,i),shiftamount(i));
%     gins_temper(:,i) = circshift(gins_temper(:,i),shiftamount(i),1); 
%     gins_yy2(:,i) = circshift(gins_yy2(:,i),shiftamount(i),1);
%     end
% 
%     gins_interp = scatteredInterpolant(double(gins_xx2(:)),double(gins_yy2(:)),gins_temper(:));
%     gins_reshape = reshape(gins_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));
% % Warning: Duplicate data points have been detected and removed - corresponding values have been averaged. 
% gins_months_reshape (:,:,j) = gins_reshape;
% end
%%
% figure(3); pcolor (oisst_xx2, oisst_yy2, gins_months_reshape (:,:,2)); title ('gins reshape 2')
% % weird grid but maybe that's good?
%  
% save 'C:\Users\jmason\Documents\MATLAB\CMIP6\gins_bt_clim_reshape.mat' gins_months_reshape;
    

%% GFDL %%%% 

% convert to mat
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_hist_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_hist_bt_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_245_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_245_bt_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_585_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_585_bt_crop.nc');
%%
% load
gfdl_hist_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_hist_sst_crop.nc.mat')
gfdl_hist_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_hist_bt_crop.nc.mat')

figure(2); pcolor(gfdl_hist_sst.ncStruct.lon, gfdl_hist_sst.ncStruct.lat, gfdl_hist_sst.ncStruct.thetao(:,:,1)); title ('GFDL sst raw');
shading interp
    colorbar
    % this looks good, but clearly curvilinear
%% interpolate historical to oisst ands save

% make placeholder dataframe with same x/y dimensions as OIsst, with 1032 time steps 

gfdl_hist_sst_reshape = zeros (140, 240, 12);

for j=1:12
    % take one month slice at a time
   GFDL_temper = gfdl_hist_sst.ncStruct.thetao(:,:,j);
   
   % re-define gfdl lat/lon (for some reason need to re-define each time,
   % or gets weirdly skewed)
    GFDL_xx1 = gfdl_hist_sst.ncStruct.lon;
    GFDL_yy2 = gfdl_hist_sst.ncStruct.lat;
    
    % shift code from Kristin 
    GFDL_xx2 = GFDL_xx1.*(GFDL_xx1>=-180) + mod(GFDL_xx1,180).*(GFDL_xx1<-180);
    shiftamount = -(mod(find(GFDL_xx2==min(GFDL_xx2))-1,size(GFDL_xx2,1))+0);
    
    for i=1:size(GFDL_xx2,2)
    GFDL_xx2(:,i) = circshift(GFDL_xx2(:,i),shiftamount(i));
    GFDL_temper(:,i) = circshift(GFDL_temper(:,i),shiftamount(i),1); 
    GFDL_yy2(:,i) = circshift(GFDL_yy2(:,i),shiftamount(i),1);
    end

    GFDL_interp = scatteredInterpolant(double(GFDL_xx2(:)),double(GFDL_yy2(:)),GFDL_temper(:));
    GFDL_reshape = reshape(GFDL_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));

gfdl_hist_sst_reshape (:,:,j) = GFDL_reshape;

end
%
figure(3); pcolor (oisst_xx2, oisst_yy2, gfdl_hist_sst_reshape (:,:,2)); title ('GFDL reshape 2')
    
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\gfdl_hist_sst_reshape.mat' gfdl_hist_sst_reshape;
%

%% scenarios--interpolate and calculate delta
% load
gfdl_245_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_245_sst_crop.nc.mat')
gfdl_245_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_245_bt_crop.nc.mat')
gfdl_585_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_585_sst_crop.nc.mat')
gfdl_585_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\gfdl_585_bt_crop.nc.mat')

%%
% make placeholder dataframe with same x/y dimensions as OIsst, with 1032 time steps 
gfdl_months_reshape = zeros (140, 240, 1032);


for j=1:1032
    % take one month slice at a time
   GFDL_temper = gfdl_585_sst.ncStruct.thetao(:,:,j);
   
   % re-define gfdl lat/lon (for some reason need to re-define each time,
   % or gets weirdly skewed)
    GFDL_xx1 = gfdl_585_sst.ncStruct.lon;
    GFDL_yy2 = gfdl_585_sst.ncStruct.lat;
    
    % shift code from Kristin 
    GFDL_xx2 = GFDL_xx1.*(GFDL_xx1>=-180) + mod(GFDL_xx1,180).*(GFDL_xx1<-180);
    shiftamount = -(mod(find(GFDL_xx2==min(GFDL_xx2))-1,size(GFDL_xx2,1))+0);
    
    for i=1:size(GFDL_xx2,2)
    GFDL_xx2(:,i) = circshift(GFDL_xx2(:,i),shiftamount(i));
    GFDL_temper(:,i) = circshift(GFDL_temper(:,i),shiftamount(i),1); 
    GFDL_yy2(:,i) = circshift(GFDL_yy2(:,i),shiftamount(i),1);
    end

    GFDL_interp = scatteredInterpolant(double(GFDL_xx2(:)),double(GFDL_yy2(:)),GFDL_temper(:));
    GFDL_reshape = reshape(GFDL_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));
% Warning: Duplicate data points have been detected and removed - corresponding values have been averaged.

%GFDL_hist_sst_reshape (:,:,j) = GFDL_reshape;
gfdl_months_reshape (:,:,j) = GFDL_reshape;
end
%
figure(3); pcolor (oisst_xx2, oisst_yy2, gfdl_months_reshape (:,:,2)); title ('GFDL reshape 2')
%
% calculate deltas
% only 12 months for climatology, so stack 86 times for the 86 years of
% projection
% https://www.mathworks.com/help/matlab/ref/repmat.html
gfdl_hist_sst_rep = repmat (gfdl_hist_sst_reshape, [1 1 86]);
% 
gfdl_585_sst_delta = gfdl_months_reshape - gfdl_hist_sst_rep;
% 
figure(4); pcolor (oisst_xx2, oisst_yy2, gfdl_585_sst_delta (:,:,20)); title ('GFDL delta');

%
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\gfdl_585_sst_deltas.mat' gfdl_585_sst_delta;
%% also calculate and save delta plus climatology

% sst
oisst_rep = repmat (oisst_sst, [1 1 86]);

gfdl_245_sst_projection = gfdl_245_sst_delta + oisst_rep;

figure(5); pcolor (oisst_xx2, oisst_yy2, gfdl_245_sst_projection (:,:,1000)); title ('GFDL sst projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\gfdl_245_sst_projection.mat' gfdl_245_sst_projection;

% bt
gins_rep = repmat (gins_months_reshape, [1 1 86]);

gfdl_245_bt_projection = gfdl_245_bt_delta + gins_rep;

figure(6); pcolor (oisst_xx2, oisst_yy2, gfdl_245_bt_projection (:,:,1000)); title ('GFDL bt projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\gfdl_245_bt_projection.mat' gfdl_245_bt_projection;
%% CNRM %%

% don't have hist working yet, so I'm going to reshape and save the
% scenarios. 

netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_hist_sst_crop.nc');
%netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_hist_bt_crop.nc');
 netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_245_sst_crop.nc');
% netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\cnrm_245_bt_crop.nc')
% netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\cnrm_585_bt_crop.nc');
 netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_585_sst_crop.nc');

%%
% load
cnrm_hist_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_hist_bt_crop.nc.mat')
cnrm_hist_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_hist_sst_crop.nc.mat')

figure(2); pcolor(cnrm_hist_sst.ncStruct.lon, cnrm_hist_sst.ncStruct.lat, cnrm_hist_sst.ncStruct.thetao(:,:,1)); title ('CNRM raw');
shading interp
    colorbar
    % this looks good, but clearly curvilinear
  
%% load scenarios
cnrm_585_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_585_sst_crop.nc.mat');
cnrm_585_sst.ncStruct.thetao(1:10, 1:10, 1)

%%
cnrm_245_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\cnrm_245_sst_crop.nc.mat');
cnrm_245_sst.ncStruct.thetao(1:10, 1:10, 1)
%% interpolate to oisst
%cnrm_585_sst_reshape = zeros (140, 240, 1032);

cnrm_hist_bt_reshape = zeros (140, 240, 12);

for j=1:12
   cnrm_temper = cnrm_hist_bt.ncStruct.thetao(:,:,j);
   
    cnrm_xx1 = cnrm_hist_bt.ncStruct.lon;
    cnrm_yy2 = cnrm_hist_bt.ncStruct.lat;
    
    cnrm_xx2 = cnrm_xx1.*(cnrm_xx1>=-180) + mod(cnrm_xx1,180).*(cnrm_xx1<-180);
    shiftamount = -(mod(find(cnrm_xx2==min(cnrm_xx2))-1,size(cnrm_xx2,1))+0);
    
    for i=1:size(cnrm_xx2,2)
    cnrm_xx2(:,i) = circshift(cnrm_xx2(:,i),shiftamount(i));
    cnrm_temper(:,i) = circshift(cnrm_temper(:,i),shiftamount(i),1); 
    cnrm_yy2(:,i) = circshift(cnrm_yy2(:,i),shiftamount(i),1);
    end

    cnrm_interp = scatteredInterpolant(double(cnrm_xx2(:)),double(cnrm_yy2(:)),cnrm_temper(:));
    cnrm_reshape = reshape(cnrm_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));
% Warning: Duplicate data points have been detected and removed - corresponding values have been averaged. 
%cnrm_sst_585_reshape (:,:,j) = cnrm_reshape;
cnrm_hist_bt_reshape (:,:,j) = cnrm_reshape;
end

% plot to check
figure(3); pcolor (oisst_xx2, oisst_yy2, cnrm_hist_bt_reshape (:,:,2)); title ('cnrm reshape 2')

% save temporary scenarios
%save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_reshape.mat' cnrm_sst_reshape;
%save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_reshape.mat' cnrm_sst_585_reshape;
% save historical
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_hist_bt_reshape.mat' cnrm_hist_bt_reshape;
%save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_hist_sst_reshape.mat' cnrm_hist_sst_reshape;

%% load reshaped scenarios
load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_reshape.mat')
%cnrm_245_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_bt_reshape.mat')
load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_reshape.mat')
%cnrm_585_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_bt_reshape.mat')

load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_hist_sst_reshape.mat')
%% check if identical
cnrm_sst_245_reshape(1:10, 1:10, 1)
cnrm_sst_585_reshape(1:10, 1:10, 1)
%% calculate deltas

cnrm_hist_sst_rep = repmat (cnrm_hist_sst_reshape, [1 1 86]);
% 
cnrm_585_sst_delta = cnrm_sst_585_reshape - cnrm_hist_sst_rep;
% 
figure(4); pcolor (oisst_xx2, oisst_yy2, cnrm_585_sst_delta (:,:,20)); title ('cnrm delta');

%
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_deltas.mat' cnrm_585_sst_delta;

cnrm_245_sst_delta = cnrm_sst_245_reshape - cnrm_hist_sst_rep;

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_deltas.mat' cnrm_245_sst_delta;

%% bottom temp
cnrm_hist_bt_rep = repmat (cnrm_hist_bt_reshape, [1 1 86]);

cnrm_245_bt_delta = cnrm_245_bt.cnrm_bt_reshape - cnrm_hist_bt_rep;
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_bt_deltas.mat' cnrm_245_bt_delta;

cnrm_585_bt_delta = cnrm_585_bt.cnrm_bt_reshape - cnrm_hist_bt_rep;

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_bt_deltas.mat' cnrm_585_bt_delta;


%% also calculate and save delta plus climatology

% sst
oisst_rep = repmat (oisst_sst, [1 1 86]);

cnrm_245_sst_projection = cnrm_245_sst_delta + oisst_rep;

figure(5); pcolor (oisst_xx2, oisst_yy2, cnrm_245_sst_projection (:,:,1000)); title ('cnrm sst projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_projection.mat' cnrm_245_sst_projection;

cnrm_585_sst_projection = cnrm_585_sst_delta + oisst_rep;

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_projection.mat' cnrm_585_sst_projection;

%% bt
load('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\gins_bt_clim_reshape.mat');
gins_rep = repmat (gins_months_reshape, [1 1 86]);

cnrm_245_bt_projection = cnrm_245_bt_delta + gins_rep;

figure(6); pcolor (oisst_xx2, oisst_yy2, cnrm_245_bt_projection (:,:,1000)); title ('cnrm bt projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_bt_projection.mat' cnrm_245_bt_projection;


cnrm_585_bt_projection = cnrm_585_bt_delta + gins_rep;

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_bt_projection.mat' cnrm_585_bt_projection;

%%
% check if cnrm 245 and 585 are identical
load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_projection.mat')
load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_projection.mat')

%%
cnrm_245_sst_projection(1:10, 1:10, 1)
cnrm_585_sst_projection(1:10, 1:10, 1) % yes


%% check if sst_crop et. are identical. 
cnrm_585_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_585_sst_crop.nc.mat')
cnrm_245_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\cnrm_245_sst_crop.nc.mat')

cnrm_245_sst.ncStruct.thetao(1:10, 1:10, 1)
cnrm_585_sst.ncStruct.thetao(1:10, 1:10, 1)

%%

%%%%%%%%%
%% MOHC 
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_hist_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_hist_bt_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_bt_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_585_sst_crop.nc');
netcdf2mat ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_585_bt_crop.nc');
%%
% load
mohc_hist_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\mohc_hist_sst_crop.nc.mat')
mohc_hist_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\mohc_hist_bt_crop.nc.mat')
% mohc_245_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_sst_crop.nc.mat')
% mohc_245_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_bt_crop.nc.mat')
% mohc_585_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_585_sst_crop.nc.mat')
% mohc_585_bt = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_585_bt_crop.nc.mat')

% figure(12); pcolor(mohc_245_bt.ncStruct.longitude, mohc_245_bt.ncStruct.latitude, mohc_245_bt.ncStruct.thetao(:,:,1)); title ('mohc raw');
% shading interp
%     colorbar


%% reshape and save historical
mohc_hist_bt_reshape = zeros (140, 240, 12);

for j=1:12
   mohc_temper = mohc_hist_bt.ncStruct.thetao(:,:,j);
   
    mohc_xx1 = mohc_hist_bt.ncStruct.longitude;
    mohc_yy2 = mohc_hist_bt.ncStruct.latitude;
    
    mohc_xx2 = mohc_xx1.*(mohc_xx1>=-180) + mod(mohc_xx1,180).*(mohc_xx1<-180);
    shiftamount = -(mod(find(mohc_xx2==min(mohc_xx2))-1,size(mohc_xx2,1))+0);
    
    for i=1:size(mohc_xx2,2)
    mohc_xx2(:,i) = circshift(mohc_xx2(:,i),shiftamount(i));
    mohc_temper(:,i) = circshift(mohc_temper(:,i),shiftamount(i),1); 
    mohc_yy2(:,i) = circshift(mohc_yy2(:,i),shiftamount(i),1);
    end

    mohc_interp = scatteredInterpolant(double(mohc_xx2(:)),double(mohc_yy2(:)),mohc_temper(:));
    mohc_reshape = reshape(mohc_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));

mohc_hist_bt_reshape (:,:,j) = mohc_reshape;
end

% plot to check

figure(3); pcolor (oisst_xx2, oisst_yy2, mohc_hist_bt_reshape (:,:,2)); title ('mohc reshape 2')
% save historical
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_hist_bt_reshape.mat' mohc_hist_bt_reshape;

%% interpolate scenarios to oisst and calculate delta and save

mohc_months_reshape = zeros (140, 240, 1032);

for j=1:1032
   mohc_temper = mohc_585_bt.ncStruct.thetao(:,:,j);
   
    mohc_xx1 = mohc_585_bt.ncStruct.longitude;
    mohc_yy2 = mohc_585_bt.ncStruct.latitude;
    
    mohc_xx2 = mohc_xx1.*(mohc_xx1>=-180) + mod(mohc_xx1,180).*(mohc_xx1<-180);
    shiftamount = -(mod(find(mohc_xx2==min(mohc_xx2))-1,size(mohc_xx2,1))+0);
    
    for i=1:size(mohc_xx2,2)
    mohc_xx2(:,i) = circshift(mohc_xx2(:,i),shiftamount(i));
    mohc_temper(:,i) = circshift(mohc_temper(:,i),shiftamount(i),1); 
    mohc_yy2(:,i) = circshift(mohc_yy2(:,i),shiftamount(i),1);
    end

    mohc_interp = scatteredInterpolant(double(mohc_xx2(:)),double(mohc_yy2(:)),mohc_temper(:));
    mohc_reshape = reshape(mohc_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));
% Warning: Duplicate data points have been detected and removed - corresponding values have been averaged. 
mohc_months_reshape (:,:,j) = mohc_reshape;

end

% plot to check
figure(3); pcolor (oisst_xx2, oisst_yy2, mohc_months_reshape (:,:,2)); title ('mohc reshape 2')


% calculate deltas

mohc_hist_bt_rep = repmat (mohc_hist_bt_reshape, [1 1 86]);

mohc_585_bt_delta = mohc_months_reshape - mohc_hist_bt_rep;

figure(4); pcolor (oisst_xx2, oisst_yy2, mohc_585_bt_delta (:,:,200)); title ('mohc delta');

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_585_bt_deltas.mat' mohc_585_bt_delta;

%% also calculate and save delta plus climatology

% sst
oisst_rep = repmat (oisst_sst, [1 1 86]);

mohc_245_sst_projection = mohc_245_sst_delta + oisst_rep;

figure(5); pcolor (oisst_xx2, oisst_yy2, mohc_245_sst_projection (:,:,1000)); title ('mohc sst projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_sst_projection.mat' mohc_245_sst_projection;

% bt
gins_rep = repmat (gins_months_reshape, [1 1 86]);

mohc_245_bt_projection = mohc_245_bt_delta + gins_rep;

figure(6); pcolor (oisst_xx2, oisst_yy2, mohc_245_bt_projection (:,:,1000)); title ('mohc bt projection'); 

save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\mohc_245_bt_projection.mat' mohc_245_bt_projection;



%%%%%%%
%% IPSL
%netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_hist_sst_crop.nc');
netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_hist_bt_crop.nc');
%netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_245_sst_crop.nc');
netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_245_bt_crop.nc');
%netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_585_sst_crop.nc');
netcdf2mat ('C:\Users\jmason\Documents\Python Scripts\ipsl_585_bt_crop.nc');

%% load
ipsl_hist_sst = load ('\\rschfs1x\userRS\F-J\jgm278_RS\Documents\conda_dir\ipsl_hist_sst_crop.nc.mat')
%ipsl_245_bt = load ('C:\Users\jmason\Documents\Python Scripts\ipsl_245_bt_crop.nc.mat')
% has different names--nav_lon and nav_lat.

% figure(2); pcolor(ipsl_245_bt.ncStruct.nav_lon, ipsl_245_bt.ncStruct.nav_lat, ipsl_245_bt.ncStruct.thetao(:,:,1)); title ('ipsl raw');
% shading interp
%     colorbar
    
%% interpolate to oisst
%ipsl_months_reshape = zeros (140, 240, 1032);
ipsl_hist_sst_reshape = zeros (140, 240, 12);

for j=1:12
   ipsl_temper = ipsl_hist_sst.ncStruct.thetao(:,:,j);
   
    ipsl_xx1 = ipsl_hist_sst.ncStruct.nav_lon;
    ipsl_yy2 = ipsl_hist_sst.ncStruct.nav_lat;
    
    ipsl_xx2 = ipsl_xx1.*(ipsl_xx1>=-180) + mod(ipsl_xx1,180).*(ipsl_xx1<-180);
    shiftamount = -(mod(find(ipsl_xx2==min(ipsl_xx2))-1,size(ipsl_xx2,1))+0);
    
    for i=1:size(ipsl_xx2,2)
    ipsl_xx2(:,i) = circshift(ipsl_xx2(:,i),shiftamount(i));
    ipsl_temper(:,i) = circshift(ipsl_temper(:,i),shiftamount(i),1); 
    ipsl_yy2(:,i) = circshift(ipsl_yy2(:,i),shiftamount(i),1);
    end

    ipsl_interp = scatteredInterpolant(double(ipsl_xx2(:)),double(ipsl_yy2(:)),ipsl_temper(:));
    ipsl_reshape = reshape(ipsl_interp(double(oisst_xx2(:)),double(oisst_yy2(:))),size(oisst_sst(:,:,1)));
% Warning: Duplicate data points have been detected and removed -
% corresponding values have been averaged. %
%ipsl_months_reshape (:,:,j) = ipsl_reshape;
ipsl_hist_sst_reshape (:,:,j) = ipsl_reshape;
end

% plot to check
figure(3); pcolor (oisst_xx2, oisst_yy2, ipsl_hist_sst_reshape (:,:,2)); title ('ipsl reshape 2')

% save historical
save '\\rschfs1x\userRS\F-J\jgm278_RS\Documents\MATLAB\ipsl_hist_sst_reshape.mat' ipsl_hist_sst_reshape;

%% calculate deltas

ipsl_hist_bt_rep = repmat (ipsl_hist_bt_reshape, [1 1 86]);

ipsl_245_bt_delta = ipsl_months_reshape - ipsl_hist_bt_rep;
%ipsl_hist_sst_delta = ipsl_months_reshape - ipsl_hist_sst_reshape;

figure(4); pcolor (oisst_xx2, oisst_yy2, ipsl_245_bt_delta (:,:,200)); title ('ipsl delta'); colormap(bluewhitered);

save 'C:\Users\jmason\Documents\MATLAB\CMIP6\ipsl_245_bt_deltas.mat' ipsl_245_bt_delta;


%% also calculate and save delta plus climatology

% sst
% oisst_rep = repmat (oisst_sst, [1 1 86]);
% 
% ipsl_585_sst_projection = ipsl_585_sst_delta + oisst_rep;
% 
% figure(5); pcolor (oisst_xx2, oisst_yy2, ipsl_585_sst_projection (:,:,1000)); title ('ipsl sst projection'); 
% 
% save 'C:\Users\jmason\Documents\MATLAB\CMIP6\ipsl_585_sst_projection.mat' ipsl_585_sst_projection;

% bt
gins_rep = repmat (gins_months_reshape, [1 1 86]);

ipsl_585_bt_projection = ipsl_585_bt_delta + gins_rep;

figure(6); pcolor (oisst_xx2, oisst_yy2, ipsl_585_bt_projection (:,:,1000)); title ('ipsl bt projection'); 

save 'C:\Users\jmason\Documents\MATLAB\CMIP6\ipsl_585_bt_projection.mat' ipsl_585_bt_projection;

