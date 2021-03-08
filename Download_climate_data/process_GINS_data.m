%% Process GINS data
% 5/14/2020

% find bottom temp climatology
% 
%%
clear; clc; close all;
addpath('C:\Users\jmason\Documents\MATLAB\GeneralFunctions\')

%%
%This code will display the contents of a netcdf file:
%%
ncdisp('C:\Users\jmason\Documents\MATLAB\GINS\gins_8594_t14_04.nc');
netcdf2mat ('C:\Users\jmason\Documents\MATLAB\GINS\gins_8594_t14_04.nc');

%%
gins_84 = load ('C:\Users\jmason\Documents\MATLAB\GINS\gins_8594_t00_04.nc.mat');

t_mn = gins_84.ncStruct.t_mn;
imagesc (t_mn(:,:,1));

%%
dirList = dir;
dirList = dirList(3:17); 

for i=1:length(dirList)
    ncfilename = dirList(i).name;
    if ncfilename(end-2:end) ~= '.nc'
        continue;
    end
    netcdf2mat(ncfilename)
%     pause;
end

%%
% average seasons together. seasons are actually different from how I
% calculated for OISST, but should be fine for this process since all the
% models are calculated in the same way. Winter for GINS is jan-mar, spring
% apr-jun, etc. Winter is 13, spring 14, summer 15, fall 16

% 3rd dimension is depth NOT time

% GINS info talks about objectively analyzed temperature primarily, so will
% use that for now. t_an
filePattern = fullfile(pwd, '*.mat');
list = dir (filePattern );

%gins_W = 

% for i = 1:length(list)
%     load (
%     
annual_85 = load ('gins_8594_t00_04.nc.mat');
%% doing by brute force for now
w_85 = load ('gins_8594_t13_04.nc.mat')
w_95 = load ('gins_95A4_t13_04.nc.mat')
w_05 = load ('gins_A5B2_t13_04.nc.mat') % winters
%%
GINS_w = cat (4, w_85.ncStruct.t_an, w_95.ncStruct.t_an, w_05.ncStruct.t_an);
GINS_w_mn = mean (GINS_w, 4, 'omitnan');

figure(1); imagesc (GINS_w_mn(:,:,1));

%% doing by brute force for now
w_85 = load ('gins_8594_t13_04.nc.mat')
w_95 = load ('gins_95A4_t13_04.nc.mat')
w_05 = load ('gins_A5B2_t13_04.nc.mat') % winters
%%
GINS_w = cat (4, w_85.ncStruct.t_an, w_95.ncStruct.t_an, w_05.ncStruct.t_an);
GINS_w_mn = mean (GINS_w, 4, 'omitnan');

figure(1); imagesc (GINS_w_mn(:,:,1));
%% 
sp_85 = load ('gins_8594_t14_04.nc.mat')
sp_95 = load ('gins_95A4_t14_04.nc.mat')
sp_05 = load ('gins_A5B2_t14_04.nc.mat') 

su_85 = load ('gins_8594_t15_04.nc.mat')
su_95 = load ('gins_95A4_t15_04.nc.mat')
su_05 = load ('gins_A5B2_t15_04.nc.mat') % winters

f_85 = load ('gins_8594_t16_04.nc.mat')
f_95 = load ('gins_95A4_t16_04.nc.mat')
f_05 = load ('gins_A5B2_t16_04.nc.mat') % winters
%
GINS_sp = cat (4, sp_85.ncStruct.t_an, sp_95.ncStruct.t_an, sp_05.ncStruct.t_an);
GINS_sp_mn = mean (GINS_sp, 4, 'omitnan');

figure(3); imagesc (GINS_sp_mn(:,:,1));

GINS_su = cat (4, su_85.ncStruct.t_an, su_95.ncStruct.t_an, su_05.ncStruct.t_an);
GINS_su_mn = mean (GINS_su, 4, 'omitnan');

figure(4); imagesc (GINS_su_mn(:,:,1));

GINS_f = cat (4, f_85.ncStruct.t_an, f_95.ncStruct.t_an, f_05.ncStruct.t_an);
GINS_f_mn = mean (GINS_f, 4, 'omitnan');

figure(5); imagesc (GINS_f_mn(:,:,1));

%% save
save 'C:\Users\jmason\Documents\MATLAB\GINS\gins_w.mat' GINS_w_mn;
save 'C:\Users\jmason\Documents\MATLAB\GINS\gins_sp.mat' GINS_sp_mn;
save 'C:\Users\jmason\Documents\MATLAB\GINS\gins_su.mat' GINS_su_mn;
save 'C:\Users\jmason\Documents\MATLAB\GINS\gins_f.mat' GINS_f_mn;