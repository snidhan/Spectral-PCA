 % This script reads the timestamp text file and maps it to uniform time 
 % stamp file for interpolation
 
 clear all; 
 clc;
 %% Reading the actual time stamps
 dataDIR = '/home/sheel/Work/projects/spod_re5e4/post/frinf/time_stamps/';
 filename = 'time_stamp_1892600_2471900.txt';
 fullfile = strcat(dataDIR, filename);
 time_stamp = dlmread(fullfile);
 dt_time_stamp = time_stamp(2:end,1) - time_stamp(1:end-1,1);
 plot(time_stamp,'ko');
 hold on;
 %% Time file generated from start and end time
 time_start = time_stamp(1,1);
 time_end   = time_stamp(end,1);
 Delta_t = time_end - time_start;
 time_interpolated = linspace(time_start, time_end, size(time_stamp,1))';
 plot(time_interpolated,'bs');
 
 dataDIR = '/home/sheel/Work/projects/spod_re5e4/post/frinf/time_stamps/';
 filename = 'time_stamp_1892600_2471900_uniform.txt';
 fullfile = strcat(dataDIR, filename);
 fileID  = fopen(fullfile, 'w');
 fprintf(fileID, '%10.5f\n', time_interpolated);
 
 %% Best fit line to the actual time stamp
 x = linspace(1,size(time_stamp,1),size(time_stamp,1))';
 p = polyfit(x,time_stamp,1);
 time_best_fit = polyval(p,x);
 plot(time_best_fit,'rs')
 time_diff = time_best_fit - time_stamp;