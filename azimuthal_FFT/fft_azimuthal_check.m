%% FFT of velocity data
clear; clc; close all;
A = importdata('w_1892600_15.txt');
B = A(2:257,1) - mean(A(2:257,1));
plot(B);

% Fs = 1/(2*pi/256);
% 
% Y = fft(B);
% Y2 = abs(Y(1:128+1)/256);
% 
% f = Fs*(0:128)/256;

fftb = fft(B);
figure
plot(abs(fftb));

%% FFT of synthetic data
% clear all; close all;
% theta = linspace(0,2*pi,255)';
% 
% signal = 5 + 5*sin(2.*theta);
% signal_perturb = signal - mean(signal); 
% 
% plot(theta,signal_perturb);
% 
% Y = fft(signal_perturb);
% figure;
% plot(abs(Y), 'Linewidth',1.5);
