% Simulate noisy functional data and smoothed them by BFDA

%% Set up seed for simulation 

stream = RandStream('twister','Seed', 2016);
reset(stream);  % set up a seed for simulation

PWD='U:\ownCloud\BFDA_test\BFDA-master';

% Add pathes of the required MATLAB packages
% BFDA, bspline, fdaM, mcmcdiag, PACE
addpath(genpath(cat(2, PWD, '\BFDA_SourceCode')))
addpath(genpath(cat(2, PWD, '\bspline')))
addpath(genpath(cat(2, PWD, '\fdaM')))
addpath(genpath(cat(2, PWD, '\mcmcdiag')))
addpath(genpath(cat(2, PWD, '\PACErelease2.11')))


load('./Examples/Data/Simu_Data.mat')
load('./Examples/Data/Simu_Output.mat')

%% Set up parameters for simulation
n = 30; % Number of functional samples
p = 40; % Number of pooled grid points, or evaluated grid points
s = sqrt(5); % Standard deviation of functional observations
r = 2; % Signal to noise ratio
rho = 1/2; % Scale parameter in the Matern function
nu = 3.5; % Order parameter in the Matern function
pgrid = (0.001 : (pi/2)/(p-1) : (pi/2)); % Pooled grid
dense = 0.6; % Proportion of observations on the pooled grid
au = 0; bu = pi/2; % Function domain 
m = 20; % Number of working grid points
stat = 1; % Specify stationary data
cgrid = 1; % Specify common observation grid

%% Analyzing stationary functional data with common grid
% Generate simulated data from GP(3sin(4t), s^2 Matern_cor(d; rho, nu)) 
% with noises from N(0, (s/r)^2)
GausFD_cgrid = sim_gfd(pgrid, n, s, r, nu, rho, dense, cgrid, stat);

% setup parameters for BFDA
% run with BHM
param = setOptions_bfda('smethod', 'bhm', 'cgrid', 1, 'mat', 1, ...
           'M', 10000, 'Burnin', 2000, 'w', 1, 'ws', 1);

% run with Bayesian Functional PCA
% param = setOptions_bfda('smethod', 'bfpca', 'M', 50, 'Burnin', 20, ...
%			'a', 0.001, 'b', 0.001, 'w', 1);

% run with standard Bayesian Gaussian Process model
% param = setOptions_bfda('smethod', 'bgp', 'mat', 1, ...
%           'M', 50, 'Burnin', 20);

% run with Cubic Smoothing Splines
% param = setOptions_bfda('smethod', 'css');

% call BFDA
[out_cgrid, param] = ...
    BFDA(GausFD_cgrid.Xraw_cell, GausFD_cgrid.Tcell, param);

figure()
plot(out_cgrid.Z)
title('Smoothed functional data')
figure()
plot(out_cgrid.Sigma)
title('Functional covariance estimate')


%% Analyzing stationary functional data with uncommon grid
GausFD_ucgrid = sim_gfd(pgrid, n, s, r, nu, rho, dense, 0, stat);

param_uc = setOptions_bfda('smethod', 'bhm', 'cgrid', 0, 'mat', 1, 'M',...
    10000, 'Burnin', 2000, 'pace', 1, 'ws', 0.1);

[out_ucgrid, param_uc] = ...
    BFDA(GausFD_ucgrid.Xraw_cell, GausFD_ucgrid.Tcell, param_uc);

figure()
plot(out_ucgrid.Z)
title('Smoothed functional data')
figure()
plot(out_ucgrid.Sigma)
title('Functional covariance estimate')

%% Analyzing non-stationary functional data with common grid
GausFD_cgrid_ns = sim_gfd(pgrid, n, s, r, nu, rho, dense, cgrid, 0);

param_ns = setOptions_bfda('smethod', 'bhm', 'cgrid', 1, 'mat', 0, 'M',...
    10000, 'Burnin', 2000, 'pace', 1, 'ws', 0.01);

[out_cgrid_ns, param_ns] = ...
    BFDA(GausFD_cgrid_ns.Xraw_cell, GausFD_cgrid_ns.Tcell, param_ns);

figure()
plot(out_cgrid_ns.Z)
title('Smoothed functional data')
figure()
plot(out_cgrid_ns.Sigma)
title('Estimated functional covariance')

%% Analyzing non-stationary functional data with uncommon grid
GausFD_ucgrid_ns = sim_gfd(pgrid, n, s, r, nu, rho, dense, 0, 0);

param_uc_ns = setOptions_bfda('smethod', 'bhm', 'cgrid', 0, 'mat', 0, ...
    'M', 10000, 'Burnin', 2000, 'pace', 1, 'ws', 0.01);

[out_ucgrid_ns, param_uc_ns ] = ...
    BFDA(GausFD_ucgrid_ns.Xraw_cell, GausFD_ucgrid_ns.Tcell, param_uc_ns);

figure()
plot(out_ucgrid_ns.Z)
title('Smoothed functional data')
figure()
plot(out_ucgrid_ns.Sigma)
title('Estimated functional covariance')

%% Analyzing stationary functional data with random grids
GausFD_rgrid = sim_gfd_rgrid(n, p, au, bu, s, r, nu, rho, stat);

eval_grid = prctile(sort(unique(cell2mat(GausFD_rgrid.Tcell))), ...
                    1:(100/p):100);

param_rgrid = setOptions_bfda('smethod', 'babf', 'cgrid', 0, 'mat', 1, ...
    'M', 10000, 'Burnin', 2000, 'm', m, 'eval_grid', eval_grid, 'ws', 1);

% call BFDA
[out_rgrid, param_rgrid]= ...
    BFDA(GausFD_rgrid.Xraw_cell, GausFD_rgrid.Tcell, param_rgrid);

figure()
plot(out_rgrid.Z_cgrid)
title('Smoothed functional data')
figure()
plot(out_rgrid.Sigma_cgrid)
title('Estimated functional covariance')

%% Analyzing nonstationary functional data with random grids
GausFD_rgrid_ns = sim_gfd_rgrid(n, m, au, bu, s, r, nu, rho, 0);

eval_grid_ns = prctile(sort(unique(cell2mat(GausFD_rgrid_ns.Tcell))), ...
                    1:(100/p):100);

param_rgrid_ns = setOptions_bfda('smethod', 'babf', 'cgrid', 0, 'mat', ...
    0, 'M', 10000, 'Burnin', 2000, 'm', m, 'eval_grid', ...
    eval_grid_ns, 'ws', 0.05);

% call BFDA
[out_rgrid_ns, param_rgrid_ns] = ...
    BFDA(GausFD_rgrid_ns.Xraw_cell, GausFD_rgrid_ns.Tcell, param_rgrid_ns);

figure()
plot(out_rgrid_ns.Z_cgrid)
title('Smoothed functional data')
figure()
plot(out_rgrid_ns.Sigma_cgrid)
title('Estimated functional covariance')

%% Coverage probability
Xtrue_mat = reshape(cell2mat(GausFD_cgrid.Xtrue_cell), ...
    size(GausFD_cgrid.Xtrue_cell{1}, 2), size(GausFD_cgrid.Xtrue_cell, 2));

Covprob(Xtrue_mat, out_cgrid.Z_CL, out_cgrid.Z_UL, 2)
Covprob(GausFD_cgrid.Cov_true, out_cgrid.Sigma_CL, out_cgrid.Sigma_UL, 2)
Covprob(GausFD_cgrid.Mean_true', out_cgrid.mu_CI(:, 1), out_cgrid.mu_CI(:, 2), 1)

Covprob(GausFD_ucgrid.Cov_true, out_ucgrid.Sigma_CL, out_ucgrid.Sigma_UL, 2)
Covprob(GausFD_ucgrid.Mean_true', out_ucgrid.mu_CI(:, 1), out_ucgrid.mu_CI(:, 2), 1)

Xtrue_mat_ns = reshape(cell2mat(GausFD_cgrid_ns.Xtrue_cell),...
    size(GausFD_cgrid_ns.Xtrue_cell{1}, 2), size(GausFD_cgrid_ns.Xtrue_cell, 2));

Covprob(Xtrue_mat_ns, out_cgrid_ns.Z_CL, out_cgrid_ns.Z_UL, 2)
Covprob(GausFD_cgrid_ns.Cov_true, out_cgrid_ns.Sigma_CL, out_cgrid_ns.Sigma_UL, 2)
Covprob(GausFD_cgrid_ns.Mean_true', out_cgrid_ns.mu_CI(:, 1), out_cgrid_ns.mu_CI(:, 2), 1)


%% Calculate RMSE (root mean square error)
display('RMSE of the estimated stationary covariance')
rmse(out_cgrid.Sigma_SE, GausFD_cgrid.Cov_true)

display('RMSE of the estimated functional data')
Xtrue_mat = reshape(cell2mat(GausFD_cgrid.Xtrue_cell), ...
    size(GausFD_cgrid.Xtrue_cell{1}, 2), size(GausFD_cgrid.Xtrue_cell, 2));
rmse(out_cgrid.Z, Xtrue_mat)

% calculate the true non-stationary covariance matrix
display('RMSE of the estimated non-stationary covariance')
Ctrue_ns = cov_ns(pgrid, s, nu, rho);
rmse(out_cgrid_ns.Sigma_SE, Ctrue_ns)
    
%% Save simulated data sets and BFDA results
save('./Examples/Data/Simu_Data.mat', 'GausFD_cgrid', 'GausFD_ucgrid', ...
                           'GausFD_cgrid_ns', 'GausFD_ucgrid_ns', ...
                           'GausFD_rgrid', 'GausFD_rgrid_ns', 'p', ...
                           'eval_grid', 'eval_grid_ns', 'pgrid', 'n')

save('./Examples/Data/Simu_Output.mat', 'out_cgrid', 'out_ucgrid', ...
                           'out_cgrid_ns', 'out_ucgrid_ns', ...
                           'out_rgrid', 'out_rgrid_ns')
                       
%% Make plots by calling plot_script_smooth.m
%close all;
%run('./Examples/plot_script_smooth.m');

