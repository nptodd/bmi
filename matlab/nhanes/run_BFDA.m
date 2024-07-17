function [ Nsub, Nobs, out_ucgrid, param_uc ] = run_BFDA(stratum_loc_a, dir_data_a, ... 
    deb_bmi_nb_a, stat_cov_bool_a, w_a, ws_a, meth_a, ageMin_a, ageMax_a, log_transform_a)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LOAD DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% full path to the data file
full_path = [dir_data_a, stratum_loc_a, '.csv'];

% load data on strata analyzed
nhanes_loc = importfile(full_path);

if (deb_bmi_nb_a==1)
    nhanes_loc.bmi = nhanes_loc.bmi_deb1; % use debiased BMIs, version 1
end

if (deb_bmi_nb_a==2)
    nhanes_loc.bmi = nhanes_loc.bmi_deb2; % use debiased BMIs, version 2
end


if (log_transform_a==1)
    nhanes_loc.bmi = log(nhanes_loc.bmi); % use log BMI
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% DEFINE DATA FOR BFDA %%%%%%%%%%%%%%%%%%%%%%%%%%

% seqn_unique stores identifiers (sequence number [seqn]) exactly once
% its length is thus the number of subjects in the strata
seqn_unique = unique(nhanes_loc.seqn);

% data_bfda is a struct that contains data for BFDA + identifier
data_bfda.seqn = seqn_unique ;

for i=1:length(seqn_unique)
    SEQ = seqn_unique(i);
    data_bfda.ages{i}=nhanes_loc.age(nhanes_loc.seqn == SEQ)';
    data_bfda.bmis{i}=nhanes_loc.bmi(nhanes_loc.seqn == SEQ)';
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RUN BFDA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if(strcmp(meth_a, 'bhm'))
    param_uc = setOptions_bfda('smethod', 'bhm', 'cgrid', 0, 'mat', stat_cov_bool_a,...
                                 'pace', 1, 'M', 20000, 'Burnin', 3000, 'w', w_a, 'ws', ws_a);
else % meth_a==babf
    eval_grid_ns = [ageMin_a:ageMax_a]; 
    param_uc = setOptions_bfda('smethod', 'babf', 'cgrid', 0, 'mat', stat_cov_bool_a,...
                               'm', 3, 'eval_grid', eval_grid_ns, 'Burnin', 3000, 'M', 20000, 'ws', ws_a);
end

[out_ucgrid, param_uc] = BFDA(data_bfda.bmis, data_bfda.ages, param_uc, log_transform_a);

Nobs= size(nhanes_loc, 1);
Nsub= length(seqn_unique);

end
