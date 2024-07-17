
% define character parameters for file names and emails : deb_bmi_bool etc.  
% are defined in the command line of the R script that calls matlab

if (deb_bmi_nb == 2)
    bmi_char='bmiDeb2_'; % debiased bmis
    bmi_mail = 'debiased BMIs (version 2)';
elseif (deb_bmi_nb == 1)
    bmi_char='bmiDeb1_'; % debiased bmis
    bmi_mail = 'debiased BMIs (version 1)';
else
    bmi_char='bmiUncor_'; % unccorected bmis
    bmi_mail = 'uncorrected BMIs';
end

if (stat_cov_bool) 
    cov_char='stat';
else
    cov_char='nonstat';
end

if (log_transform) 
    log_transform_char='log'; 
else
    log_transform_char='no_log';
end

w_char = sprintf('%.1f', w) ;
ws_char = sprintf('%.1f', ws) ;

ageMin_char=sprintf('%d', ageMin);
ageMax_char=sprintf('%d', ageMax);

minCoh_char=sprintf('%d', cohortMin-1900);
maxCoh_char=sprintf('%d', cohortMax-1900);

