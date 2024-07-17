
% set to default values for parameters not specified
if (~exist('method')) method='bhm';, end
if (~exist('deb_bmi_nb')) deb_bmi_nb=2;, end
if (~exist('stat_cov_bool')) stat_cov_bool=1;, end
if (~exist('log_transform')) log_transform=false;, end 

% Add pathes of the required MATLAB packages
addtoolboxes

% set email properties : detail of sender + adress_receiver
% mail_details

% define character parameters for file names and emails
character_variables

dir_data=['../../data/for_matlab/' ...
    typeStrata ...
    '/ages' ageMin_char '_' ageMax_char ...
    '/cohorts'  minCoh_char '_' maxCoh_char '/'];
dir_results=['../../results/' ...
    typeStrata ...
    '/ages' ageMin_char '_' ageMax_char ...
    '/cohorts'  minCoh_char '_' maxCoh_char '/'...
    method '/' log_transform_char '/'];

% core message for emails
% core_mail_message = [method ' method with assumption of '...
%     cov_char 'ionnary covariance and w = ' w_char ...
%     ' and ws = ' ws_char ' applied on ' bmi_mail] ;

% Strata is a struct array
Strata = dir([dir_data, '*.csv']);

% number of models = number of strata
Nmodels = length(Strata);

M = floor(Nmodels/2); parpool(M);

parfor I=1:Nmodels
    
    % placed here for transparency issues
    param_info = [bmi_char, cov_char, 'Cov', '_w', w_char, '_ws', ws_char];
    
    short_dir = [param_info, '_short'];
    long_dir =  [param_info, '_long'];
    
    short_dir_full = [dir_results, short_dir];
    long_dir_full = [dir_results, long_dir];
    
    % remove the extension from data file name
    stratum_loc = regexprep(Strata(I).name, '.csv', '');
    
    % create folders to store individuals results
    mkdir(short_dir_full, stratum_loc)
    mkdir(long_dir_full, stratum_loc)
    
    [ Nsub, Nobs, out_ucgrid, param_uc ] = run_BFDA(stratum_loc, dir_data,...
        deb_bmi_nb, stat_cov_bool, w, ws, method, ageMin, ageMax, log_transform) ;
    
    P1 = [long_dir_full, '\', stratum_loc, '\'];
    P2 = [short_dir_full, '\', stratum_loc, '\'];
    
    export_results(out_ucgrid, param_uc, P1, P2)
    
    Nsub = sprintf('%d', Nsub);
    Nobs = sprintf('%d', Nobs);
    
    disp(['............ Strata ', stratum_loc, ' end run'])
    
%     props = java.lang.System.getProperties;
%     props.setProperty('mail.smtp.auth','true');
%     props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
%     props.setProperty('mail.smtp.socketFactory.port','465');
%     
%     mail_title = [param_info, '- Model for ', stratum_loc, ' has been fitted'];
%     mail_message = [core_mail_message 10 ...
%         10 ...
%         'The analysis uses all NHANES waves and is restricted to those :' 10 ...
%         '- aged', ageMin_char, '-', ageMax_char, 'at screening' 10 ...
%         'and' 10 ...
%         '- born 19', minCoh_char, '-19' maxCoh_char '.' 10 ...
%         10 ...
%         'In stratum :'  10 ...
%         '- ' Nsub ' subjects ;' 10 ...
%         '- ' Nobs ' observations.' ];
%     sendmail(address_receiver, mail_title, mail_message);
    
end

delete(gcp('nocreate'))

% pause(10)

% props = java.lang.System.getProperties;
% props.setProperty('mail.smtp.auth','true');
% props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
% props.setProperty('mail.smtp.socketFactory.port','465');
% 
% sendmail('nicolasphilippetodd@gmail.com', ...
%     ['All models have been run for cohorts 19', minCoh_char, '-19' maxCoh_char], ...
%     core_mail_message);
