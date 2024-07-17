function [ ] = export_results(out_ucgrid_a, param_uc_a, prefix1_a, prefix2_a)

%% save results
% save all MCMC samples by age (shortest of the three dimensions of Zout) :
% each file contains Subject x MCMC gaussian process values for one given age

for i=1:size(out_ucgrid_a.Zout, 1) % along the age dimension
    
    name_loc = sprintf('Zout_age%d.csv', param_uc_a.tau(i)) ;
    
    csvwrite( [prefix1_a, name_loc], ...
        squeeze(out_ucgrid_a.Zout(i,:,:))) ;
end

clear name_loc i ;

%%%%%%%%%%% save other outputs of BFDA in folder BFDA_results

save([prefix2_a, 'param_uc.mat'], 'param_uc_a');

csvwrite([prefix2_a, 'Z.csv'], out_ucgrid_a.Z);
csvwrite([prefix2_a, 'Z_CL.csv'], out_ucgrid_a.Z_CL);
csvwrite([prefix2_a, 'Z_UL.csv'], out_ucgrid_a.Z_UL);

out_ucgrid_reduced = rmfield(out_ucgrid_a, {'Z', 'Z_CL', 'Z_UL', 'Zout'});

save([prefix2_a, 'out_ucgrid_reduced.mat'], 'out_ucgrid_reduced');


