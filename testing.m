function [temp] = testing(zip_file)
    addpath('KINARMAnalysisScriptsv3');
%     zip_file = strcat(data_directory,'\',zip_file);
%     fprintf('\n \n \n \n \n');
%     fprintf(zip_file);
%     dir
%     fprintf('\n \n \n \n \n');
    out = KINARM_add_hand_kinematics(zip_load(zip_file));
    out = filter_double_pass(out, 'enhanced', 'fc', 10);
    
    for k = 1:length(out.c3d)
        trials{k} = strcat('trial_',string(k));
        eval(strcat('temp.trial_',string(k),'=out.c3d(',string(k),');'));
    end
    for k = 1:length(out.c3d)
%         x = diff23f5(temp.(trials{k}).Right_HandX,1/1000,10);
%         y = diff23f5(temp.(trials{k}).Right_HandY,1/1000,10);
        
        temp.(trials{k}).Right_HandVel =...
            sqrt(temp.(trials{k}).Right_HandXVel.^2+...
                 temp.(trials{k}).Right_HandYVel.^2);
             
        temp.(trials{k}).Right_HandAcc =...
            sqrt(temp.(trials{k}).Right_HandXAcc.^2+...
                 temp.(trials{k}).Right_HandYAcc.^2);
    end
end