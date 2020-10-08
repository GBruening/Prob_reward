%% load data
%data01 = zip_load('762284083_2019-04-04_10-01-06_4T_Probc_Dan');
%data01 = zip_load('762284083_2019-04-02_08-58-56_4T_Probc_YuchengG');
%data01 = zip_load('762284083_2019-04-03_17-41-25_4T_Probc_LuF');
%data01 = zip_load('762284083_2019-04-10_16-36-40_4T_Probc_Copper');
%data01 = zip_load('762284083_2019-05-17_09-27-23_4T_Probc_Tuddy_DF'); %p2
%data01 = zip_load('762284083_2019-05-20_10-13-14_4T_Probc_Stephen'); % p3
%data01 = zip_load('762284083_2019-05-20_13-26-45_4T_Probc_Theresa'); % p2
data01 = zip_load('762284083_2019-05-21_13-08-20_4T_Probc_Clarissa'); % p3
%data01 = zip_load('762284083_2019-05-21_15-38-02_4T_Probc_Laura'); % p2
%data01 = zip_load('762284083_2019-05-30_10-17-55_4T_Probc_Hannah'); % p3

%% peakV
frame_rate = data01.c3d(1).HAND.RATE;
TPlist = 8;
BTlist = 4;
h = 1/frame_rate;
fc = 25; % 40
trial_in_block = data01.c3d(1).BLOCK_TABLE.LIST_REPS(2)*TPlist;
base_LP = data01.c3d(1).BLOCK_TABLE.LIST_REPS(1);
base_BN = data01.c3d(1).BLOCK_TABLE.BLOCK_REPS(1);
base_num = base_LP*base_BN*BTlist;
trial_num_b = length(data01.c3d);
trial_num_t = trial_num_b-base_num;
% round, ceil, floor, fix
block_num = sum(data01.c3d(1).BLOCK_TABLE.BLOCK_REPS(2:5));
Index_Egg = zeros(trial_in_block,block_num);
First_Egg = zeros(trial_in_block,block_num);
First_NO_Egg = zeros(trial_in_block,block_num);
Second_Egg = zeros(trial_in_block,block_num);
Second_NO_Egg = zeros(trial_in_block,block_num);
Third_Egg = zeros(trial_in_block,block_num);
Third_NO_Egg = zeros(trial_in_block,block_num);
Fouth_Egg = zeros(trial_in_block,block_num);
Fouth_NO_Egg = zeros(trial_in_block,block_num);
EachT = floor((BTlist*base_LP)/2);
base_index = zeros(EachT,4);
baseE = zeros(5,1);
Miss_Egg = zeros(trial_in_block,block_num);
Miss_base = zeros((BTlist*base_LP),2);
Error_In = zeros(trial_in_block,block_num);
Error_base = zeros((BTlist*base_LP),2);
Pause_In = zeros(trial_in_block,block_num);
pv = zeros(trial_in_block, block_num);
pv_base = zeros((BTlist*base_LP),2);
Slow_pv = zeros(trial_in_block,block_num);
Slow_pvba = zeros((BTlist*base_LP),2);
Egg_Index = zeros(1,11);
Pause = 0; Ftarg1_R = 0; Ftarg2_R = 0; Ftarg3_R = 0; Ftarg4_R = 0;
Mtarg = 0; Ftarg1_N = 0; Ftarg2_N = 0; Ftarg3_N = 0; Ftarg4_N = 0;
V_raw = cell(trial_num_t,1);   % block_num*trial_in_block
D_raw = cell(trial_num_t,1);
A_raw = cell(trial_num_t,1);
hand_x = cell(trial_num_t,1);
hand_y = cell(trial_num_t,1);
bn = base_num;

%%
b1 = 0;b2 = 0;b3 = 0;b4 = 0;
for k = 1:base_BN
    for i = (1+(k-1)*BTlist*base_LP):(k*BTlist*base_LP)
        if length(data01.c3d(i).EVENTS.LABELS) >= 4
            event = deblank(cell2mat(data01.c3d(i).EVENTS.LABELS(4)));
        else
            event = [];
        end
        baseE(1) = strcmp(event,'Target1_missR');
        baseE(2) = strcmp(event,'Target2_missR');
        baseE(3) = strcmp(event,'Target3_missR');
        baseE(4) = strcmp(event,'Target4_missR');
        baseE(5) = strcmp(event,'MISS_TARGET');
        
        if any(baseE(1))
            b1 = b1+1;
            base_index(b1,1) = i;
        elseif any(baseE(2))
            b2 = b2+1;
            base_index(b2,2) = i;
        elseif any(baseE(3))
            b3 = b3+1;
            base_index(b3,3) = i;
        elseif any(baseE(4))
            b4 = b4+1;
            base_index(b4,4) = i;
        elseif any(baseE(5))
            Miss_base((i-(k-1)*BTlist*base_LP),k) = i;
        else
            Error_base((i-(k-1)*BTlist*base_LP),k) = i;
        end
        
        xdata = data01.c3d(i).Right_HandX;
        ydata = data01.c3d(i).Right_HandY;
    
        x_posHME = data01.c3d(i).TARGET_TABLE.X_GLOBAL(1) * 0.01;
        y_posHME = data01.c3d(i).TARGET_TABLE.Y_GLOBAL(1) * 0.01;
   
        x1 = (xdata-x_posHME) .* 100;
        y1 = (ydata-y_posHME) .* 100;

        dist = sqrt( x1.^2 + y1.^2 );
    
        vout = diff23f5_gai(dist, h, fc );
        
        pv_base((i-(k-1)*BTlist*base_LP),k) = max(vout(:,2));
        if pv_base((i-(k-1)*BTlist*base_LP),k) < 30
            Slow_pvba((i-(k-1)*BTlist*base_LP),k) = i;
        end
    end
end

%%
PeakV_B = zeros(EachT,4);
for b1 = 1:4
    for b2 = 1:EachT
        if any(base_index(b2,b1))
            if base_index(b2,b1) <= BTlist*base_LP
                b3 = 1;
            else
                b3 = 2;
            end
            PeakV_B(b2,b1) = ...
                pv_base((base_index(b2,b1)-(b3-1)*BTlist*base_LP),b3);
        end
    end
end

%%
%mean_base = zeros(4,1);
%mean_base(1) = sum(PeakV_B(6:10,1))/5;
%mean_base(2) = sum(PeakV_B(6:10,2))/5;
%mean_base(3) = sum(PeakV_B(6:10,3))/5;
%mean_base(4) = sum(PeakV_B(6:10,4))/5;

%%
mean_base = zeros(4,1);
mean_base(1) = sum(PeakV_B(:,1))/EachT;
mean_base(2) = sum(PeakV_B(:,2))/EachT;
mean_base(3) = sum(PeakV_B(:,3))/EachT;
mean_base(4) = sum(PeakV_B(:,4))/EachT;

%%
for k = 1:block_num
    for trial = (bn+1+(k-1)*trial_in_block):(bn+k*trial_in_block)
         if length(data01.c3d(trial).EVENTS.LABELS) >= 4
           event = deblank(cell2mat(data01.c3d(trial).EVENTS.LABELS(4)));
           if length((data01.c3d(trial).EVENTS.LABELS)) == 5
               event_P = deblank(cell2mat(data01.c3d(trial).EVENTS.LABELS(5)));
           else
               event_P = 'No';
           end
           Ftarg1_R = strcmp(event,'Target1_getR');
           Ftarg1_N = strcmp(event,'Target1_missR');
           Ftarg2_R = strcmp(event,'Target2_getR');
           Ftarg2_N = strcmp(event,'Target2_missR');
           Ftarg3_R = strcmp(event,'Target3_getR');
           Ftarg3_N = strcmp(event,'Target3_missR');
           Ftarg4_R = strcmp(event,'Target4_getR');
           Ftarg4_N = strcmp(event,'Target4_missR');
           Mtarg = strcmp(event,'MISS_TARGET');
           Pause = strcmp(event_P,'TASK_PAUSED');
        else
           event = [];
           E_trial = 1;  % incorrect trial
        end
        
        if any(Pause)
            Pause_In((trial-bn-(k-1)*trial_in_block),k) = trial;
            Egg_Index(11) = Egg_Index(11)+1;
            Pause = 0;
        end
    
        if any(Mtarg)
           Miss_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(9) = Egg_Index(9)+1;
           Mtarg = 0;
        elseif any(Ftarg1_R)  
           Index_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           First_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(1) = Egg_Index(1)+1;
           Ftarg1_R = 0;
        elseif any(Ftarg1_N)
           First_NO_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(2) = Egg_Index(2)+1;
           Ftarg1_N = 0;
        elseif any(Ftarg2_R)
           Index_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Second_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(3) = Egg_Index(3)+1;
           Ftarg2_R = 0;
        elseif any(Ftarg2_N)
           Second_NO_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(4) = Egg_Index(4)+1;
           Ftarg2_N = 0;
        elseif any(Ftarg3_R)
           Index_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Third_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(5) = Egg_Index(5)+1;
           Ftarg3_R = 0;
        elseif any(Ftarg3_N)
           Third_NO_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(6) = Egg_Index(6)+1;
           Ftarg3_N = 0;
        elseif any(Ftarg4_R)
           Index_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Fouth_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(7) = Egg_Index(7)+1;
           Ftarg4_R = 0;
        elseif any(Ftarg4_N)
           Fouth_NO_Egg((trial-bn-(k-1)*trial_in_block),k) = trial;
           Egg_Index(8) = Egg_Index(8)+1;
           Ftarg4_N = 0;
        else
            if any(E_trial)
                Error_In((trial-bn-(k-1)*trial_in_block),k) = trial;
                Egg_Index(10) = Egg_Index(10)+1;
                E_trial = 0;
            end
        end
    
        xdata = data01.c3d(trial).Right_HandX;
        ydata = data01.c3d(trial).Right_HandY;
    
        x_posHME = data01.c3d(trial).TARGET_TABLE.X_GLOBAL(1) * 0.01;
        y_posHME = data01.c3d(trial).TARGET_TABLE.Y_GLOBAL(1) * 0.01;
   
        x1 = (xdata-x_posHME) .* 100;
        y1 = (ydata-y_posHME) .* 100;

        dist = sqrt( x1.^2 + y1.^2 );
    
        vout = diff23f5_gai(dist, h, fc );
        
        hand_x(trial-bn) = {xdata};
        hand_y(trial-bn) = {ydata};
        D_raw(trial-bn) = {vout(:,1)};
        V_raw(trial-bn) = {vout(:,2)}; 
        A_raw(trial-bn) = {vout(:,3)};
        
        pv((trial-bn-(k-1)*trial_in_block),k) = max(vout(:,2));
        if pv((trial-bn-(k-1)*trial_in_block),k) < 30
            Slow_pv((trial-bn-(k-1)*trial_in_block),k) = trial;
        end
    end
end
%% data storing
%A = [1 1;0 1];
%B = [1 0;0 1];
%out.new = B;   % add now field to the structure
%value1 = {A;B};   % cell
%out3 = struct(field,value1); % struct 1*2

Test_Details = struct;
Test_Details.Block_Rep = data01.c3d(1).BLOCK_TABLE.BLOCK_REPS(1:4,1);
Test_Details.Block_num = block_num;
Test_Details.TP_List = data01.c3d(1).BLOCK_TABLE.TP_LIST;
Test_Details.TP_List_Rep = data01.c3d(1).BLOCK_TABLE.LIST_REPS;
Test_Details.Trial_P_Block = trial_in_block;
Test_Details.Trial_num = trial_num;
Test_Details.Frame_Rate = frame_rate;
Test_Details.Origin_X_Global = data01.c3d(1).TARGET_TABLE.X_GLOBAL(1);
Test_Details.Origin_Y_Global = data01.c3d(1).TARGET_TABLE.Y_GLOBAL(1);

events = cell(trial_num,1);
for i = 1:trial_num
    events{i,1} = data01.c3d(i).EVENTS;
end

Raw_data = struct;
Raw_data.Hand_Pos_X = hand_x;
Raw_data.Hand_Pos_Y = hand_y;
Raw_data.Distance = D_raw;
Raw_data.Velociey = V_raw;
Raw_data.Acceleration = A_raw;
Raw_data.Events = events;

Dynamic_Property = struct;
Dynamic_Property.PeakV = pv;
Dynamic_Property.Raction_Time = Reaction_T;
Dynamic_Property.Starting_Acc = Start_A;
Dynamic_Property.Move_Duration = Reach_D;
Dynamic_Property.Trial_Index = Egg_Index;
Dynamic_Property.Reward_Trial_Index = Index_Egg;
Dynamic_Property.Target1_Reward = First_Egg;
Dynamic_Property.Target1_NO_Reward = First_NO_Egg;
Dynamic_Property.Target2_Reward = Second_Egg;
Dynamic_Property.Target2_NO_Reward = Second_NO_Egg;
Dynamic_Property.Target3_Reward = Third_Egg;
Dynamic_Property.Target3_NO_Reward = Third_NO_Egg;
Dynamic_Property.Target4_Reward = Fouth_Egg;
Dynamic_Property.Target4_NO_Reward = Fouth_NO_Egg;
Dynamic_Property.MissTarget = Miss_Egg;
Dynamic_Property.PausedTrial = Pause_In;
Dynamic_Property.ErrorTrial = Error_In; 

E_Pc_1 = {'Test Parameters',Test_Details; 'Raw data',Raw_data;...
    'Dynamic properties',Dynamic_Prop};
E_Pc_2 = cell(3,2);E_Pc_3 = cell(3,2);E_Pc_4 = cell(3,2);
E_Pc_5 = cell(3,2);

E_Ori_1 = cell(3,2);E_Ori_2 = cell(3,2);E_Ori_3 = cell(3,2);
E_Ori_4 = cell(3,2);E_Ori_5 = cell(3,2);

E_Vc_1 = cell(3,2);E_Vc_2 = cell(3,2);E_Vc_3 = cell(3,2);
E_Vc_4 = cell(3,2);E_Vc_5 = cell(3,2);

EXP_4T_PROBC = struct;
EXP_4T_PROBC.experiment01 = E_Pc_1;
EXP_4T_PROBC.experiment02 = E_Pc_2;
EXP_4T_PROBC.experiment03 = E_Pc_3; 
EXP_4T_PROBC.experiment04 = E_Pc_4;
EXP_4T_PROBC.experiment05 = E_Pc_5; 

EXP_4T_ORIGIN = struct;
EXP_4T_ORIGIN.experiment01 = E_Ori_1;
EXP_4T_ORIGIN.experiment02 = E_Ori_2;
EXP_4T_ORIGIN.experiment03 = E_Ori_3;
EXP_4T_ORIGIN.experiment04 = E_Ori_4;
EXP_4T_ORIGIN.experiment05 = E_Ori_5;

EXP_4T_VALUEC = struct;
EXP_4T_VALUEC.experiment01 = E_Vc_1;
EXP_4T_VALUEC.experiment02 = E_Vc_2;
EXP_4T_VALUEC.experiment03 = E_Vc_3;
EXP_4T_VALUEC.experiment04 = E_Vc_4;
EXP_4T_VALUEC.experiment05 = E_Vc_5;

D_KehanY = cell(10,2);
D_KehanY{1,1:2} = {'Exp 4T Origin',EXP_4T_ORIGIN};
D_KehanY{2,1:2} = {'Exp 4T Probc', EXP_4T_PROBC};
D_KehanY{3,1:2} = {'Exp 4T Valuec', EXP_4T_VALUEC};

%% movement properties
% PeakV
%%
Reaction_T = zeros(trial_num_t,1);
RT = 0; m1 = 0;
for i = 1:trial_num_t
    T_targon = floor((data01.c3d(i+bn).EVENTS.TIMES(2))*1000);
    T_excur = floor((data01.c3d(i+bn).EVENTS.TIMES(3))*1000);
    if V_raw{i,1}(T_targon) >= 0.7
        for n = T_targon:1:T_excur
            if V_raw{i,1}(n) < 2.8  % 0.7*4
                m1 = m1+1;
                if A_raw{i,1}(n) > 10
                    RT = m1;
                end
            end
        end
        Reaction_T(i) = RT;
        m1 = 0;
    elseif V_raw{i,1}(T_targon) < 0.7
        for n = T_targon:1:T_excur
            if V_raw{i,1}(n) > 0.5 && V_raw{i,1}(n) < 2
                % criterian can be |0.35~0.5|
                m1 = m1+1;
                if A_raw{i,1}(n) > 10
                    RT = n;
                end
            end
        end
        Reaction_T(i) = RT-m1+1-T_targon;
        m1 = 0;
    else
        m1 = 0;
    end
end
Reaction_T = reshape(Reaction_T,[trial_in_block,block_num]);
RT_Ori = Reaction_T;
%%
ab_reactt = zeros(trial_in_block,block_num);
for i = 1:block_num
    for j = 1:trial_in_block
        if Reaction_T(j,i) < 80 || Reaction_T(j,i) > 600
            ab_reactt(j,i) = Reaction_T(j,i);
        end
    end
end

%% max excursion
Max_Ex = zeros(trial_num_t,1); 
for i = 1:trial_num_t
    T_targon = floor((data01.c3d(i+bn).EVENTS.TIMES(2))*1000)+1;
    a = length(D_raw{i,1})-30;
    Max_Ex(i) = max(D_raw{i,1}(T_targon:a,1));
end
Max_Ex = reshape(Max_Ex,[trial_in_block,block_num]);
%%
ab_MaE = zeros(trial_in_block,block_num);
for i = 1:block_num
    for j = 1:trial_in_block
        if Max_Ex(j,i) < 9.5 || Max_Ex(j,i) > 20
            ab_MaE(j,i) = Max_Ex(j,i);
        end
    end
end
%%
M1_repl = sum(Max_Ex(2:11,3))/10;
%M2_repl = sum(Max_Ex(2:11,5))/10;
M3_repl = sum(Max_Ex(2:11,7))/10;
M4_repl = sum(Max_Ex(2:11,1))/10;
M5_repl = sum(Max_Ex(3:12,5))/10;
M6_repl = sum(Max_Ex(38:47,4))/10;

Max_Ex(1,3) = M1_repl; %Max_Ex(1,5) = M2_repl;
Max_Ex(1,7) = M3_repl; Max_Ex(1,1) = M4_repl;
Max_Ex(2,5) = M5_repl; Max_Ex(48,4) = M6_repl;

%% 10<D<10.1 min(T) reach duration
Reach_D = zeros(trial_num_t,1); % raw, including reaction time
RD = 0; m2 = 0;
for i = 1:trial_num_t
    T_targon = floor((data01.c3d(i+bn).EVENTS.TIMES(2))*1000)+1;
    for n = T_targon:1:(length(D_raw{i,1})-50)
        if D_raw{i,1}(n) > 10 && D_raw{i,1}(n) < 10.3 ...
                && V_raw{i,1}(n) > 0
            m2 = m2+1;
            RD = n;
        end
    end
    Reach_D(i) = RD-m2+1-T_targon;
    m2 = 0;
end
Reach_D = reshape(Reach_D,[trial_in_block,block_num]);
Reach_D_net = Reach_D-RT_Ori;
%%
ab_reD = zeros(trial_in_block,block_num);
for i = 1:block_num
    for j = 1:trial_in_block
        if Reach_D_net(j,i) < 100 || Reach_D_net(j,i) > 650
            ab_reD(j,i) = Reach_D_net(j,i);
        end
    end
end

%% A starting Acceleration
Start_A = zeros(trial_num_t,1);
for i = 1:trial_num_t
    T_targon = floor((data01.c3d(i+bn).EVENTS.TIMES(2))*1000);
    m3 = Reaction_T(i)+T_targon+2;
    Start_A(i) = A_raw{i,1}(m3); % not sure
end

% time limit - back peak V
%% check pv first, find slow PV, then change code below
T1_repl = sum(pv(2:11,1))/10;
T2_repl = sum(pv(2:11,3))/10;
T3_repl = sum(pv(3:12,5))/10;
T4_repl = sum(pv(2:11,7))/10;
pv2 = pv;
pv2(1,1) = T1_repl;
pv2(1,3) = T2_repl;
pv2(2,5) = T3_repl;
pv2(1,7) = T4_repl;

%%
%pv0 = pv2;
pv0 = PV_Avg.Theresa.PVraw;
n = floor(trial_in_block/4);
mm = floor(trial_in_block/8);
PV_100 = zeros(n,block_num); PV_50 = zeros(n,block_num);
PV_50no = zeros(n,block_num); PV_0 = zeros(n,block_num);
ReacT_100 = zeros(n,block_num); ReacT_50 = zeros(n,block_num);
ReacT_50no = zeros(n,block_num); ReacT_0 = zeros(n,block_num);
RD_100 = zeros(n,block_num); RD_50 = zeros(n,block_num);
RD_50no = zeros(n,block_num); RD_0 = zeros(n,block_num);
ME_100 = zeros(n,block_num); ME_50 = zeros(n,block_num);
ME_50no = zeros(n,block_num); ME_0 = zeros(n,block_num);

s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;

%rand_targ_4T_8trial_Probc01;
for b1 = 1:2      % 11263788
    for b2 = 1:trial_in_block
        if any(First_Egg(b2,b1))
            s1 = s1+1;
            PV_100(s1,b1) = pv0(b2,b1);
            ReacT_100(s1,b1) = Reaction_T(b2,b1);
            RD_100(s1,b1) = Reach_D_net(b2,b1);
            ME_100(s1,b1) = Max_Ex(b2,b1);
        elseif any(Second_Egg(b2,b1))
            s2 = s2+1;
            PV_50(s2,b1) = pv0(b2,b1);
            ReacT_50(s2,b1) = Reaction_T(b2,b1);
            RD_50(s2,b1) = Reach_D_net(b2,b1);
            ME_50(s2,b1) = Max_Ex(b2,b1);
        elseif any(Third_Egg(b2,b1))
            s3 = s3+1;
            PV_50(s3+mm,b1) = pv0(b2,b1);
            ReacT_50(s3+mm,b1) = Reaction_T(b2,b1);
            RD_50(s3+mm,b1) = Reach_D_net(b2,b1);
            ME_50(s3+mm,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_NO_Egg(b2,b1))
            s4 = s4+1;
            PV_0(s4,b1) = pv0(b2,b1);
            ReacT_0(s4,b1) = Reaction_T(b2,b1);
            RD_0(s4,b1) = Reach_D_net(b2,b1);
            ME_0(s4,b1) = Max_Ex(b2,b1);
        elseif any(Second_NO_Egg(b2,b1))
            s6 = s6+1;
            PV_50no(s6,b1) = pv0(b2,b1);
            ReacT_50no(s6,b1) = Reaction_T(b2,b1);
            RD_50no(s6,b1) = Reach_D_net(b2,b1);
            ME_50no(s6,b1) = Max_Ex(b2,b1);
        elseif any(Third_NO_Egg(b2,b1))
            s7 = s7+1;
            PV_50no(s7+mm,b1) = pv0(b2,b1);
            ReacT_50no(s7+mm,b1) = Reaction_T(b2,b1); 
            RD_50no(s7+mm,b1) = Reach_D_net(b2,b1);
            ME_50no(s7+mm,b1) = Max_Ex(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
end

s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
for b1 = 3:1:4     % 33154866
    for b2 = 1:trial_in_block
        if any(First_Egg(b2,b1))
            s1 = s1+1;
            PV_50(s1,b1) = pv0(b2,b1);
            ReacT_50(s1,b1) = Reaction_T(b2,b1);
            RD_50(s1,b1) = Reach_D_net(b2,b1);
            ME_50(s1,b1) = Max_Ex(b2,b1);
        elseif any(Second_NO_Egg(b2,b1))
            s2 = s2+1;
            PV_0(s2,b1) = pv0(b2,b1);
            ReacT_0(s2,b1) = Reaction_T(b2,b1);
            RD_0(s2,b1) = Reach_D_net(b2,b1);
            ME_0(s2,b1) = Max_Ex(b2,b1);
        elseif any(Third_Egg(b2,b1))
            s3 = s3+1;
            PV_100(s3,b1) = pv0(b2,b1);
            ReacT_100(s3,b1) = Reaction_T(b2,b1);
            RD_100(s3,b1) = Reach_D_net(b2,b1);
            ME_100(s3,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_Egg(b2,b1))
            s4 = s4+1;
            PV_50(s4+mm,b1) = pv0(b2,b1);
            ReacT_50(s4+mm,b1) = Reaction_T(b2,b1);
            RD_50(s4+mm,b1) = Reach_D_net(b2,b1);
            ME_50(s4+mm,b1) = Max_Ex(b2,b1);
        elseif any(First_NO_Egg(b2,b1))
            s6 = s6+1;
            PV_50no(s6,b1) = pv0(b2,b1);
            ReacT_50no(s6,b1) = Reaction_T(b2,b1);
            RD_50no(s6,b1) = Reach_D_net(b2,b1);
            ME_50no(s6,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_NO_Egg(b2,b1))
            s7 = s7+1;
            PV_50no(s7+mm,b1) = pv0(b2,b1);
            ReacT_50no(s7+mm,b1) = Reaction_T(b2,b1);
            RD_50no(s7+mm,b1) = Reach_D_net(b2,b1);
            ME_50no(s7+mm,b1) = Max_Ex(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
end

s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
for b1 = 5:1:6      % 44261577
    for b2 = 1:trial_in_block
        if any(First_Egg(b2,b1))
            s1 = s1+1;
            PV_50(s1,b1) = pv0(b2,b1);
            ReacT_50(s1,b1) = Reaction_T(b2,b1);
            RD_50(s1,b1) = Reach_D_net(b2,b1);
            ME_50(s1,b1) = Max_Ex(b2,b1);
        elseif any(Second_Egg(b2,b1))
            s2 = s2+1;
            PV_50(s2+mm,b1) = pv0(b2,b1);
            ReacT_50(s2+mm,b1) = Reaction_T(b2,b1);
            RD_50(s2+mm,b1) = Reach_D_net(b2,b1);
            ME_50(s2+mm,b1) = Max_Ex(b2,b1);
        elseif any(Third_NO_Egg(b2,b1))
            s3 = s3+1;
            PV_0(s3,b1) = pv0(b2,b1);
            ReacT_0(s3,b1) = Reaction_T(b2,b1);
            RD_0(s3,b1) = Reach_D_net(b2,b1);
            ME_0(s3,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_Egg(b2,b1))
            s4 = s4+1;
            PV_100(s4,b1) = pv0(b2,b1);
            ReacT_100(s4,b1) = Reaction_T(b2,b1);
            RD_100(s4,b1) = Reach_D_net(b2,b1);
            ME_100(s4,b1) = Max_Ex(b2,b1);
        elseif any(First_NO_Egg(b2,b1))
            s6 = s6+1;
            PV_50no(s6,b1) = pv0(b2,b1);
            ReacT_50no(s6,b1) = Reaction_T(b2,b1);
            RD_50no(s6,b1) = Reach_D_net(b2,b1);
            ME_50no(s6,b1) = Max_Ex(b2,b1);
        elseif any(Second_NO_Egg(b2,b1))
            s7 = s7+1;
            PV_50no(s7+mm,b1) = pv0(b2,b1);
            ReacT_50no(s7+mm,b1) = Reaction_T(b2,b1);
            RD_50no(s7+mm,b1) = Reach_D_net(b2,b1);
            ME_50no(s7+mm,b1) = Max_Ex(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
end

s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
for b1 = 7:1:8      % 22483755
    for b2 = 1:trial_in_block
        if any(First_NO_Egg(b2,b1))
            s1 = s1+1;
            PV_0(s1,b1) = pv0(b2,b1);
            ReacT_0(s1,b1) = Reaction_T(b2,b1);
            RD_0(s1,b1) = Reach_D_net(b2,b1);
            ME_0(s1,b1) = Max_Ex(b2,b1);
        elseif any(Second_Egg(b2,b1))
            s2 = s2+1;
            PV_100(s2,b1) = pv0(b2,b1);
            ReacT_100(s2,b1) = Reaction_T(b2,b1);
            RD_100(s2,b1) = Reach_D_net(b2,b1);
            ME_100(s2,b1) = Max_Ex(b2,b1);
        elseif any(Third_Egg(b2,b1))
            s3 = s3+1;
            PV_50(s3,b1) = pv0(b2,b1);
            ReacT_50(s3,b1) = Reaction_T(b2,b1);
            RD_50(s3,b1) = Reach_D_net(b2,b1);
            ME_50(s3,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_Egg(b2,b1))
            s4 = s4+1;
            PV_50(s4+mm,b1) = pv0(b2,b1);
            ReacT_50(s4+mm,b1) = Reaction_T(b2,b1);
            RD_50(s4+mm,b1) = Reach_D_net(b2,b1);
            ME_50(s4+mm,b1) = Max_Ex(b2,b1);
        elseif any(Third_NO_Egg(b2,b1))
            s6 = s6+1;
            PV_50no(s6,b1) = pv0(b2,b1);
            ReacT_50no(s6,b1) = Reaction_T(b2,b1);
            RD_50no(s6,b1) = Reach_D_net(b2,b1);
            ME_50no(s6,b1) = Max_Ex(b2,b1);
        elseif any(Fouth_NO_Egg(b2,b1))
            s7 = s7+1;
            PV_50no(s7+mm,b1) = pv0(b2,b1);
            ReacT_50no(s7+mm,b1) = Reaction_T(b2,b1);
            RD_50no(s7+mm,b1) = Reach_D_net(b2,b1);
            ME_50no(s7+mm,b1) = Max_Ex(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s2=0;s3=0;s4=0;s6=0;s7=0;
end

%%
tinn = zeros(block_num,3);  % 100,50,0
n1=0;n2=0;n3=0;n4=0;
% check missed trial
for b3 = 1:block_num
    for b4 = 1:n
        if any(PV_100(b4,b3))
            n1 = n1+1;
        end
        
        if any(PV_50(b4,b3))
            n2 = n2+1;
        end
        
        if any(PV_0(b4,b3))
            n3 = n3+1;
        end
        
        if any(PV_50no(b4,b3))
            n4 = n4+1;
        end
    end
    tinn(b3,1) = n1; tinn(b3,2) = n2; tinn(b3,3) = n3; tinn(b3,4) = n4;
    n1=0;n2=0;n3=0;n4=0;
end

%%
n = floor(trial_in_block/4);
pv0 = PV_Avg.Hannah.PVraw;
Reaction_T = PV_Avg.Hannah.RTraw;
pv_A = zeros(n,block_num); pv_B = zeros(n,block_num);
pv_C = zeros(n,block_num); pv_D = zeros(n,block_num);
rt_A = zeros(n,block_num); rt_B = zeros(n,block_num);
rt_C = zeros(n,block_num); rt_D = zeros(n,block_num);
c1=0; c2=0; c3=0; c4=0;
for i = 1:block_num
    for j = 1:trial_in_block
        if any(First_Egg(j,i))
            c1 = c1+1;
            pv_A(c1,i) = pv0(j,i);
            rt_A(c1,i) = Reaction_T(j,i);
        elseif any(First_NO_Egg(j,i))
            c1 = c1+1;
            pv_A(c1,i) = pv0(j,i);
            rt_A(c1,i) = Reaction_T(j,i);
        elseif any(Second_Egg(j,i))
            c2 = c2+1;
            pv_B(c2,i) = pv0(j,i);
            rt_B(c2,i) = Reaction_T(j,i);
        elseif any(Second_NO_Egg(j,i))
            c2 = c2+1;
            pv_B(c2,i) = pv0(j,i);
            rt_B(c2,i) = Reaction_T(j,i);
        elseif any(Third_Egg(j,i))
            c3 = c3+1;
            pv_C(c3,i) = pv0(j,i);
            rt_C(c3,i) = Reaction_T(j,i);
        elseif any(Third_NO_Egg(j,i))
            c3 = c3+1;
            pv_C(c3,i) = pv0(j,i);
            rt_C(c3,i) = Reaction_T(j,i);
        elseif any(Fouth_Egg(j,i))
            c4 = c4+1;
            pv_D(c4,i) = pv0(j,i);
            rt_D(c4,i) = Reaction_T(j,i);
        elseif any(Fouth_NO_Egg(j,i))
            c4 = c4+1;
            pv_D(c4,i) = pv0(j,i);
            rt_D(c4,i) = Reaction_T(j,i);
        else
            c5 = 0;
        end
    end
    c1=0; c2=0; c3=0; c4=0;
end

%% p1 11263788,33154866,44261577,22483755
m = 1:12;
figure;
for i = 1:block_num
    subplot(3,3,i);
    plot(m,pv_A(:,i));
    hold on
    plot(m,pv_B(:,i));
    plot(m,pv_C(:,i));
    plot(m,pv_D(:,i));
    grid on
    hold off
end
legend('target 1','target 2','target 3','target 4');

%%
m = 1:12;
figure;
for i = 1:block_num
    subplot(3,3,i);
    plot(m,rt_A(:,i));
    hold on
    plot(m,rt_B(:,i));
    plot(m,rt_C(:,i));
    plot(m,rt_D(:,i));
    grid on
    hold off
end
legend('target 1','target 2','target 3','target 4');

%% find no egg peakv
n = floor(trial_in_block/4);
mm = floor(trial_in_block/8);
PVnr_A = zeros(mm,4); PVnr_B = zeros(mm,4);
PVnr_C = zeros(mm,4); PVnr_D = zeros(mm,4);
RTnr_A = zeros(mm,4); RTnr_B = zeros(mm,4);
RTnr_C = zeros(mm,4); RTnr_D = zeros(mm,4);
pv50_A = zeros(mm,4); pv50_B = zeros(mm,4);
pv50_C = zeros(mm,4); pv50_D = zeros(mm,4);
rt50_A = zeros(mm,4); rt50_B = zeros(mm,4);
rt50_C = zeros(mm,4); rt50_D = zeros(mm,4);
s2=0;s3=0;t2=0;t3=0;
for b1 = 1:2      % 11263788
    for b2 = 1:trial_in_block
        if any(Second_NO_Egg(b2,b1))
            s2 = s2+1;
            PVnr_B(s2,b1) = pv2(b2,b1);
            RTnr_B(s2,b1) = Reaction_T(b2,b1);
        elseif any(Third_NO_Egg(b2,b1))
            s3 = s3+1;
            PVnr_C(s3,b1) = pv2(b2,b1);
            RTnr_C(s3,b1) = Reaction_T(b2,b1);
        elseif any(Second_Egg(b2,b1))
            t2 = t2+1;
            pv50_B(t2,b1) = pv2(b2,b1);
            rt50_B(t2,b1) = Reaction_T(b2,b1);
        elseif any(Third_Egg(b2,b1))
            t3 = t3+1;
            pv50_C(t3,b1) = pv2(b2,b1);
            rt50_C(t3,b1) = Reaction_T(b2,b1);
        else
            s5 = 0;
        end
    end
    s2=0;s3=0;t2=0;t3=0;
end

s1=0;s4=0;t1=0;t4=0;
for b1 = 3:1:4     % 33154866
    for b2 = 1:trial_in_block
        if any(First_NO_Egg(b2,b1))
            s1 = s1+1;
            PVnr_A(s1,b1-2) = pv2(b2,b1);
            RTnr_A(s1,b1-2) = Reaction_T(b2,b1);
        elseif any(Fouth_NO_Egg(b2,b1))
            s4 = s4+1;
            PVnr_D(s4,b1-2) = pv2(b2,b1);
            RTnr_D(s4,b1-2) = Reaction_T(b2,b1);
        elseif any(First_Egg(b2,b1))
            t1 = t1+1;
            pv50_A(t1,b1-2) = pv2(b2,b1);
            rt50_A(t1,b1-2) = Reaction_T(b2,b1);
        elseif any(Fouth_Egg(b2,b1))
            t4 = t4+1;
            pv50_D(t4,b1-2) = pv2(b2,b1);
            rt50_D(t4,b1-2) = Reaction_T(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s4=0;t1=0;t4=0;
end

s1=0;s2=0;t1=0;t2=0;
for b1 = 5:1:6      % 44261577
    for b2 = 1:trial_in_block
        if any(First_NO_Egg(b2,b1))
            s1 = s1+1;
            PVnr_A(s1,(b1-2)) = pv2(b2,b1);
            RTnr_A(s1,(b1-2)) = Reaction_T(b2,b1);
        elseif any(Second_NO_Egg(b2,b1))
            s2 = s2+1;
            PVnr_B(s2,(b1-2)) = pv2(b2,b1);
            RTnr_B(s2,(b1-2)) = Reaction_T(b2,b1);
        elseif any(First_Egg(b2,b1))
            t1 = t1+1;
            pv50_A(t1,(b1-2)) = pv2(b2,b1);
            rt50_A(t1,(b1-2)) = Reaction_T(b2,b1);
        elseif any(Second_Egg(b2,b1))
            t2 = t2+1;
            pv50_B(t2,(b1-2)) = pv2(b2,b1);
            rt50_B(t2,(b1-2)) = Reaction_T(b2,b1);
        else
            s5 = 0;
        end
    end
    s1=0;s2=0;t1=0;t2=0;
end

s3=0;s4=0;t3=0;t4=0;
for b1 = 7:1:8      % 22483755
    for b2 = 1:trial_in_block
        if any(Third_NO_Egg(b2,b1))
            s3 = s3+1;
            PVnr_C(s3,(b1-4)) = pv2(b2,b1);
            RTnr_C(s3,(b1-4)) = Reaction_T(b2,b1);
        elseif any(Fouth_NO_Egg(b2,b1))
            s4 = s4+1;
            PVnr_D(s4,(b1-4)) = pv2(b2,b1);
            RTnr_D(s4,(b1-4)) = Reaction_T(b2,b1);
        elseif any(Third_Egg(b2,b1))
            t3 = t3+1;
            pv50_C(t3,(b1-4)) = pv2(b2,b1);
            rt50_C(t3,(b1-4)) = Reaction_T(b2,b1);
        elseif any(Fouth_Egg(b2,b1))
            t4 = t4+1;
            pv50_D(t4,(b1-4)) = pv2(b2,b1);
            rt50_D(t4,(b1-4)) = Reaction_T(b2,b1);
        else
            s5 = 0;
        end
    end
    s3=0;s4=0;t3=0;t4=0;
end

%%
tinnr = zeros(4,4);  % 100,50,0
n1=0;n2=0;n3=0;n4=0;
% check missed trial
for b3 = 1:4
    for b4 = 1:mm
        if any(PVnr_A(b4,b3))
            n1 = n1+1;
        end
        
        if any(PVnr_B(b4,b3))
            n2 = n2+1;
        end
        
        if any(PVnr_C(b4,b3))
            n3 = n3+1;
        end
        
        if any(PVnr_D(b4,b3))
            n4 = n4+1;
        end        
    end
    tinnr(b3,1) = n1; tinnr(b3,2) = n2; 
    tinnr(b3,3) = n3; tinnr(b3,4) = n4; 
    n1=0;n2=0;n3=0;n4=0;
end

%%
tinn50 = zeros(4,4);  % 100,50,0
n1=0;n2=0;n3=0;n4=0;
% check missed trial
for b3 = 1:4
    for b4 = 1:mm
        if any(pv50_A(b4,b3))
            n1 = n1+1;
        end
        
        if any(pv50_B(b4,b3))
            n2 = n2+1;
        end
        
        if any(pv50_C(b4,b3))
            n3 = n3+1;
        end
        
        if any(pv50_D(b4,b3))
            n4 = n4+1;
        end        
    end
    tinn50(b3,1) = n1; tinn50(b3,2) = n2; 
    tinn50(b3,3) = n3; tinn50(b3,4) = n4; 
    n1=0;n2=0;n3=0;n4=0;
end
%% no reward peakv
Pnm_A = zeros(4,1);
Pnm_B = zeros(4,1);
Pnm_C = zeros(4,1);
Pnm_D = zeros(4,1);
for i = 1:4
    Pnm_A(i) = sum(PVnr_A(1:tinnr(i,1),i))/tinnr(i,1);
    Pnm_B(i) = sum(PVnr_B(1:tinnr(i,1),i))/tinnr(i,2);
    Pnm_C(i) = sum(PVnr_C(1:tinnr(i,1),i))/tinnr(i,3);
    Pnm_D(i) = sum(PVnr_D(1:tinnr(i,1),i))/tinnr(i,4);
end
Pnm_al = [sum(sum(PVnr_A))/sum(tinnr(:,1));sum(sum(PVnr_B))/...
    sum(tinnr(:,2));sum(sum(PVnr_C))/sum(tinnr(:,3));...
    sum(sum(PVnr_D))/sum(tinnr(:,4))];

%%
m = 1:16;
figure;
bar(m, [Pnm_A;Pnm_B;Pnm_C;Pnm_D]);
hold on
line([4.5 4.5],[0 80],'Color','r','LineStyle','--');
line([8.5 8.5],[0 80],'Color','r','LineStyle','--');
line([12.5 12.5],[0 80],'Color','r','LineStyle','--');
hold off
m = 1:4;
figure;
bar(m,Pnm_al);
m = 1:3;
figure;
bar(m,[(Pnm_al(2)-Pnm_al(1)),(Pnm_al(3)-Pnm_al(1)),...
    (Pnm_al(4)-Pnm_al(1))]);

%%
P50m_A = zeros(4,1);
P50m_B = zeros(4,1);
P50m_C = zeros(4,1);
P50m_D = zeros(4,1);
for i = 1:4
    P50m_A(i) = sum(pv50_A(1:tinnr(i,1),i))/tinn50(i,1);
    P50m_B(i) = sum(pv50_B(1:tinnr(i,1),i))/tinn50(i,2);
    P50m_C(i) = sum(pv50_C(1:tinnr(i,1),i))/tinn50(i,3);
    P50m_D(i) = sum(pv50_D(1:tinnr(i,1),i))/tinn50(i,4);
end
P50m_al = [sum(sum(pv50_A))/sum(tinn50(:,1));sum(sum(pv50_B))/...
    sum(tinn50(:,2));sum(sum(pv50_C))/sum(tinn50(:,3));...
    sum(sum(pv50_D))/sum(tinn50(:,4))];

%%
m = 1:16;
figure;
bar(m, [P50m_A;P50m_B;P50m_C;P50m_D]);
hold on
line([4.5 4.5],[0 80],'Color','r','LineStyle','--');
line([8.5 8.5],[0 80],'Color','r','LineStyle','--');
line([12.5 12.5],[0 80],'Color','r','LineStyle','--');
hold off
m = 1:4;
figure;
bar(m,P50m_al);
m = 1:3;
figure;
bar(m,[(P50m_al(2)-P50m_al(1)),(P50m_al(3)-P50m_al(1)),...
    (P50m_al(4)-P50m_al(1))]);

%% PeakV - mean of each block
PV_100_mean = zeros(block_num,2);  
PV_50_mean = zeros(block_num,2);
PV_50a_mean = zeros(block_num,2);
PV_0_mean = zeros(block_num,2);
for j = 1:block_num
    PV_100_mean(j,1) = sum(PV_100(:,j))/tinn(j,1);
    PV_50_mean(j,1) = sum(PV_50(:,j))/tinn(j,2);
    PV_50a_mean(j,1) = sum(PV_50(:,j)+PV_50no(:,j))/(tinn(j,2)+tinn(j,4));
    PV_0_mean(j,1) = sum(PV_0(:,j))/tinn(j,3);
end

%% PeakV - mean of each two blocks
nn = floor(block_num/2);  
for j = 1:nn
    PV_100_mean(j,2) = sum(sum(PV_100(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),1));
    PV_50_mean(j,2) = sum(sum(PV_50(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),2));
    PV_50a_mean(j,2) = sum(sum(PV_50(:,(2*j-1):(2*j))+PV_50no(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),2)+tinn((2*j-1):(2*j),4));
    PV_0_mean(j,2) = sum(sum(PV_0(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),3));
end

%PV_100_e = sqrt(sum(sqr(PV_100 - PV_100_mean))/(n-1));
%PV_50_e = sqrt(sum(sqr(PV_50 - PV_50_mean))/(n-1));
%PV_0_e = sqrt(sum(sqr(PV_0 - PV_0_mean))/(n-1));

%% PeakV - standard deviation of each block
PV_100_e = zeros(block_num,2);  
PV_50_e = zeros(block_num,2);
PV_0_e = zeros(block_num,2);
for i = 1:block_num      
    PV_100_e(i,1) = std(PV_100(1:tinn(i,1),i));
    PV_50_e(i,1) = std(PV_50(1:tinn(i,2),i));
    PV_0_e(i,1) = std(PV_0(1:tinn(i,3),i));
end

%% PeakV - standard deviation of each two blocks
T_100 = zeros(2*n,4);   
T_50 = zeros(2*n,4);
T_0 = zeros(2*n,4);
for j = 1:nn
    T_100(1:tinn((2*j-1),1),j) = PV_100(1:tinn((2*j-1),1),(2*j-1));
    T_100((tinn((2*j-1),1)+1):((tinn((2*j-1),1)+tinn((2*j),1))),j)...
        = PV_100(1:tinn((2*j),1),(2*j));
    PV_100_e(j,2) = std(T_100(1:(tinn((2*j-1),1)+tinn((2*j),1)),j));
    
    T_50(1:tinn((2*j-1),2),j) = PV_50(1:tinn((2*j-1),2),(2*j-1));
    T_50((tinn((2*j-1),2)+1):((tinn((2*j-1),2)+tinn((2*j),2))),j)...
        = PV_50(1:tinn((2*j),2),(2*j));
    PV_50_e(j,2) = std(T_50(1:(tinn((2*j-1),2)+tinn((2*j),2)),j));
    
    T_0(1:tinn((2*j-1),3),j) = PV_0(1:tinn((2*j-1),3),(2*j-1));
    T_0((tinn((2*j-1),3)+1):((tinn((2*j-1),3)+tinn((2*j),3))),j)...
        = PV_0(1:tinn((2*j),3),(2*j));
    PV_0_e(j,2) = std(T_0(1:(tinn((2*j-1),3)+tinn((2*j),3)),j));
end

%% Reaction time - mean of each block
RT_100_mean = zeros(block_num,2);  
RT_50_mean = zeros(block_num,2);
RT_0_mean = zeros(block_num,2);
for j = 1:block_num
    RT_100_mean(j,1) = sum(ReacT_100(:,j))/tinn(j,1);
    RT_50_mean(j,1) = sum(ReacT_50(:,j))/tinn(j,2);
    RT_0_mean(j,1) = sum(ReacT_0(:,j))/tinn(j,3);
end

%% Reaction time -  mean of each two blocks
nn = floor(block_num/2);
for j = 1:nn
    RT_100_mean(j,2) = sum(sum(ReacT_100(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),1));
    RT_50_mean(j,2) = sum(sum(ReacT_50(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),2));
    RT_0_mean(j,2) = sum(sum(ReacT_0(:,(2*j-1):(2*j))))/...
        sum(tinn((2*j-1):(2*j),3));
end

%% Reaction time - standard deviation of each block
RT_100_e = zeros(block_num,2);  
RT_50_e = zeros(block_num,2);
RT_0_e = zeros(block_num,2);
for i = 1:block_num      % standard deviation of each block
    RT_100_e(i,1) = std(ReacT_100(1:tinn(i,1),i));
    RT_50_e(i,1) = std(ReacT_50(1:tinn(i,2),i));
    RT_0_e(i,1) = std(ReacT_0(1:tinn(i,3),i));
end

%% Reaction Time - standard deviation of each two blocks
R_100 = zeros(2*n,4);   
R_50 = zeros(2*n,4);
R_0 = zeros(2*n,4);
for j = 1:nn
    R_100(1:tinn((2*j-1),1),j) = ReacT_100(1:tinn((2*j-1),1),(2*j-1));
    R_100((tinn((2*j-1),1)+1):((tinn((2*j-1),1)+tinn((2*j),1))),j)...
        = PV_100(1:tinn((2*j),1),(2*j));
    RT_100_e(j,2) = std(R_100(1:(tinn((2*j-1),1)+tinn((2*j),1)),j));
    
    R_50(1:tinn((2*j-1),2),j) = ReacT_50(1:tinn((2*j-1),2),(2*j-1));
    R_50((tinn((2*j-1),2)+1):((tinn((2*j-1),2)+tinn((2*j),2))),j)...
        = PV_50(1:tinn((2*j),2),(2*j));
    RT_50_e(j,2) = std(R_50(1:(tinn((2*j-1),2)+tinn((2*j),2)),j));
    
    R_0(1:tinn((2*j-1),3),j) = ReacT_0(1:tinn((2*j-1),3),(2*j-1));
    R_0((tinn((2*j-1),3)+1):((tinn((2*j-1),3)+tinn((2*j),3))),j)...
        = PV_0(1:tinn((2*j),3),(2*j));
    RT_0_e(j,2) = std(R_0(1:(tinn((2*j-1),3)+tinn((2*j),3)),j));
end

%% Reaction time - plots of each block
m = 1:3;
for i = 1:block_num
    RT_Prob = [RT_100_mean(i,1) RT_50_mean(i,1) RT_0_mean(i,1)];
    RT_e = [RT_100_e(i,1) RT_50_e(i,1) RT_0_e(i,1)];
    figure;
    hold on
    bar(m,RT_Prob); 
    er = errorbar(m,RT_Prob,RT_e,RT_e);
    er.Color = [0 0 0];
    er.LineStyle = 'none';
    xlabel('Probability of Reward');
    ylabel('Reaction Time');
    title('RT / Block (ms)');
    hold off
end

%% Reaction time - plots of each two blocks
for i = 1:nn
    RT_Prob = [RT_100_mean(i,2) RT_50_mean(i,2) RT_0_mean(i,2)];
    RT_e = [RT_100_e(i,2) RT_50_e(i,2) RT_0_e(i,2)];
    figure;
    hold on
    bar(m,RT_Prob);
    er = errorbar(m,RT_Prob,RT_e,RT_e);
    er.Color = [0 0 0];
    er.LineStyle = 'none';
    xlabel('Probability of Reward');
    ylabel('RT / Block (ms)');
    title('Reaction Time');
    hold off
end

%%
m = 1:3;
for i = 1:block_num
    PV_Prob = [PV_100_mean(i,1) PV_50_mean(i,1) PV_0_mean(i,1)];
    PV_e = [PV_100_e(i,1) PV_50_e(i,1) PV_0_e(i,1)];
    figure;
    hold on
    bar(m,PV_Prob);
    er = errorbar(m,PV_Prob,PV_e,PV_e);
    er.Color = [0 0 0];
    er.LineStyle = 'none';
    xlabel('Probability of Reward');
    ylabel('Peak Velocity');
    hold off
end

%%
m = 1:3;
for i = 1:nn
    PV_Prob = [PV_100_mean(i,2) PV_50_mean(i,2) PV_0_mean(i,2)];
    PV_e = [PV_100_e(i,2) PV_50_e(i,2) PV_0_e(i,2)];
    figure;
    hold on
    bar(m,PV_Prob);
    er = errorbar(m,PV_Prob,PV_e,PV_e);
    er.Color = [0 0 0];
    er.LineStyle = 'none';
    xlabel('Probability of Reward');
    ylabel('Peak Velocity');
    hold off
end

%% a 1, b 2, c 3, d 4
m = 1:3;
% 11263788,33154866,44261577,22483755
meanA_100 = PV_100_mean(1,2); 
meanA_50 = (sum(sum(pv50_A)))/sum(tinn50(:,1));
meanA_0 = PV_0_mean(4,2);
meanA = [meanA_100 meanA_50 meanA_0];
%%
eA_100 = PV_100_e(1,2);
eA_50 = std([pv50_A(1:tinn50(1,1),1);pv50_A(1:tinn50(2,1),2);...
    pv50_A(1:tinn50(3,1),3);pv50_A(1:tinn50(4,1),4)]);
eA_0 = PV_0_e(4,2);
eA = [eA_100 eA_50 eA_0];
figure;
hold on
bar(m,meanA);
er = errorbar(m,meanA,eA,eA);
er.Color = [0 0 0];
er.LineStyle = 'none';
title('Target 1');
xlabel('Probability of Reward');
ylabel('Peak Velocity');
hold off

%% 11263788,33154866,44261577,22483755
meanB_100 = PV_100_mean(4,2); 
meanB_50 = (sum(sum(pv50_B)))/sum(tinn50(:,2));
meanB_0 = PV_0_mean(2,2);
meanB = [meanB_100 meanB_50 meanB_0];
%%
eB_100 = PV_100_e(4,2);
eB_50 = std([pv50_B(1:tinn50(1,2),1);pv50_B(1:tinn50(2,2),2);...
    pv50_B(1:tinn50(3,2),3);pv50_B(1:tinn50(4,2),4)]);
eB_0 = PV_0_e(2,2);
eB = [eB_100 eB_50 eB_0];
figure;
hold on
bar(m,meanB);
er = errorbar(m,meanB,eB,eB);
er.Color = [0 0 0];
er.LineStyle = 'none';
title('Target 2');
xlabel('Probability of Reward');
ylabel('Peak Velocity');
hold off

%% 11263788,33154866,44261577,22483755
meanC_100 = PV_100_mean(2,2); 
meanC_50 = (sum(sum(pv50_C)))/sum(tinn50(:,3));
meanC_0 = PV_0_mean(3,2);
meanC = [meanC_100 meanC_50 meanC_0];
%%
eC_100 = PV_100_e(2,2);
eC_50 = std([pv50_C(1:tinn50(1,3),1);pv50_C(1:tinn50(2,3),2);...
    pv50_C(1:tinn50(3,3),3);pv50_C(1:tinn50(4,3),4)]);
eC_0 = PV_0_e(3,2);
eC = [eC_100 eC_50 eC_0];
figure;
hold on
bar(m,meanC);
er = errorbar(m,meanC,eC,eC);
er.Color = [0 0 0];
er.LineStyle = 'none';
title('Target 3');
xlabel('Probability of Reward');
ylabel('Peak Velocity');
hold off

%% 11263788,33154866,44261577,22483755
meanD_100 = PV_100_mean(3,2); 
meanD_50 = (sum(sum(pv50_D)))/sum(tinn50(:,4));
meanD_0 = PV_0_mean(1,2);
meanD = [meanD_100 meanD_50 meanD_0];
%%
eD_100 = PV_100_e(3,2);
eD_50 = std([pv50_D(1:tinn50(1,4),1);pv50_D(1:tinn50(2,4),2);...
    pv50_D(1:tinn50(3,4),3);pv50_D(1:tinn50(4,4),4)]);
eD_0 = PV_0_e(1,2);
eD = [eD_100 eD_50 eD_0];
figure;
hold on
bar(m,meanD);
er = errorbar(m,meanD,eD,eD);
er.Color = [0 0 0];
er.LineStyle = 'none';
title('Target 4');
xlabel('Probability of Reward');
ylabel('Peak Velocity');
hold off

%%
%m = 1:4;
m = 1:3;
mean100_Ta = [meanA_100,meanB_100,meanC_100,meanD_100];
mean50_Ta = [meanA_50,meanB_50,meanC_50,meanD_50];
mean0_Ta = [meanA_0,meanB_0,meanC_0,meanD_0];
figure;
%bar(m,mean100_Ta);
bar(m,[(meanB_100-meanA_100),(meanC_100-meanA_100),...
    (meanD_100-meanA_100)]);
figure;
%bar(m,mean50_Ta);
bar(m,[(meanB_50-meanA_50),(meanC_50-meanA_50),...
    (meanD_50-meanA_50)]);
figure;
%bar(m,mean0_Ta);
bar(m,[(meanB_0-meanA_0),(meanC_0-meanA_0),...
    (meanD_0-meanA_0)]);

%%
m1=1:3; m2=4:6; m3=7:9;
figure; hold on
bar(m1,[(meanB_100-meanA_100),(meanC_100-meanA_100),...
    (meanD_100-meanA_100)],'r');
%bar(m,mean50_Ta);
bar(m2,[(meanB_50-meanA_50),(meanC_50-meanA_50),...
    (meanD_50-meanA_50)],'b');
%bar(m,mean0_Ta);
bar(m3,[(meanB_0-meanA_0),(meanC_0-meanA_0),...
    (meanD_0-meanA_0)],'g');

%%
mean_Ta = [mean([meanA_100,meanA_50,meanA_0]),...
    mean([meanB_100,meanB_50,meanB_0]),mean([meanC_100,meanC_50,meanC_0]),...
    mean([meanD_100,meanD_50,meanD_0])];
figure;
bar(m,mean_Ta);

Ra_Targa = [mean_Ta(1)/mean_Ta(1);mean_Ta(2)/mean_Ta(1);...
    mean_Ta(3)/mean_Ta(1);mean_Ta(4)/mean_Ta(1)];

Ra_Targ0 = [(meanA_0/meanA_0),(meanB_0/meanA_0),...
    (meanC_0/meanA_0),(meanD_0/meanA_0)];

%% PV of each block
mean_block = zeros(block_num,1);
for i = 1:block_num
    mean_block(i) = sum(pv2(:,i))/trial_in_block;
end

m = 1:block_num;
figure;
bar(m,mean_block);

%% PV of each two blocks
mean_2block = zeros(nn,1);
for i = 1:nn
    mean_2block(i) = sum(sum(pv2(:,(2*i-1):(2*i))))/(trial_in_block*2);
end

m = 1:nn;
figure;
bar(m,mean_2block);
xlabel('block');
ylabel('Average Peak Velocity (cm/s)');
title('Average Peak Velocity / Block');

%%
m_100re = [meanA_100/mean_2block(1),meanB_100/mean_2block(4),...
    meanC_100/mean_2block(2),meanD_100/mean_2block(3)];

m_0re = [meanA_0/mean_2block(4),meanB_0/mean_2block(2),...
    meanC_0/mean_2block(3),meanD_0/mean_2block(1)];

m = 1:4;
figure;
bar(m,m_100re);
figure;
bar(m,m_0re);

%%
m = 1:3;
mean_100 = sum(sum(PV_100))/sum(tinn(:,1));
mean_50 = sum(sum(PV_50))/sum(tinn(:,2));
mean_0 = sum(sum(PV_0))/sum(tinn(:,3));
e_100 = std([PV_100(1:tinn(1,1),1);PV_100(1:tinn(2,1),2);...
    PV_100(1:tinn(3,1),3);PV_100(1:tinn(4,1),4);PV_100(1:tinn(5,1),5);...
    PV_100(1:tinn(6,1),6);PV_100(1:tinn(7,1),7);PV_100(1:tinn(8,1),8)]);
e_50 = std([PV_50(1:tinn(1,2),1);PV_50(1:tinn(2,2),2);...
    PV_50(1:tinn(3,2),3);PV_50(1:tinn(4,2),4);PV_50(1:tinn(5,2),5);...
    PV_50(1:tinn(6,2),6);PV_50(1:tinn(7,2),7);PV_50(1:tinn(8,2),8)]);
e_0 = std([PV_0(1:tinn(1,3),1);PV_0(1:tinn(2,3),2);...
    PV_0(1:tinn(3,3),3);PV_0(1:tinn(4,3),4);PV_0(1:tinn(5,3),5);...
    PV_0(1:tinn(6,3),6);PV_0(1:tinn(7,3),7);PV_0(1:tinn(8,3),8)]);
meanaa = [mean_100 mean_50 mean_0];
eaa = [e_100 e_50 e_0];
figure;
hold on
bar(m,meanaa);
er = errorbar(m,meanaa,eaa,eaa);
er.Color = [0 0 0];
er.LineStyle = 'none';
title('Total');
xlabel('Probability of Reward');
ylabel('Peak Velocity');
hold off

%%
%PV_Avg = struct;
PV_Avg.Laura = struct;
PV_Avg.Laura.Mean = {'100 50 0';meanaa};
PV_Avg.LuF.SD = {'100 50 0';eaa};
PV_Avg.LuF.PV = {'100%',PV_100;'50%',PV_50;'0%',PV_0};
PV_Avg.LuF.target_in = tinn;

%%
PV_Avg.Hannah = struct;
PV_Avg.Hannah.MeanTP = {'100 50 50a 0',0;'PeakV',meanaa;'ReacT',meanTaa};
PV_Avg.Hannah.MeanTP{2,3} = meanPV;
PV_Avg.Hannah.MeanTP{3,3} = meanRT;
PV_Avg.Hannah.PVraw = pv2;
PV_Avg.Hannah.RTraw = Reaction_T;
PV_Avg.Hannah.Target_in = tinn;

%%
PV_Avg.Dan.PVfil = {'100','50','50no','0';PV_100,PV_50,PV_50no,PV_0}; 
PV_Avg.Dan.RTfil = {'100','50','50no','0';ReacT_100,ReacT_50,...
    ReacT_50no,ReacT_0};
PV_Avg.Dan.PVfilnol = {'100','50','50no','0';PVN_100,PVN_50,PVN_50no,PVN_0}; 
PV_Avg.Dan.RTfilnol = {'100','50','50no','0';RT_100,RT_50,RT_50no,RT_0}; 
PV_Avg.Dan.Target_in50 = tinn50;
%%
PV_Avg.Dan.MeanTP{4,1} = '100 50 0';
PV_Avg.Dan.MeanTP{5,1} = 'PeakV';
PV_Avg.Dan.MeanTP{6,1} = 'ReacT';
PV_Avg.Dan.MeanTP{4,2} = '1st Block';
PV_Avg.Dan.MeanTP{4,3} = '2nd Block';
PV_Avg.Dan.MeanTP{4,4} = 'mid12';
PV_Avg.Dan.MeanTP{5,2} = [meanP_100(2),meanP_50a(2),meanP_0(2)];
PV_Avg.Dan.MeanTP{5,3} = [meanP_100(3),meanP_50a(3),meanP_0(3)];
PV_Avg.Dan.MeanTP{5,4} = [meanP_100(4),meanP_50a(4),meanP_0(4)];
PV_Avg.Dan.MeanTP{6,2} = [meanT_100(2),meanT_50a(2),meanT_0(2)];
PV_Avg.Dan.MeanTP{6,3} = [meanT_100(3),meanT_50a(3),meanT_0(3)];
PV_Avg.Dan.MeanTP{6,4} = [meanT_100(4),meanT_50a(4),meanT_0(4)];
PV_Avg.Dan.MeanTP{7,1} = 'raw';
PV_Avg.Dan.MeanTP{8,1} = 'A B C D';
PV_Avg.Dan.MeanTP{8,2} = '100 50 0';
PV_Avg.Dan.MeanTP{8,3} = 'PV;RT';
PV_Avg.Dan.MeanTP{9,1} = [meanAP;meanAT];
PV_Avg.Dan.MeanTP{9,2} = [meanBP;meanBT];
PV_Avg.Dan.MeanTP{9,3} = [meanCP;meanCT];
PV_Avg.Dan.MeanTP{9,4} = [meanDP;meanDT];
%%
PV_Avg.Dan.MeanTP{10,1} = 'normalized by 2 blocks';
PV_Avg.Dan.MeanTP{11,1} = 'A B C D';
PV_Avg.Dan.MeanTP{11,2} = '100 50 0';
PV_Avg.Dan.MeanTP{11,3} = 'PV;RT';
PV_Avg.Dan.MeanTP{12,1} = [meanAP;meanAT];
PV_Avg.Dan.MeanTP{12,2} = [meanBP;meanBT];
PV_Avg.Dan.MeanTP{12,3} = [meanCP;meanCT];
PV_Avg.Dan.MeanTP{12,4} = [meanDP;meanDT];

%%
PV_Avg.Hannah.Event = {'EggN 1Y 1N 2Y 2N 3Y 3N 4Y 4N Mis Err Pau';...
    Egg_Index;First_Egg;First_NO_Egg;Second_Egg;Second_NO_Egg;...
    Third_Egg;Third_NO_Egg;Fouth_Egg;Fouth_NO_Egg;Miss_Egg;...
    Error_In;Pause_In};
PV_Avg.Hannah.TDE = {'Revised ReactionT',RT_Ori;'reRevised RT',Reaction_T;...
    'Reach Duration',Reach_D_net;'Max Excursion',Max_Ex};
PV_Avg.Hannah.TDEfil = {0,'100','50','50no','0';'PeakV',PV_100,PV_50,PV_50no,...
    PV_0;'ReactionT',ReacT_100,ReacT_50,ReacT_50no,ReacT_0;'ReachD',...
    RD_100,RD_50,RD_50no,RD_0;'MaxExcur',ME_100,ME_50,ME_50no,ME_0};
PV_Avg.Hannah.TDEfil(6,:) = {0,'Targ1','Targ2','Targ3','Targ4'};
PV_Avg.Hannah.TDEfil(7,:) = {'PeakV',pvA,pvB,pvC,pvD};
PV_Avg.Hannah.TDEfil(8,:) = {'ReactionT',rtA,rtB,rtC,rtD};
PV_Avg.Hannah.TDEfil(9,:) = {'ReacD',rdA,rdB,rdC,rdD};
PV_Avg.Hannah.TDEfil(10,:) = {'MaxExcur',meA,meB,meC,meD};

%%
PV_Avg.Hannah.PVtarg = {'100 50 0','mean2block',meanP_2block,0;...
    'target1','target2','target3','target4';meanAP,meanBP,meanCP,meanDP};
PV_Avg.Hannah.RTtarg = {'100 50 0','mean2block',meanT_2block,0;...
    'target1','target2','target3','target4';meanAP,meanBP,meanCP,meanDP};

%%
c = categorical({'100%','50%','0%'});
bar(c,meanaa);

%% normalize with targets
% see data_oth_roto

%% normalize with time
pv3 = zeros(trial_in_block,block_num);
pv4 = zeros(trial_in_block,block_num);
for i = 1:block_num
    for j = 1:trial_in_block
        pv3(j,i) = pv2(j,i)/mean_block(i);
        pv4(j,i) = pv2(j,i)/mean_2block(round(i/2));
    end
end

%%

























