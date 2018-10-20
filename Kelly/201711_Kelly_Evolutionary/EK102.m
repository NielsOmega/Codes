%% Evolutionary Kelly

%% Global Commands

clear;clc; % clear

% specify directoy for the files <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
directory='C:\Users\N\Documents\Studium\_Codes\Finance\201701 Kelly\201710 Evolutionary Kelly';
cd(directory)
addpath(genpath(directory))

%set global commands for font size and line width
size_font=9;
size_line=1.5;
set(0,'DefaultAxesFontSize',size_font,'DefaultTextFontSize',size_font);
set(0,'defaultlinelinewidth',size_line)

% figures
set(0, 'defaultFigurePaperType', 'A4')
set(0, 'defaultFigurePaperUnits', 'centimeters')
set(0, 'defaultFigurePaperPositionMode', 'auto')
figure_wide=[680 678 800 420];

% reset rngs before running
rng(4)

%% Setup

% total wealth starting point
W_total=100;
W_0=1;

% investors
k=200;

% investments
f_step=1/k;
f_end=2;
f=f_step:f_step*f_end:f_end;

% periods
T=2000;

%% Process

m=1%100; % #trajectories

% init
gini=nan(T+1,m);
mu=0.03;
sigma=0.245; %\sigma^2=2*scale^2 <> scale=sqrt((sigma^2)/2) f=0.5
alpha=2;
scale=sigma/sqrt(2);

% minimum wealth metric
    % 0 for no minimum wealth
    % x for minimum wealth from state
W_min=0

% wealth boundary
W_death=0.1

% children probability metric
p_child=0.001
Population=ones(T+2,k); % population initializer

for j=1:m
j
   
%x=mu+sigma*randn(T,1);
%x=mu+sigma*trnd(5,T,1);
x=mu+scale*random('stable',alpha,0,1,0,T,1);
x(x<-10)=mu;

%% Wealth over time

W_t_k=nan(T+1,k);
W_t_k(1,:)=W_0; % starting wealth
Sub=zeros(T+1,1); % subsidies initializer

for i=1:T
    i
    % wealth equation
    W_t_k(i+1,:)=W_t_k(i,:).*(1+f.*x(i));
    
    % minimum wealth
    if W_min~=0
        sub_index=W_t_k(i+1,:)<W_min; % guys who need subsidies
        
        % subsidies dependend on population size
        Sub(i+1)=Sub(i)+W_min*sum(sub_index)*sum(Population(i+1,sub_index))-sum(W_t_k(i+1,sub_index)); 
        
        W_t_k(i+1,:)=max(W_t_k(i+1,:),W_min);
        
    end
    
    % death
    W_t_k(i+1,W_t_k(i+1,:)<W_death)=0;
    Population(i+1,W_t_k(i+1,:)<W_death)=0;
    
    
    % children
    if p_child~=0
        % who can have children
        sub_index_fertile=W_t_k(i+1,:)./Population(i+1,:)>W_death; %
        % how many per cohorte
        children_multiplier=sub_index_fertile.*Population(i+1,:);
        % random children
        children=binornd(children_multiplier,p_child,1,k);
        % new population
        Population(i+2,:)=Population(i+1,:)+children;
        
    end
    
    
end

%% Gini

%p=ones(1,k)'; % population

for i=1:T+1
    w=W_t_k(i,:)';
    p=Population(i,:)';
    w(p==0)=[]; % delete wealths for dead populations
    p(p==0)=[];
    gini(i,j) = ginicoeff(p,w./p); %/p
end


end

%% Plot Wealth distribution

subplot(1,2,1)
mesh(W_t_k+1) %+1
ylim([1,T+1])
subplot(1,2,2)
mesh(W_t_k+1)
ylim([1,T+1])
set(gca, 'ZScale', 'log')

%% Total wealth after T periods

figure();
plot(sum(W_t_k,2));hold on;
plot(Sub);
plot(sum(W_t_k,2)-Sub)
%set(gca, 'YScale', 'log')

%% Plot Gini

gini_mean=nan(T+1,1);
gini_lower=gini_mean;
gini_upper=gini_mean;

for i=1:T+1
    gini_mean(i,1) = mean(gini(i,:));
    gini_lower(i,1) = quantile(gini(i,:),0.05);
    gini_upper(i,1) = quantile(gini(i,:),0.95);
end

figure();
boundedline(0:1:T,gini_mean,[gini_mean-gini_lower,...
                            gini_upper-gini_mean],'alpha','cmap',[0 102 204]./255)
                        %[0 204 102]./255 green
                        %[204 0 0]./255 red
                        %[0 102 204]./255 blue
xlabel('Time');ylabel('Gini coefficient');
ylim([0 1])
%set(h, 'XScale', 'log')

%% Population
figure()
mesh(Population)
xlabel('f');ylabel('T');zlabel('P');
ylim([1 T+2])
set(gca, 'ZScale', 'log')

%plot(Population)

%% Theoretically optimal fraction

optimal_fraction=0

if optimal_fraction==1
    
W_E_log=nan(k,1);
x_long=mu+scale*random('stable',alpha,0,1,0,10^6,1);
for i=1:k
    W_E_log(i,1)=real(mean(log(f(i)*x_long+1)));
end
plot([0 f],[0; W_E_log])

end

%% 

% p_poor=nan(T+1,1);
% for i=1:T+1
%     p_poor(i,1)=sum(W_t_k(i,:)<0.5)/k;
% end
% figure()
% plot(p_poor)

















