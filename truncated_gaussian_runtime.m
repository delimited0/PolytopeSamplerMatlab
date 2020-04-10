%% positive orthant

P2 = Problem; 
P2.lb = zeros(2, 1);
% P2.ub = ones(2, 1);
P2.df = @(x) x;

iter = 1000;

tic;
plan = prepare(P2, struct('display', 1));
out = sample(plan, iter);
t = toc;

scatter(out.samples(1, :), out.samples(2, :))

%%
initSampler;

rng(1);
% iter = 100000;
% dimensions = [100, 500, 1000, 2000, 4000, 6000, 8000, 10000];
n_sizes = [1000, 10000, 100000];
len_sizes = length(n_sizes);
dimensions = [100, 500, 1000, 2000];
len_dim = length(dimensions);
times = inf(len_dim, len_sizes);
samples = cell(len_dim, len_sizes);

for i = 1:len_dim
    for j = 1:len_sizes
        d = dimensions(i);
        n = n_sizes(j);
        
        lb = zeros(d, 1);
        ub = inf(d, 1);
        mu = zeros(d, 1);
        Sigma = eye(d);
        P = make_orthant_problem(lb, ub, mu, Sigma);
        
        tic;
        plan = prepare(P);
        out = sample(plan, n);
        t = toc;

        times(i, j) = t;
        samples{i, j} = out.samples';
    end
end

save(['rhmc_', date, '.mat'], 'times', 'samples', 'dimensions');
% save('rhmc_2020_2_18.mat', 'times', 'samples', 'dimensions')

%% dense covariance
initSampler;

rng(1);
iter = 1000;
% dimensions = [100, 500, 1000, 2000, 4000, 6000, 8000, 10000];
dimensions = [100, 500, 1000, 2000, 4000, 6000];
len_dim = length(dimensions);
times = inf(len_dim, 1);
samples = cell(len_dim, 1);

for i = 1:len_dim
    d = dimensions(i);
    
    lb = .5 * ones(d, 1);
    ub = ones(d, 1);
    
    mu = zeros(d, 1);
    Sigma = .5 * eye(d) + .5 * ones(d, 1) * ones(1, d);
    P = make_orthant_problem(lb, ub, mu, Sigma);
    
    tic;
    plan = prepare(P);
    out = sample(plan, iter);
    t = toc;
    
    times(i) = t;
    samples{i} = out.samples;
end

save('rhmc_2020_3_4.mat', 'times', 'samples', 'dimensions')