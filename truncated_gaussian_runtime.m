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

%% d = 100
P100 = Problem;
P100.lb=zeros(100,1);
P100.Aineq = zeros(1, P100.n);
P100.Aineq(1) = -1;
P100.Aineq(2) = -1;
P100.bineq = -1;
P100.df=@(x) x;

iter = 1000;

tic;
plan = prepare(P100, struct('display', 1));
out = sample(plan, iter);
t = toc;

%% d = 1000
P1000 = make_orthant_problem(1000, 0, inf);

tic;
plan = prepare(P1000, struct('display', 1));
out = sample(plan, iter);
t = toc;

%% d = 2000
P2000 = make_orthant_problem(2000, 0, inf);

tic;
plan = prepare(P2000, struct('display', 1));
out = sample(plan, iter);
t = toc;

%% d = big
P = make_orthant_problem(4000, 0, inf);

tic;
plan = prepare(P, struct('display', 1));
out = sample(plan, iter);
t = toc;
