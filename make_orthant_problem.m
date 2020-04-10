function problem = make_orthant_problem(lb, ub, mu, Sigma)
%make_orthant_problem 
%   Detailed explanation goes here

L = chol(Sigma, 'lower');
invL = inv(L);
problem = Problem;
% problem.lb = lb * ones(dim, 1);
% problem.ub = ub * ones(dim, 1);
% problem.Aineq = [invL, invL];
% problem.bineq = [lb; ub] + repmat(mu, 2, 1);
problem.lb = lb;
proble.ub = ub;

problem.df=@(x) x;

end

