function problem = make_orthant_problem(dim, lb, ub)
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

problem = Problem;
problem.lb = lb * ones(dim, 1);
problem.ub = ub * ones(dim, 1);
problem.df=@(x) x;

end

