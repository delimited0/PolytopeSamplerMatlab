%% Demo to sample from a flow polytope
initSampler

%% Form the problem P
load('Recon2.v04.mat')
P = Problem;
P.Aeq = modelR204.S;
P.beq = modelR204.b;
P.lb = modelR204.lb;
P.ub = modelR204.ub;

%% Generate samples from P
iter = 1000;

% problem 20...
opts = struct;
opts.display = 1;
opts.recordInterval = 10;
%opts.trajLength = 2;
%opts.maxRelativeStepSize = 0.1;
plan = prepare(P, opts);
tic;

out = {};
for i = 1:1000
i
rng(i*1494+7541)
out{i} = sample(plan, iter);
out{i}.samples = [];
out{i}.samplesFullDim = single(out{i}.samplesFullDim);
end
t = toc;

out = combineSamples(out); % combine the samples

%% Output the result
fprintf('Total time = %f sec\n', t)

[ess] = effectiveSampleSize(out.samplesFullDim);
fprintf('Mixing time = %f iter\n', size(out.samplesFullDim,2) / min(ess))

p = unifScaleTest(out, plan, struct('toPlot',1));
fprintf('p value = %f\n', p)

plot(out.samplesFullDim(2835,:))