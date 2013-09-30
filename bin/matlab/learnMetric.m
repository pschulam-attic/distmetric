function A = learnMetric(X,Y,nquants)

[n, p] = size(X);

q = quantile(Y,linspace(0,1,nquants+1));
labs = zeros(n,1);

for i = 1:nquants
    q1 = q(i);
    q2 = q(i+1);
    
    if i == 1
        labs(Y >= q1 & Y <= q2) = i;
    else
        labs(Y > q1 & Y <= q2) = i;
    end
end

A = MetricLearning(@ItmlAlg,labs,X,eye(p));