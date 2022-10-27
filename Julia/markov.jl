using LinearAlgebra, Symbolics, NLsolve

P =[0   1/3  1/3   1/3;
    0   0    1/2   1/2; 
    1   0    0     0;
    1/2 1/2  0     0]

I =[1 0 0 0;
    0 1 0 0; 
    0 0 1 0;
    0 0 0 1]

u = [1  0   0   0]

function three_surfs(P)
    l = sqrt(length(P))
    
    for k = 1:3
        P = P[rand(1:l),:]'*P
    end
    return P
end

function f!(F, v)
    a = v[1]
    b = v[2]
    c = v[3]
    d = v[4]
    F[1] = a-c-d/2
    F[2] = b-a/3-d/2
    F[3] = c-
    F[4] = x4
end