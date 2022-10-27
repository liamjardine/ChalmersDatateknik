Pkg.add("Plots")

function f(k::Int,n::Int)
    return (1-(1/2)^(k+1))^n - (1-(1/2)^k)^n
end

function expected_value(n::Int)
    result = 0;
    for k = 1:10_000
        result += k * f(k, n)
    end
    return result
end

for m = 1:20
    print(m)
    print("     ")
    println(expected_value(2^m))
end

function random_numbers(r::Int,a::Int)
    for i = 1:a
        rand(1:r)
    end 
end