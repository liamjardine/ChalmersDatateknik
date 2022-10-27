

function ci(mean, sd, n, z)
    a = mean - z * sd/sqrt(n)
    b = mean + z * sd/sqrt(n)
    return("[" + a + b +"]")
end