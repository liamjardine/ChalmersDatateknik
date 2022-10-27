
# Get libaries
import math
from typing import Concatenate

# Define functions

def exponent(n, exp):
    sum = n
    for i in range(1,exp):
        sum *=  n
    return sum

# Print

print(exponent(10, 3))
print("Hello World!")

