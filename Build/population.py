#!/usr/bin/env python 
import numpy as np
import matplotlib.pyplot as plt
from scipy.io import FortranFile

# Read in the population file
population = FortranFile('population.pop', 'r')


population=population.read_ints()

years=np.arange(0,len(population))


plt.plot(years,population)
plt.title("Population Growth")
plt.xlabel("Year")
plt.ylabel("Population")

plt.show()

