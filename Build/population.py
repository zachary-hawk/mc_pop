#!/usr/bin/env python 
import numpy as np
import matplotlib.pyplot as plt
from scipy.io import FortranFile

# Read in the population file
population = FortranFile('population.pop', 'r', '>u4')
population_dat=population.read_ints()
population.close()
years=np.arange(0,len(population_dat))

plt.figure()
plt.plot(years,population_dat)
plt.title("Population Growth")
plt.xlabel("Year")
plt.ylabel("Population")


try:
    age=FortranFile('average_age.pop', 'r', '>u4')
    age_dat=age.read_reals()
    age.close()
    years=np.arange(0,len(age_dat))
    plt.figure()
    plt.plot(years,age_dat)
    plt.title("Average Age")
    plt.xlabel("Year")
    plt.ylabel("Average age")
    
except:
    print("No age data")
    


try:
    age=FortranFile('birth_rate.pop', 'r', '>u4')
    age_dat=age.read_reals()
    age.close()
    years=np.arange(0,len(age_dat))
    plt.figure()
    plt.plot(years,age_dat)
    plt.title("Birth Rate")
    plt.xlabel("Year")
    plt.ylabel("Birth rate")
 
except:
    print("No birth rate data")


    

plt.show()

