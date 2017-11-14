# -*- coding: utf-8 -*-
"""
Created on Fri Nov 10 12:24:35 2017

@author: roel
"""
import pandas as p
import numpy as np
import matplotlib.pyplot as plt
import random

random.seed(100)

d = p.read_csv('dataset.csv',delimiter=';',encoding='latin1')

# Get participants per year
g = d.groupby(by=['year','type']).count().reset_index()
g = g[['year','type','order']]
g = g.rename(columns={'order':'participants'})
d = d.merge(g, how='left', on=['year','type'])

# Get relative numbers per year & type
d = d.assign(
        rOrder = d.order/d.participants,
        rPosition = d.position/d.participants,
        rScore = d.score/(d.maxpoints*(d.voters - 1)))

# Split into two groups
d = d.sort_values(by=['rOrder'])
dFirstHalf = d.iloc[0:(int(round(len(d)/2,-1))+1)]['rScore'].as_matrix()
dSecondHalf = d.iloc[int(round(len(d)/2,-1))+1:(len(d))]['rScore'].as_matrix()
dMean = np.mean(dSecondHalf) - np.mean(dFirstHalf)

# Bootstrap resampling function
# Thanks at https://gist.github.com/aflaxman/6871948
def bStrap(X, n=None):
    if n == None:
        n = len(X)
    resample_i = np.floor(np.random.rand(n)*len(X)).astype(int)
    X_resample = X[resample_i]
    return X_resample

# Function for calculating mean difference between two (bootstrapped) samples
def bStrapMeanDiff(d1,d2):
    d1s = bStrap(d1)
    d2s = bStrap(d2)
    dMeanDiff = np.mean(d1s) - np.mean(d2s)
    return dMeanDiff

# Run 1000 bootstrapped sample differences
b = int(1000)
dMeanDiffs = np.zeros(b)
for x in range(0,b):
    dMeanDiffs[x] = bStrapMeanDiff(dSecondHalf,dFirstHalf)

# Calculate p = 0.95 CI
dq = np.percentile(dMeanDiffs,[2.5,97.5])

plt.hist(dMeanDiffs,bins='auto',density=True)
plt.axvline(dMean,color='green')
plt.axvline(dq[0],color='red')
plt.axvline(dq[1],color='red')
plt.show()
        