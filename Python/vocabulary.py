from pandas import read_csv
import os
import pystan


dat = read_csv('Data/vocabulary.csv')
fit0_stan = pystan.StanModel(file = 'Stan/vocabulary0.stan')