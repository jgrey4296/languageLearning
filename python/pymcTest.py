import numpy as np
import matplotlib.pyplot as plt
import IPython
#https://ipython.readthedocs.io/en/stable/config/options/terminal.html
#IPython.embed(simple_prompt=True)
#in shell: ipython --simple-prompty --matplotlib

# init random number generator
np.random.seed(123)

#True parameter values
alpha, sigma = 1, 1
beta = [1, 2.5]

#size of dataset
size = 100

#predicator variable
X1 = np.random.randn(size)
X2 = np.random.randn(size) * 0.2

#simulate outcome
Y = alpha + beta[0]*X1 + beta[1]*X2 + np.random.randn(size)*sigma

#draw the data
fig, axes = plt.subplots(1, 2, sharex=True, figsize=(10,4))
axes[0].scatter(X1, Y)
axes[1].scatter(X2, Y)
axes[0].set_ylabel('Y')
axes[0].set_xlabel('X1')
axes[1].set_xlabel('X2')



#pymc:
import pymc3 as pm
basic_model = pm.Model()

with basic_model:
    #priors
    alpha = pm.Normal('alpha', mu=0, sd=10)
    beta = pm.Normal('beta', mu=0, sd=10, shape=2)
    sigma = pm.HalfNormal('sigma', sd=1)

    #expected value:
    mu = alpha + beta[0] * X1 + beta[1]*X2

    #likelihood
    Y_obs = pm.Normal('Y_obs', mu=mu, sd=sigma, observed=Y)

map_estimate = pm.find_MAP(model=basic_model)

print(map_estimate)

from scipy import optimize

map_estimate_2 = pm.find_MAP(model=basic_model, fmin=optimize.fmin_powell)

print("\n-----\n")
print(map_estimate_2)

IPython.embed(simple_prompt=True)
