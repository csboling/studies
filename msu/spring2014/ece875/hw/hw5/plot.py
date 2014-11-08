import matplotlib.pyplot as plt
import numpy as np

xs  = np.linspace(-1e-4, 1.5e-4, 1000)
E   = np.zeros(xs.size)
rho = np.zeros(xs.size)

Emax = 4.86e3
q    = 1.6e-19
drho = 1e19
ND   = 3e14
es   = 11.9*8.85e-14
WD   = 0.8e-4#1.87e-4
a    = 1e19

it = np.nditer(xs, flags=['f_index'])
while not it.finished:
  if -0.8e-4 <= it[0] < 0:
    rho[it.index] = -drho*WD + drho*it[0]
    E[it.index]   = -q*a/(2*es)*((WD)**2 - it[0]**2)
  elif    0 < it[0] < 1.07e-4:
    rho[it.index] = ND
    E[it.index] = -Emax + q*ND*it[0]/es
  else: 
    E[it.index] = 0
  it.iternext()

print -q*a/(2*es)*(WD)**2
    
plt.subplot(211)
plt.plot(xs, rho)
plt.title('Impurity/charge distribution')
plt.xlabel('distance (cm)')
plt.ylabel('charge (C)')

plt.subplot(212)
plt.plot(xs, E)
plt.title('Electric field')
plt.xlabel('distance (cm)')
plt.ylabel('charge (V/cm)')

plt.show()

