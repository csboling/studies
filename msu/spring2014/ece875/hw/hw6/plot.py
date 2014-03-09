import matplotlib.pyplot as plt
import numpy as np

from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

xs  = np.linspace(-0.5e-4, 1.5e-4, 1000)
E   = np.zeros(xs.size)

q    = 1.6e-19
es   = 12.9*8.85e-14


bar  = 0.8 * q
ND   = [1e15, 1e17, 1e18]
psi  = [.64,  .76,  .82]
WD   = [9.36e-5, 1.02e-5, 3.37e-6]

plt.rc('text', usetex=True)

for i in xrange(len(ND)):
  a = q * psi[i] / (WD[i])**2

  it = np.nditer(xs, flags=['f_index'])
  while not it.finished:
    if       it[0] < 0:
      E[it.index] = 0
    elif 0 < it[0] < WD[i]:
      E[it.index] = a*(it[0] - WD[i])**2 + bar - q*psi[i]
    else: 
      E[it.index] = bar - q*psi[i]
    it.iternext()

  plt.plot(xs, E, label=r'$N_D = 10^{%d}$' % np.log10(ND[i]), linestyle=linecycler.next())


plt.title('Energy band diagram')
plt.xlabel('distance (cm)')
plt.ylabel(r'$E - E_F$ (J)')
plt.legend(loc='best')


plt.show()

