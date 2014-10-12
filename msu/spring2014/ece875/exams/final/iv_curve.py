import matplotlib.pyplot as plt
import numpy as np

from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

vs  = np.linspace(-1.8, 0.1, 1000)
I   = np.zeros(vs.size)

Vt  = -1.746
m   = 0.102

plt.rc('text', usetex=True)

it = np.nditer(vs, flags=['f_index'])
while not it.finished:
  I[it.index] =10**(-3 -(it[0] - Vt)/m)
  it.iternext()

fig = plt.figure()
ax = fig.add_subplot(111)
line = ax.semilogy(vs, I)

x = 0
y = 10**(-3 + Vt/m)
ax.annotate('gate voltage = 0, drain current = %.3e A' % y,
            xy=(x,y),
            xytext=(x-1, y),
            arrowprops=dict(facecolor='black', shrink=0.05))

plt.title('Current / voltage characteristic')
plt.xlabel('Gate voltage (V)')
plt.ylabel('Drain current (A)')


plt.show()

