import matplotlib.pyplot as plt
import numpy as np

from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

V  = np.arange(-1, 0.7, 0.1)
C1 = np.array([22, 22.5, 23, 23.5, 24.1, 24.8, 25.5, 26.4, 27.3, 28.5, 29.8, 31.5, 33.6, 36.4, 40.4, 47.3, 63.8])
C2 = np.array([71.6, 73.9, 76.5, 79.3, 82.4, 86.0, 90.0, 94.7, 100.2, 106.7, 114.8, 125.0, 138.5, 157.7, 187.9, 246.7, 469.2])

Y1 = np.reciprocal(np.square(C1))
Y2 = np.reciprocal(np.square(C2))
cubeY1 = np.reciprocal(np.power(C1, 3))

slope1 = (Y1[5] - Y1[0]) / (V[5] - V[0])
slope2 = (Y2[5] - Y2[0]) / (V[5] - V[0])

cubeslope1 = (cubeY1[5] - cubeY1[0]) / (V[5] - V[0])
print 'slope vs 1/C_1^2', slope1
print 'slope vs 1/C_1^3', cubeslope1
print 'slope vs 1/C_2^2', slope2

xs     = np.arange(-1, 1.5, 0.1)
line1 = np.dot(slope1, xs) + (Y1[0] - slope1*xs[0])
line2 = np.dot(slope2, xs) + (Y2[0] - slope2*xs[0])
cubeline = np.dot(cubeslope1, xs) + (Y1[0] - cubeslope1*xs[0])

plt.figure()
#plt.plot(V, Y1, label=r'$\frac{1}{C_1^2}$', linestyle=linecycler.next())
plt.plot(V, cubeY1)
#plt.plot(xs, line1, label=r'extrapolated line')
#plt.plot(xs, np.zeros(xs.size))
plt.title(r'Junction #1')
plt.ylabel(r'$\frac{1}{C^2}$ (cm$^2$ / nF)$^2$')
plt.xlabel(r'$V$ (V)')
plt.legend(loc='best')
zindex1 = np.where(np.abs(line1) == min(np.abs(line1)))[0]
print 'zero crossing for 1/C_1^2 = ', xs[zindex1]
zindex1 = np.where(np.abs(cubeline) == min(np.abs(cubeline)))[0]
print 'zero crossing for 1/C_1^3 = ', xs[zindex1]

plt.figure()
plt.plot(V, Y2, label=r'$\frac{1}{C_2^2}$', linestyle=linecycler.next())
plt.plot(xs, line2, label=r'extrapolated line')
plt.plot(xs, np.zeros(xs.size))
plt.title(r'Junction #1')
plt.title(r'Junction #2')
plt.ylabel(r'$\frac{1}{C^2}$ (cm$^2$ / nF)$^2$')
plt.xlabel(r'$V$ (V)')
plt.legend(loc='best')
zindex2 = np.where(np.abs(line2) == min(np.abs(line2)))[0]
print 'zero crossing for 1/C_2^2 = ', xs[zindex2]

#xs  = np.linspace(-0.5e-4, 1.5e-4, 1000)
#E   = np.zeros(xs.size)
#
#q    = 1.6e-19
#es   = 12.9*8.85e-14
#
#
#bar  = 0.8 * q
#ND   = [1e15, 1e17, 1e18]
#psi  = [.64,  .76,  .82]
#WD   = [9.36e-5, 1.02e-5, 3.37e-6]
#
#plt.rc('text', usetex=True)
#
#for i in xrange(len(ND)):
#  a = q * psi[i] / (WD[i])**2
#
#  it = np.nditer(xs, flags=['f_index'])
#  while not it.finished:
#    if       it[0] < 0:
#      E[it.index] = 0
#    elif 0 < it[0] < WD[i]:
#      E[it.index] = a*(it[0] - WD[i])**2 + bar - q*psi[i]
#    else: 
#      E[it.index] = bar - q*psi[i]
#    it.iternext()
#
#  plt.plot(xs, E, label=r'$N_D = 10^{%d}$' % np.log10(ND[i]), linestyle=linecycler.next())
#
#
#plt.title(r'$\frac{1}{C^2}$ versus $V$ curves for Problem 4')
#plt.legend(loc='best')


#plt.show()

