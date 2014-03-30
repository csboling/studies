import matplotlib.pyplot as plt
import numpy as np

from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

xs  = np.linspace(-0.8, 0.1, 1000)
Q   = np.zeros(xs.size)

q    = 1.6e-19
es   = 11.9*8.85e-14
ei   = 3.9 *8.85e-14

ND   = 1e16
ni   = 9.65e9
psiB = 0.0259 * np.log(ND / ni)
alpha = (ni/ND)*np.sqrt(2 * np.log(ND / ni))
beta  = 1 / 0.0259

LD = np.sqrt(es / (q * ND * beta))
coeff = np.sqrt(2) * es * 0.0259 / LD

plt.rc('text', usetex=True)

it = np.nditer(xs, flags=['f_index'])
while not it.finished:
  if -2*psiB < it[0] < 0:
    Q[it.index] = coeff*np.sqrt(beta * -it[0])
  elif it[0] < -2*psiB: 
    Q[it.index] = coeff*alpha*np.exp(0.5*beta * np.abs(it[0]))
  elif it[0] > 0:
    Q[it.index] = coeff*alpha*np.exp(0.5*beta * (it[0] + 2*psiB)) - coeff*np.sqrt(beta*2*psiB)
  else: 
    Q[it.index] = 0
  it.iternext()


fig = plt.figure()
ax = fig.add_subplot(111)

line, = ax.plot(xs, Q)

x = -2*psiB
y = coeff*np.sqrt(beta*-x)
ax.annotate('strong inversion point: $\psi_s = $%.3f V,\n $Q_s =$ %.3e V/cm$^2$' % (x,y), 
            xy=(x,y),
            xytext=(-0.5, 2e-7),
            arrowprops=dict(facecolor='black', shrink=0.05))



plt.xlim([-0.8, 0.1])
plt.title('Surface charge per unit area')
plt.xlabel('Surface potential (V)')
plt.ylabel('Surface charge (C / cm$^2$)')


plt.show()

