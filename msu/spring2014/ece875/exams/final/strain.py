import matplotlib.pyplot as plt
import numpy as np

from itertools import cycle
lines = ["-","--","-.",":"]
linecycler = cycle(lines)

T = [27, 40, 100]
S = [3.77e-3, 1.17e-2, 5.6e-2]

plt.plot(T, S)
plt.plot(T, S, 'x')

plt.xlim([25,105])
plt.title('Strain versus temperature')
plt.xlabel('Temperature (degrees C)')
plt.ylabel('Magnitude of strain')


plt.show()

