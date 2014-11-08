import sys

import numpy as np
import matplotlib.pyplot as plt
from itertools import cycle

lines = ['-', '--', '-.', ':']

def main(bits, fname):
  data = np.genfromtxt(fname, dtype=None, delimiter=' ')
  
  plt.hold(True)
  vin      = data[0:, 0]
  voltline = np.dot(vin, 2**bits)
  ideal    = data[0:, 1]
  response = data[0:, 2]
  
  linecycler = cycle(lines)
  plt.plot(vin, voltline, label='Input   voltage',  linestyle=next(linecycler))
  plt.plot(vin, ideal,    label='Ideal   response', linestyle=next(linecycler))
  plt.plot(vin, response, label='Circuit response', linestyle=next(linecycler))

  plt.title("ADC Response")
  plt.xlabel("Normalized input voltage")
  plt.ylabel("Output code")

  plt.legend(loc='best')
  plt.show()

if __name__ == '__main__':
  if len(sys.argv) > 2:
    main(int(sys.argv[1]), sys.argv[2])
  else:
    print '''
          usage:
            python plot.py bitdepth infile
          '''

