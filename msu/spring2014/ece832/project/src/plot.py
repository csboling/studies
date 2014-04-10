import sys

import numpy as np
import matplotlib.pyplot as plt

def decode(*args):
  for i in xrange(len(args)):
    return np.sum()

def main(fname):
  data = np.genfromtxt(fname, dtype=None, delimiter=',', names=True)
  plt.plot(data['V_IN_Y'], decode( data['DIGITAL_BITS3__Y']
                                 , data['DIGITAL_BITS2__Y']
                                 , data['DIGITAL_BITS1__Y']
                                 , data['DIGITAL_BITS0__Y']
                                 ))

if __name__ == '__main__':
  if len(sys.argv) > 1:
    main(sys.argv[1])
  else:
    main('data.csv')

