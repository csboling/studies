import numpy as np

NC = 2.8e19
barrier = 0.65
builtin = 0.5259
es = 11.9 * 8.85e-14
q = 1.6e-19

iteration = 1
deltaphi = 0
prevND = 0

while True:
  print 'iteration %d' % iteration
  print 'deltaphi: %.5e' % deltaphi
  ND = NC * np.exp((barrier - builtin + deltaphi)/(-0.0259))
  print 'ND: %.5e' % ND
  Em = np.sqrt((q*ND) / es)
  print 'Em: %.5e' % Em
  deltaphi = np.sqrt(q * Em / (4 * np.pi * es))

  if np.abs(prevND - ND) < 0.01e16:
    break
  prevND = ND
  iteration += 1
  print
