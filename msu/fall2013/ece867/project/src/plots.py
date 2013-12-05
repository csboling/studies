import numpy as np
import matplotlib.pyplot as plt
import scipy

def plot_wavelets(z, wavelet, levels):
  plt.figure()

  Asym, Dsym = [[]], [[]]
  for i in xrange(1, levels+1):
    H = aic.dwtmat(z.size, wavelet, i)
    w = np.dot(H, z)
    Asym.append(w[:w.size/2])
    Dsym.append(w[w.size/2:])

  Ahaar, Dhaar = [[]], [[]]
  for i in xrange(1, levels+1):
    H = aic.dwtmat(z.size, 'haar', i)
    w = np.dot(H, z)
    Ahaar.append(w[:w.size/2])
    Dhaar.append(w[w.size/2:])

  plt.subplot(334)
  y = np.hstack((Asym[4], Dsym[4], Dsym[3]))
  x = np.arange(y.size)
  plt.title('A4, D4, D3 - symlet')
  plt.stem(x,y)

  plt.subplot(331)
  recon = pywt.waverec([Asym[4], Dsym[4]], wavelet)
  reconsym4 = scipy.signal.resample(recon, z.size)
  plt.title('A4, D4 - symlet')
  plt.plot(reconsym4)

  plt.subplot(337)
  recon = pywt.waverec([Asym[4], Dsym[4], Dsym[3]],wavelet)
  reconsym43 = scipy.signal.resample(recon, z.size)
  plt.title('A4, D4, D3 - symlet')
  plt.plot(reconsym43)

  plt.subplot(335)
  y = np.hstack((Ahaar[4], Dhaar[4], Dhaar[3]))
  x = np.arange(y.size)
  plt.title('A4, D4, D3 - haar')
  plt.stem(x,y)

  plt.subplot(332)
  recon = pywt.waverec([Ahaar[4], Dhaar[4]],'haar')
  reconhaar4 = scipy.signal.resample(recon, z.size)
  plt.title('A4, D4 - haar')
  plt.plot(reconhaar4)

  plt.subplot(338)
  recon = pywt.waverec([Ahaar[4], Dhaar[4], Dhaar[3]],'haar')
  reconhaar43 = scipy.signal.resample(recon, z.size)
  plt.title('A4, D4, D3 - haar')
  plt.plot(reconhaar43)

  normsym  = reconsym4  / np.max(reconsym4)
  normhaar = reconhaar4 / np.max(reconhaar4)
  normorig = z / np.max(z)
  print 'sym MSE without d3:' , ((normsym - normorig)**2).mean(), 
  print 'sparsity', np.linalg.norm(np.hstack((Ahaar[4], Dhaar[4])),0)/float(z.size)
  print 'haar MSE without d3:', ((normhaar - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Asym[4], Dsym[4])),0)/float(z.size)
  plt.subplot(333)
  plt.plot(normorig)
  plt.plot(normsym)
  plt.plot(normhaar)
  print

  normsym  = reconsym43  / np.max(reconsym43)
  normhaar = reconhaar43 / np.max(reconhaar43)
  print 'sym MSE with d3:' , ((normsym - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Ahaar[4], Dhaar[4], Dhaar[3])),0)/float(z.size)
  print 'haar MSE with d3:', ((normhaar - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Asym[4], Dsym[4], Dsym[3])),0)/float(z.size)
  plt.subplot(339)
  plt.plot(normorig)
  plt.plot(normsym)
  plt.plot(normhaar)

def plot_aic(invec, alpha_in, t_recon, alpha, recon, filtered):

  plt.figure()
  plt.subplot(411)
  plt.plot(invec)
  plt.title('Original waveform')

  plt.subplot(412)
  y = alpha_in#np.dot(Psi, invec)
  x = np.arange(y.size)
  plt.stem(x,y)
  plt.title('Wavelet coefficients')

  plt.subplot(414)
  y = alpha
  x = np.arange(y.size)
  plt.stem(x,y)
  plt.title('BCR-recovered coefficients')

  plt.subplot(413)
  if t_recon != None:
    print 'trivial reconstruction MSE:',
    print ((t_recon - invec)**2).mean()
  print 'bcr reconstruction MSE:',
  print ((recon - invec)**2).mean()
  plt.plot(invec[:200], label='Original')
  if t_recon != None:
    plt.plot(t_recon[:200], label='Trivial reconstruction')
  plt.plot(recon[:200], label='BCR reconstruction')
  if filtered != None:
    plt.plot(filtered[:200], label='Filtered BCR')
  plt.title('Reconstructed waveforms')
  plt.legend(loc='best')

