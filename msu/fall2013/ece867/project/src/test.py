from functools import partial

import numpy as np
import scipy.io, scipy.signal
import matplotlib.pyplot as plt
import pywt

import aic
import coroutine as cr

Fs = 24000.0
channels = 4
dft_size = 128
levels = 1
wavelet = 'sym4'

PLOT_FFT      = 0
PLOT_WAVELET  = 0
PLOT_RECON    = 1

def plot_fft(sig):
  z0 = np.squeeze(sig)
  Fz = np.fft.fft(z0)

  freq = np.fft.fftfreq(z0.size)
  plt.plot(freq, np.abs(Fz))

def threshold(x, thresh=0.5):
  if abs(x) > thresh:
    return x
  else:
    return 0

sinecount = np.floor(5*np.random.rand(channels)) + 1
max_sines = np.max(sinecount)
f = np.zeros((channels, max_sines))
for i in xrange(f.shape[0]):
  f[i] = Fs/2 * np.hstack((np.random.rand(sinecount[i]), 
                           [0]*(max_sines-sinecount[i])))
coeffs = 5 * np.random.rand(channels, max_sines)
w = f/Fs

# import Quiroga et al's simulated data set 
# (from http://www2.le.ac.uk/departments/engineering/research/
#              bioengineering/neuroengineering-lab/software)
# and filter it between 500 and 7500 Hz to examine spike data
extracel = scipy.io.loadmat('quiroga_simulation_1.mat')['data'][0,...]
bw_b, bw_a = scipy.signal.butter(1, (300/Fs, 3000/Fs))
ec_filtered = scipy.signal.lfilter(bw_b, bw_a, extracel)

t  = np.arange(5*int(Fs))
#ys = np.array([[np.sin(2*np.pi*w0*t) for w0 in ch] for ch in w])
#z  = np.dot(coeffs, ys)[0,...,...]
z = np.array([ec_filtered[t.size*i + t] for i in xrange(channels)])

offset = 4096#26385
windowsize = 512
z0 = np.asarray(z[0])[offset:offset+windowsize]
z1 = np.asarray(z[0])[26385:26385+windowsize]

if (PLOT_WAVELET): 
  plt.figure()

  Asym, Dsym = [[]], [[]]
  for i in xrange(1, levels+1):
    H = aic.dwtmat(z0.size, wavelet, i)
    w = np.dot(H, z0)
    Asym.append(w[:w.size/2])
    Dsym.append(w[w.size/2:])

  Ahaar, Dhaar = [[]], [[]]
  for i in xrange(1, levels+1):
    H = aic.dwtmat(z0.size, 'haar', i)
    w = np.dot(H, z0)
    Ahaar.append(w[:w.size/2])
    Dhaar.append(w[w.size/2:])

  plt.subplot(334)
  y = np.hstack((Asym[4], Dsym[4], Dsym[3]))
  x = np.arange(y.size)
  plt.title('A4, D4, D3 - symlet')
  plt.stem(x,y)

  plt.subplot(331)
  recon = pywt.waverec([Asym[4], Dsym[4]],wavelet)
  reconsym4 = scipy.signal.resample(recon, z0.size)
  plt.title('A4, D4 - symlet')
  plt.plot(reconsym4)

  plt.subplot(337)
  recon = pywt.waverec([Asym[4], Dsym[4], Dsym[3]],wavelet)
  reconsym43 = scipy.signal.resample(recon, z0.size)
  plt.title('A4, D4, D3 - symlet')
  plt.plot(reconsym43)

  plt.subplot(335)
  y = np.hstack((Ahaar[4], Dhaar[4], Dhaar[3]))
  x = np.arange(y.size)
  plt.title('A4, D4, D3 - haar')
  plt.stem(x,y)

  plt.subplot(332)
  recon = pywt.waverec([Ahaar[4], Dhaar[4]],'haar')
  reconhaar4 = scipy.signal.resample(recon, z0.size)
  plt.title('A4, D4 - haar')
  plt.plot(reconhaar4)

  plt.subplot(338)
  recon = pywt.waverec([Ahaar[4], Dhaar[4], Dhaar[3]],'haar')
  reconhaar43 = scipy.signal.resample(recon, z0.size)
  plt.title('A4, D4, D3 - haar')
  plt.plot(reconhaar43)

  normsym  = reconsym4  / np.max(reconsym4)
  normhaar = reconhaar4 / np.max(reconhaar4)
  normorig = z0 / np.max(z0)
  print 'sym MSE without d3:' , ((normsym - normorig)**2).mean(), 
  print 'sparsity', np.linalg.norm(np.hstack((Ahaar[4], Dhaar[4])),0)/float(z0.size)
  print 'haar MSE without d3:', ((normhaar - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Asym[4], Dsym[4])),0)/float(z0.size)
  plt.subplot(333)
  plt.plot(normorig)
  plt.plot(normsym)
  plt.plot(normhaar)
  print

  normsym  = reconsym43  / np.max(reconsym43)
  normhaar = reconhaar43 / np.max(reconhaar43)
  print 'sym MSE with d3:' , ((normsym - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Ahaar[4], Dhaar[4], Dhaar[3])),0)/float(z0.size)
  print 'haar MSE with d3:', ((normhaar - normorig)**2).mean(),
  print 'sparsity', np.linalg.norm(np.hstack((Asym[4], Dsym[4], Dsym[3])),0)/float(z0.size)
  plt.subplot(339)
  plt.plot(normorig)
  plt.plot(normsym)
  plt.plot(normhaar)

#test_data = 0.5*np.random.normal(size=(channels, windowsize))
test_data = np.zeros((channels, windowsize))
test_data[0] = z1#z[0, :windowsize]
test_data[1] = z[2, :windowsize]
test_data[2] = z[3, :windowsize]
test_data[3] = z0#[3, :windowsize]

#test_data = z[..., :windowsize]

if PLOT_RECON:
  o0 = np.zeros(test_data.shape[1])
  t_recon0 = np.zeros(test_data.shape[1])

  # transform matrix for a basis in which the signals
  # are believed to be sparse
  D = [[]]
  for i in xrange(levels):
    H = aic.dwtmat(windowsize, wavelet, i+1)
    newA, newD = np.split(H,2)
    D.append(newD)
#  Psi = np.vstack((newA, D[4], D[3], D[2], D[1]))
  Psi = np.vstack((newA, D[1]))
  recon0   = np.zeros((5, Psi.shape[0]))
  converter = aic.cmux(Fs, channels)

  # build a pipeline
  t_recon     = converter.triv_recon(windowsize, 0, cr.circbuf(t_recon0))
  recon       = converter.reconstruct(Psi, windowsize, 0, cr.circbuf(recon0))
  bcr_recon   = converter.bcr_recon(levels, Psi, recon, lasso=True, 
                k=0.001, iteration_cap=100)


  aic_targets = cr.broadcast([cr.circbuf(o0),
                              t_recon,
                              bcr_recon])
  #aic_out = converter.bypass(aic_targets, 0)
  aic_out     = converter.out(aic_targets)

  # push the data through
  print test_data.shape
  for x in test_data.T:
    aic_out.send(x)

  plt.subplot(411)
  plt.plot(test_data[0])

  plt.subplot(412)
  y = np.dot(Psi, test_data[0])
  x = np.arange(y.size)
  plt.stem(x,y)

  threshed = np.vectorize(threshold)(y)
  sparsity = np.linalg.norm(threshed,0)/float(y.size)
  print 'input sparsity:', sparsity

#  recon0[0] = np.vectorize(threshold)(recon0[0])
  plt.subplot(413)
  y = recon0[0]
  x = np.arange(y.size)
  plt.stem(x,y)

  reconD = [[]]
  rem = recon0[0]
  for i in xrange(levels):
    rem, newD = np.split(rem, 2)
    reconD.append(newD)
#  A, D = np.split(recon0[0], 2)
  bcr_recon0 = pywt.waverec((rem, None), wavelet, 1)
  bw_b, bw_a  = scipy.signal.bessel(1, (300/Fs, 3000/Fs))
  rc_filtered = scipy.signal.lfilter(bw_b, bw_a, bcr_recon0)
#  bcr_recon0 = pywt.waverec([rem, reconD[4], reconD[3], reconD[2], reconD[1]], wavelet)
  #bcr_recon0 = scipy.signal.resample(bcr_recon0, windowsize)

  plt.subplot(414)
  norm_orig0    = test_data[0] / np.max(test_data[0])
  norm_t_recon0 = t_recon0 / np.max(t_recon0)
  norm_b_recon0 = bcr_recon0 / np.max(bcr_recon0)
  print 'trivial reconstruction MSE:',
  print ((norm_t_recon0 - norm_orig0)**2).mean()
  print 'bcr reconstruction MSE:',
  print ((norm_b_recon0 - norm_orig0)**2).mean()
  plt.plot(norm_orig0)
  plt.plot(norm_t_recon0)
  plt.plot(norm_b_recon0)
 
  

#  plt.subplot(311)
#  plot_fft(z[0])

#  plt.subplot(312)
#  plot_fft(z[1])

#  plt.subplot(313)
#  plot_fft(t_recon0)

#  plt.subplot(335)
#  Hx = np.dot(aic.haarmat(z0.size), z0)
#  plt.plot(Hx[:z0.size/2])
#  plot_fft(t_recon0)

#  plt.subplot(336)
#  plt.plot(Hx[z0.size/2:])
#  plot_dwt(t_recon0, wavelet)

#  plt.subplot(339)
#  plt.plot(recon0)  

#  plt.subplot(337)
#  inv_haar_recon0 = np.dot(aic.haarmat(recon0.size).T, recon0)
#  plt.plot(inv_haar_recon0)
  
#  plt.subplot(338)
#  plot_fft(inv_haar_recon0)

#  plt.subplot(336)
#  plot_dwt(recon0, wavelet)
#  print recon[0].shape
#  print recon[0]
#  freq_recon = np.fft.fftfreq(recon0.shape[0])
#  plt.subplot(414)
#  plt.plot(freq_recon, np.abs(recon[0]))

plt.show()
