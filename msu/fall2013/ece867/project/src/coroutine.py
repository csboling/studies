def coroutine(f):
  def start(*args, **kwargs):
    g = f(*args, **kwargs)
    g.next()
    return g
  return start

@coroutine
def broadcast(targets):
  while True:
    msg = (yield)
    for target in targets:
      target.send(msg)

@coroutine
def disperse(targets):
  while True:
    results = (yield)
    for i in xrange(len(results)):
      targets[i].send(results[i])

@coroutine
def cascade(blocks):
  feeder = block(cascade(blocks[1:]))
  while True:
    msg = (yield)
    feeder.send(msg)

@coroutine
def circbuf(v, debug=False):
  if debug:
   print 'destination array:',v.shape
  while True:
    for i in xrange(len(v)):
      x = (yield)
      if debug:
        print 'slot',i,' = ',x.shape
      v[i] = x

