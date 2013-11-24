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
def printer():
  while True:
    print (yield)

class Flush(Exception):
  pass
@coroutine
def circbuf(v, target=None):
  size  = len(v)
  count = 0
  while True:
    for i in xrange(len(v)):
      try:
        v[i] = (yield)
        if count < size:
          count += 1
      except Flush:
        if target == None:
          break
        else:
          tail = i - count
          if tail < 0: tail += size
          while count:
            target.send(v[tail])
            tail  += 1
            if tail == size: tail = 0
            count -= 1
            
    
       

