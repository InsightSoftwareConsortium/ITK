import time

start = time.time()
import itk
itk.force_load()
print "load:", time.time() - start

img = itk.Image.UC2.New(Regions=(10,20))
img.Allocate()

def index(it):
    idx = itk.Index[2]()
    for dummy in range(0, it):
        img.GetPixel(idx)

def index2(it):
    idx = itk.Index[2]()
    for dummy in range(0, it):
        idx[0] = 0
        idx[1] = 0
        img.GetPixel(idx)

def integer(it):
    for dummy in range(0, it):
        img.GetPixel(0)

def pylist(it):
    l = [0, 0]
    for dummy in range(0, it):
        img.GetPixel(l)

def pytuple(it):
    l = (0, 0)
    for dummy in range(0, it):
        img.GetPixel(l)

def new(it):
    for dummy in range(0, it):
        lo = itk.Image.UC2.__New_orig__()

def extended_new(it):
    for dummy in range(0, it):
        lo = itk.Image.UC2.New()


def run(f, it):
    start = time.time()
    f(it)
    return time.time() - start


it = 1000000
print "index:", run(index, it)
print "index2:", run(index2, it)
print "integer:", run(integer, it)
print "list:", run(pylist, it)
print "tuple:", run(pytuple, it)
print "new:", run(new, 100000)
print "extended_new:", run(extended_new, 100000)
