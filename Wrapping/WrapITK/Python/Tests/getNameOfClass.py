#!/usr/bin/env python

# a short program to check the value returned by the GetNameOfClass() methods

import itk, sys
itk.auto_progress(2)

# must force the load to return all the names with dir(itk)
itk.force_load()
# itk.ImageToImageFilter

# a list of classes to exclude. Typically, the classes with a custom New() method, which return a subclass
# of the current class
exclude = ["FFTRealToComplexConjugateImageFilter", "FFTComplexConjugateToRealImageFilter", "OutputWindow", "FFTComplexToComplexImageFilter"]

wrongName = False

for t in dir(itk):
  if t not in exclude:
    T = itk.__dict__[t]
    # first case - that's a templated class
    if isinstance(T, itk.Vector.__class__) and len(T)>0:
      # use only the first specialization - all of them return the same name
      i = T.values()[0]
      # GetNameOfClass() is a virtual method of the LightObject class, so we must
      # instantiate an object with the New() method
      if 'New' in dir(i):
        I = i.New()
        # be sure that the type of the instantiated object is the same than the
        # one of the class. It can be different if the class is an "abstract" one
        # and don't provide any New() method. In that case, the one of the superclass
        # is used.
        if 'GetNameOfClass' in dir(I):
          print "Checking", t
          n = I.GetNameOfClass()
          if n != t and itk.class_(I) == i:
            print >> sys.stderr, t, "doesn't provide the right name."
            wrongName = True
    else:
      if 'New' in dir(T):
        I = T.New()
        if 'GetNameOfClass' in dir(I):
          print "Checking", t
          n = I.GetNameOfClass()
          if n != t and itk.class_(I) == T:
            print >> sys.stderr, t, "doesn't provide the right name."
            wrongName = True


if wrongName:
  print >> sys.stderr, "Some classes are not providing the correct name."
  sys.exit(1)
