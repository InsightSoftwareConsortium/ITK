#!/usr/bin/env python

import sys, re, itk, os
from sys import argv

# python 2.3 compatibility
if sys.version < '2.4' :
        # set compatibility
        import sets
        set = sets.Set

        def sorted(iterable, cmp=None, key=None, reverse=False) :
	    i = list(iterable)
	    if key :
		d = {}
		for v in iterable :
		    k = key(v)
		    if not d.has_key(k) :
			d[k] = []
		    d[k].append(v)
		keys = d.keys()
		keys.sort(cmp)
		i = []
		for k in keys :
		    i += d[k]
	    else :
		i.sort(cmp)
	    if reverse :
		i.reverse()
	    return i

# declares filter which will not be wrapped
excluded = set([
  "UnaryFunctorImageFilter",
  "ReconstructionImageFilter",
  "PadImageFilter",
  "ObjectMorphologyImageFilter",
  "MovingHistogramDilateImageFilter",
  "MovingHistogramErodeImageFilter",
  "MovingHistogramImageFilter",
  "MovingHistogramMorphologicalGradientImageFilter",
  "MovingHistogramMorphologyImageFilter",
  "MorphologyImageFilter",
  "FFTWRealToComplexConjugateImageFilter",
  "FFTWComplexConjugateToRealImageFilter",
  "FFTRealToComplexConjugateImageFilter",
  "FFTComplexConjugateToRealImageFilter",
  "SCSLComplexConjugateToRealImageFilter",
  "SCSLRealToComplexConjugateImageFilter",
  "BinaryMorphologyImageFilter",
  "BinaryFunctorImageFilter",
  "TernaryFunctorImageFilter",
  "ShiftScaleInPlaceImageFilter",
  "FastIncrementalBinaryDilateImageFilter",
  "BasicMorphologicalGradientImageFilter",
  "TwoOutputExampleImageFilter",
  "NaryFunctorImageFilter",
  "NonThreadedShrinkImageFilter",
  "RegionGrowImageFilter",
  "ConnectedComponentFunctorImageFilter",
  "BasicDilateImageFilter",
  "BasicErodeImageFilter",
  "BasicErodeImageFilter",
  "AdaptImageFilter",
  ])


# get filters from sources
headers = []
for d in argv[1:]:
  headers += sum([ f for p,d,f in os.walk(d) ], [])
filters = set([f[len('itk'):-len('.h')] for f in headers if f.endswith("Filter.h")]) - excluded

# get filter from wrapper files
# remove filters which are not in the toolkit (external projects, PyImageFilter, ...)
wrapped = set([a for a in dir(itk) if a.endswith("Filter")]).intersection(filters)

nonWrapped = filters - wrapped


# print non wrapped filters without much text to stdout, so they can be easily reused
for f in sorted(nonWrapped) :
	print f

# and print stats in stderr to avoid poluting the list above
print >>sys.stderr
print >>sys.stderr, '%i filters' % len(filters)
print >>sys.stderr, '%i wrapped filters' % len(wrapped)
print >>sys.stderr, '%i non wrapped filters' % len(nonWrapped)
print >>sys.stderr, '%f%% covered' % (len(wrapped) / float(len(filters)) * 100)
print >>sys.stderr


# the goal is to return a non zero value if coverage is not 100%
# but we are not yet at this stage !
#
# return len(nonWrapped)

