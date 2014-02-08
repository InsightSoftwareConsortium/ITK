# -*- coding: utf-8 -*-

import sys, re, itk, os
from optparse import OptionParser

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

parser = OptionParser(usage = 'wrappingCoverage.py paths')

parser.add_option("-b", "--base", dest="base", default="Filter", help="Base string used to search for the classes (default: Filter).")
parser.add_option("-e", "--exclude", dest="exclude", default=None, help="Path of a file with one class to exclude per line (default: None).")
parser.add_option("-E", "--no-error", action="store_true", dest="noError", help="Don't generate an error code if all the classes are not wrapped.")

opts, args = parser.parse_args()

# declares classes which will not be wrapped
excluded = set([])
if opts.exclude:
    map(excluded.add, [c.strip() for c in file(opts.exclude).readlines()])

# get classes from sources
headers = []
for d in args:
    headers += sum([ f for p,d,f in os.walk(d) if "Deprecated" not in p and "TestKernel" not in p ], [])
classes = set([f[len('itk'):-len('.h')] for f in headers if f.startswith("itk") and not f.startswith("itkv3") and f.endswith(opts.base+".h")]) - excluded

# get filter from wrapper files
# remove classes which are not in the toolkit (external projects, PyImageFilter, ...)
wrapped = set([a for a in dir(itk) if a.endswith(opts.base)]).intersection(classes)

nonWrapped = classes - wrapped


# print non wrapped classes without much text to stdout, so they can be easily reused
for f in sorted(nonWrapped) :
    print f

# and print stats in stderr to avoid poluting the list above
print >>sys.stderr
print >>sys.stderr, '%i %s' % (len(classes), opts.base)
print >>sys.stderr, '%i wrapped %s' % (len(wrapped), opts.base)
print >>sys.stderr, '%i non wrapped %s' % (len(nonWrapped), opts.base)
print >>sys.stderr, '%f%% covered' % (len(wrapped) / float(len(classes)) * 100)
print >>sys.stderr

if not opts.noError:
    sys.exit(len(nonWrapped))
