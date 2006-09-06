#!/usr/bin/env python

# also test the import callback feature
def custom_callback(name, progress):
  if progress == 0:
    print >> sys.stderr, "Loading %s..." % name,
  if progress == 1:
    print >> sys.stderr, "done"
import itkConfig
itkConfig.ImportCallback = custom_callback

import itk, sys

# test the force load function
itk.force_load()

fileName = sys.argv[1]

PType = itk.US
dim = 2
IType = itk.Image[PType, dim]
ReaderType = itk.ImageFileReader[IType]
reader = ReaderType.New(FileName=fileName)


# test echo
itk.echo(reader)
itk.echo(reader, sys.stdout)

# test class_
assert itk.class_(reader) == ReaderType
assert itk.class_(reader.GetPointer()) == ReaderType
assert itk.class_("dummy") == str

# test template
assert itk.template(ReaderType) == (itk.ImageFileReader, (IType,))
assert itk.template(reader) == (itk.ImageFileReader, (IType,))
assert itk.template(reader.GetPointer()) == (itk.ImageFileReader, (IType,))
try:
  itk.template(str)
  raise Exception("unknown class should send an exception")
except KeyError:
  pass

# test ctype
assert itk.ctype("unsigned short") == itk.US
assert itk.ctype("        unsigned      \n   short \t  ") == itk.US
try:
  itk.ctype("dummy")
  raise Exception("unknown C type should send an exception")
except KeyError:
  pass


# test image
assert repr(itk.image(reader)) == repr(reader.GetOutput().GetPointer())
assert repr(itk.image(reader.GetOutput())) == repr(reader.GetOutput().GetPointer())
assert repr(itk.image(reader.GetOutput().GetPointer())) == repr(reader.GetOutput().GetPointer())
assert itk.image(1) == 1


# test strel
# should work with the image type, an image instance or a filter
# and should work with a list, a tuple, an int or an itk.Size
for s in [2, (2, 2), [2, 2], itk.Size[2](2)] :
  st = itk.strel(dim, s)
  
  (tpl, param) = itk.template(st)
  assert tpl == itk.FlatStructuringElement
  assert param[0] == dim
  assert st.GetRadius().GetElement(0) == st.GetRadius().GetElement(1) == 2

# test size
s = itk.size(reader)
assert s.GetElement(0) == s.GetElement(1) == 256
s = itk.size(reader.GetOutput())
assert s.GetElement(0) == s.GetElement(1) == 256
s = itk.size(reader.GetOutput().GetPointer())
assert s.GetElement(0) == s.GetElement(1) == 256


# test range
assert itk.range(reader) == (0, 255)
assert itk.range(reader.GetOutput()) == (0, 255)
assert itk.range(reader.GetOutput().GetPointer()) == (0, 255)


# test write
itk.write(reader, sys.argv[2])

