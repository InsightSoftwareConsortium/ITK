#!/usr/bin/env python

import itk

# create some vars used later

idx = itk.Index[2]()
idx.Fill(12)

size = itk.Size[2]()
size.Fill(12)

t = (12, 12)

l = [12, 12]

i = 12

# ok, now lets try all those equivalent vars on some methods
idx1 = itk.Index[2](idx)
idx2 = itk.Index[2](t)
idx3 = itk.Index[2](l)
idx4 = itk.Index[2](i)
assert idx1.GetElement(0) == idx2.GetElement(0) == idx3.GetElement(0) == idx4.GetElement(0) == i
assert idx1.GetElement(1) == idx2.GetElement(1) == idx3.GetElement(1) == idx4.GetElement(1) == i

size1 = itk.Size[2](size)
size2 = itk.Size[2](t)
size3 = itk.Size[2](l)
size4 = itk.Size[2](i)
assert size1.GetElement(0) == size2.GetElement(0) == size3.GetElement(0) == size4.GetElement(0) == i
assert size1.GetElement(1) == size2.GetElement(1) == size3.GetElement(1) == size4.GetElement(1) == i

median = itk.MedianImageFilter.IUS2IUS2.New()
median.SetRadius(size)
size1 = median.GetRadius()
median.SetRadius(t)
size2 = median.GetRadius()
median.SetRadius(l)
size3 = median.GetRadius()
median.SetRadius(i)
size4 = median.GetRadius()
assert size1.GetElement(0) == size2.GetElement(0) == size3.GetElement(0) == size4.GetElement(0) == i
assert size1.GetElement(1) == size2.GetElement(1) == size3.GetElement(1) == size4.GetElement(1) == i


# smart pointers
im = itk.Image.US2.New()
assert im.GetPointer() != None
assert im.GetPointer().__class__ != im.__class__

median.SetInput( im )
assert repr(median.GetInput().GetPointer()) == repr(im.GetPointer())

median.SetInput( im.GetPointer() )
assert repr(median.GetInput().GetPointer()) == repr(im.GetPointer())

median.SetInput( None )
assert repr(median.GetInput().GetPointer()) == repr(None)





