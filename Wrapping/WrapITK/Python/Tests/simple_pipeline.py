#!/usr/bin/env python

import itk, sys
itk.auto_progress(2)

# first argument is the pixel type
PType = getattr(itk, sys.argv[1])
# second arguement the image dimension
dim = int(sys.argv[2])

# get the image type
IType = itk.Image[PType, dim]

# create the reader
reader = itk.ImageFileReader[IType].New(FileName=sys.argv[3])
# and the writer
writer = itk.ImageFileWriter[IType].New(reader, FileName=sys.argv[4])

# execute the filters in the pipeline !
writer.Update()
