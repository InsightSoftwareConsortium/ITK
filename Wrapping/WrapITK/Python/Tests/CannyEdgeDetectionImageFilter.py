#
#  Example on the use of the CannyEdgeDetectionImageFilter
#

import itk
from sys import argv, stderr, exit
itk.auto_progress(2)

if len(argv) < 3:
  print >> stderr, "Usage: CannyEdgeDetectionImageFilter.py  inputImage outputImage [variance]"
  exit(1)
  
variance = 2.0
if len(argv) > 3:
  variance = float( argv[3] )
  print variance

reader = itk.ImageFileReader.IF2.New( FileName=argv[1] )
filter  = itk.CannyEdgeDetectionImageFilter.IF2IF2.New( reader, Variance=variance )
outputCast = itk.RescaleIntensityImageFilter.IF2IUC2.New( filter, OutputMinimum=0, OutputMaximum=255 )
itk.write( outputCast, argv[2] )


