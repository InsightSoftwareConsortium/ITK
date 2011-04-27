import itk
from sys import argv

# instantiate an auto pipeline
p = itk.auto_pipeline()

# from now, and until we call p.Stop(), all the new objects will be connected to
# the pipeline p, without having to give a name to any filter

itk.ImageFileReader.IUC2.New( FileName=argv[1] )
itk.MedianImageFilter.IUC2IUC2.New( Radius=eval( argv[3] ) )
itk.CastImageFilter.IUC2IUC2.New()

# stop the auto_pipeline and test that the next (imcompatible) filter is not
# automatically connected, and restart the auto pipeline
p.Stop()
itk.CastImageFilter.IF2IF2.New()
p.Start()

itk.ImageFileWriter.IUC2.New( FileName=argv[2] )
p.Update()
