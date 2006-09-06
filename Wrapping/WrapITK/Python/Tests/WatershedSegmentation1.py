
import itk
import sys
itk.auto_progress(2)

reader = itk.ImageFileReader.IF2.New()
reader.SetFileName( sys.argv[1] )

diffusion = itk.GradientAnisotropicDiffusionImageFilter.IF2IF2.New()
diffusion.SetInput(reader.GetOutput())
diffusion.SetTimeStep(0.0625)
diffusion.SetConductanceParameter(9.0)
diffusion.SetNumberOfIterations( 5 );

gradient = itk.GradientMagnitudeImageFilter.IF2IF2.New()
gradient.SetInput(diffusion.GetOutput())

watershed = itk.WatershedImageFilter.IF2.New()
watershed.SetInput(gradient.GetOutput())
watershed.SetThreshold(0.01)
watershed.SetLevel(0.2)

relabel = itk.RelabelComponentImageFilter.IUL2IUS2.New()
relabel.SetInput( watershed.GetOutput() )

cast = itk.CastImageFilter.IUS2IUC2.New()
cast.SetInput( relabel.GetOutput() )

writer = itk.ImageFileWriter.IUC2.New()
writer.SetFileName( sys.argv[2] )
writer.SetInput( cast.GetOutput()  )
writer.Update()



