
import InsightToolkit as itk
import sys

reader = itk.itkImageFileReaderF2_New()
reader.SetFileName( sys.argv[1] )

diffusion = itk.itkGradientAnisotropicDiffusionImageFilterF2F2_New()
diffusion.SetInput(reader.GetOutput())
diffusion.SetTimeStep(0.0625)
diffusion.SetConductanceParameter(9.0)
diffusion.SetNumberOfIterations(  5 );

gradient = itk.itkGradientMagnitudeImageFilterF2F2_New()
gradient.SetInput(diffusion.GetOutput())

watershed = itk.itkWatershedImageFilterF2_New()
watershed.SetInput(gradient.GetOutput())
watershed.SetThreshold(0.01)
watershed.SetLevel(0.2)

writer = itk.itkImageFileWriterUL2_New()
writer.SetFileName( sys.argv[2] )
writer.SetInput( watershed.GetOutput()  )
writer.Update()



