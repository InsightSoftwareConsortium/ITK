from InsightToolkit import *

reader = itkImageFileReaderF2_New()
canny  = itkCannyEdgeDetectionImageFilterF2F2_New()
rescaler = itkRescaleIntensityImageFilterF2US2_New()
writer = itkImageFileWriterUS2_New()
canny.SetInput(reader.GetOutput())
rescaler.SetInput(canny.GetOutput())
writer.SetInput(rescaler.GetOutput())

rescaler.SetOutputMinimum(0)
rescaler.SetOutputMaximum(65535)

reader.SetFileName("c:/Hoffman/InsightNew/Testing/Data/Input/cthead1.png")
writer.SetFileName("./testout.png")
writer.Update()
