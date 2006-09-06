
import itk
from sys import argv
itk.auto_progress(2)

InputPixelType = itk.F
OutputPixelType = itk.F

InputImageType = itk.Image[InputPixelType, 2]
OutputImageType = itk.Image[OutputPixelType, 2]

reader = itk.ImageFileReader[InputImageType].New( FileName=argv[1] )

filter = itk.GradientMagnitudeRecursiveGaussianImageFilter[InputImageType, OutputImageType].New( reader, Sigma=float(argv[3]) )
filter.Update();

WritePixelType = itk.UC
WriteImageType = itk.Image[WritePixelType, 2]

rescaler = itk.RescaleIntensityImageFilter[OutputImageType, WriteImageType].New( filter, OutputMinimum=0, OutputMaximum=255 )
writer = itk.ImageFileWriter[WriteImageType].New( rescaler, FileName=argv[2] )
writer.Update();
