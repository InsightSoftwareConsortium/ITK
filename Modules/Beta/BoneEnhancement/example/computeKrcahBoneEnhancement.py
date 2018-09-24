from __future__ import print_function
import itk
import sys
import os

# Parse inputs
if len(sys.argv) < 8:
  os.sys.exit('Usage: {} <InputFileName>'.format(sys.argv[0]) + 
    ' <OutputPreprocessed> <OutputMeasure>' +
    ' SetEnhanceBrightObjects[0,1]> <UseImplementationParameters[0,1]>'+
    ' <NumberOfSigma> <Sigma1> [<Sigma2> <Sigma3>]')

inputFileName = sys.argv[1]
outputPreprocessedFileName = sys.argv[2]
outputMeasureFileName = sys.argv[3]
enhanceBrightObjects = int(sys.argv[4])
parameterSetToImplement = int(sys.argv[5])
numberOfSigma = int(sys.argv[6])
sigmaArray = []
for i in range(numberOfSigma):
  sigmaArray.append(float(sys.argv[7+i]))

print('Read in the following parameters:')
print('  InputFileName:               {}'.format(inputFileName))
print('  OutputPreprocessed:          {}'.format(outputPreprocessedFileName))
print('  OutputMeasure:               {}'.format(outputMeasureFileName))
print('  SetEnhanceBrightObjects:     {}'.format('Enhancing bright objects' if enhanceBrightObjects == 1 else 'Enhancing dark objects'))
print('  UseImplementationParameters: {}'.format('Using implementation parameters' if parameterSetToImplement == 1 else 'Using journal article parameters'))
print('  NumberOfSigma:               {}'.format(numberOfSigma))
print('  SigmaArray:                  {}'.format(sigmaArray))
print('')

# Read input image
print('Reading in {}'.format(inputFileName))
inputImage = itk.imread(inputFileName)

# Preprocessing filter
preprocFilter = itk.KrcahEigenToScalarPreprocessingImageToImageFilter.New(inputImage)
preprocFilter.SetInput(inputImage)

def PreprocessingCommand():
  progress = int(100*preprocFilter.GetProgress())
  if progress > PreprocessingCommand.progress:
    PreprocessingCommand.progress = progress
    print('\rProgress: {}%'.format(progress), end='')
    sys.stdout.flush()
PreprocessingCommand.progress = -1

print('Running preprocessing...')
preprocFilter.AddObserver(itk.ProgressEvent(), PreprocessingCommand)
preprocFilter.Update()
print('')

# Write the result
print('Writing out {}'.format(outputPreprocessedFileName))
itk.imwrite(preprocFilter.GetOutput(), outputPreprocessedFileName)

# Create enhancmenet filters
Dimension = 3
EigenPixelType = itk.Vector[itk.F, Dimension]
EigenImageType = itk.Image[EigenPixelType, Dimension]
MaskType = itk.Image[itk.UC, Dimension]
FloatImageType = itk.Image[itk.F, Dimension]
eigenFilterType = itk.KrcahEigenToScalarImageFilter[EigenImageType, FloatImageType, MaskType]
eigenFilter = eigenFilterType.New()

multiscaleFilter = itk.MultiScaleHessianEnhancementImageFilter.New(preprocFilter.GetOutput())
multiscaleFilter.SetInput(preprocFilter.GetOutput())
multiscaleFilter.SetEigenToScalarImageFilter(eigenFilter)
multiscaleFilter.SetSigmaArray(sigmaArray)

# Run the filter
def MyCommand():
  progress = int(100*multiscaleFilter.GetProgress())
  if progress > MyCommand.progress:
    MyCommand.progress = progress
    print('\rProgress: {}%'.format(progress), end='')
    sys.stdout.flush()
MyCommand.progress = -1

print('Running multiScaleFilter...')
multiscaleFilter.AddObserver(itk.ProgressEvent(), MyCommand)
multiscaleFilter.Update()
print('')

# Write the result
print('Writing results to {}'.format(outputMeasureFileName))
itk.imwrite(multiscaleFilter.GetOutput(), outputMeasureFileName)
