import itk
import sys
import os

# Parse inputs
if len(sys.argv) < 8:
    os.sys.exit(
        f"Usage: {sys.argv[0]} <InputFileName>"
        + " <OutputPreprocessed> <OutputMeasure>"
        + " SetEnhanceBrightObjects[0,1]> <UseImplementationParameters[0,1]>"
        + " <NumberOfSigma> <Sigma1> [<Sigma2> <Sigma3>]"
    )

inputFileName = sys.argv[1]
outputPreprocessedFileName = sys.argv[2]
outputMeasureFileName = sys.argv[3]
enhanceBrightObjects = int(sys.argv[4])
parameterSetToImplement = int(sys.argv[5])
numberOfSigma = int(sys.argv[6])
sigmaArray = []
for i in range(numberOfSigma):
    sigmaArray.append(float(sys.argv[7 + i]))

print("Read in the following parameters:")
print(f"  InputFileName:               {inputFileName}")
print(f"  OutputPreprocessed:          {outputPreprocessedFileName}")
print(f"  OutputMeasure:               {outputMeasureFileName}")
print(
    "  SetEnhanceBrightObjects:     {}".format(
        "Enhancing bright objects"
        if enhanceBrightObjects == 1
        else "Enhancing dark objects"
    )
)
print(
    "  UseImplementationParameters: {}".format(
        "Using implementation parameters"
        if parameterSetToImplement == 1
        else "Using journal article parameters"
    )
)
print(f"  NumberOfSigma:               {numberOfSigma}")
print(f"  SigmaArray:                  {sigmaArray}")
print("")

# Read input image
print(f"Reading in {inputFileName}")
inputImage = itk.imread(inputFileName)

# Preprocessing filter
preprocFilter = itk.KrcahEigenToScalarPreprocessingImageToImageFilter.New(inputImage)
preprocFilter.SetInput(inputImage)


def PreprocessingCommand():
    progress = int(100 * preprocFilter.GetProgress())
    if progress > PreprocessingCommand.progress:
        PreprocessingCommand.progress = progress
        print(f"\rProgress: {progress}%", end="")
        sys.stdout.flush()


PreprocessingCommand.progress = -1

print("Running preprocessing...")
preprocFilter.AddObserver(itk.ProgressEvent(), PreprocessingCommand)
preprocFilter.Update()
print("")

# Write the result
print(f"Writing out {outputPreprocessedFileName}")
itk.imwrite(preprocFilter.GetOutput(), outputPreprocessedFileName)

# Create enhancmenet filters
Dimension = 3
EigenPixelType = itk.Vector[itk.F, Dimension]
EigenImageType = itk.Image[EigenPixelType, Dimension]
MaskType = itk.Image[itk.UC, Dimension]
FloatImageType = itk.Image[itk.F, Dimension]
eigenFilterType = itk.KrcahEigenToScalarImageFilter[
    EigenImageType, FloatImageType, MaskType
]
eigenFilter = eigenFilterType.New()

multiscaleFilter = itk.MultiScaleHessianEnhancementImageFilter.New(
    preprocFilter.GetOutput()
)
multiscaleFilter.SetInput(preprocFilter.GetOutput())
multiscaleFilter.SetEigenToScalarImageFilter(eigenFilter)
multiscaleFilter.SetSigmaArray(sigmaArray)


# Run the filter
def MyCommand():
    progress = int(100 * multiscaleFilter.GetProgress())
    if progress > MyCommand.progress:
        MyCommand.progress = progress
        print(f"\rProgress: {progress}%", end="")
        sys.stdout.flush()


MyCommand.progress = -1

print("Running multiScaleFilter...")
multiscaleFilter.AddObserver(itk.ProgressEvent(), MyCommand)
multiscaleFilter.Update()
print("")

# Write the result
print(f"Writing results to {outputMeasureFileName}")
itk.imwrite(multiscaleFilter.GetOutput(), outputMeasureFileName)
