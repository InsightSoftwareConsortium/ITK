import itk
import sys
import os

# Parse inputs
if len(sys.argv) < 6:
    os.sys.exit(
        f"Usage: {sys.argv[0]} <InputFileName> <OutputMeasure>"
        + " SetEnhanceBrightObjects[0,1]> <NumberOfSigma> <Sigma1> [<Sigma2> <Sigma3>]"
    )

inputFileName = sys.argv[1]
outputMeasureFileName = sys.argv[2]
enhanceBrightObjects = int(sys.argv[3])
numberOfSigma = int(sys.argv[4])
sigmaArray = []
for i in range(numberOfSigma):
    sigmaArray.append(float(sys.argv[5 + i]))

print("Read in the following parameters:")
print(f"  InputFileName:           {inputFileName}")
print(f"  OutputMeasure:           {outputMeasureFileName}")
print(
    "  SetEnhanceBrightObjects: {}".format(
        "Enhancing bright objects"
        if enhanceBrightObjects == 1
        else "Enhancing dark objects"
    )
)
print(f"  NumberOfSigma:           {numberOfSigma}")
print(f"  SigmaArray:              {sigmaArray}")
print("")

# Read input image
print(f"Reading in {inputFileName}")
inputImage = itk.imread(inputFileName)

# Create enhancmenet filters
Dimension = 3
EigenPixelType = itk.Vector[itk.F, Dimension]
EigenImageType = itk.Image[EigenPixelType, Dimension]
MaskType = itk.Image[itk.UC, Dimension]
FloatImageType = itk.Image[itk.F, Dimension]
eigenFilterType = itk.DescoteauxEigenToScalarImageFilter[
    EigenImageType, FloatImageType, MaskType
]
eigenFilter = eigenFilterType.New()

multiscaleFilter = itk.MultiScaleHessianEnhancementImageFilter.New(inputImage)
multiscaleFilter.SetInput(inputImage)
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
