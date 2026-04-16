import itk
import numpy as np

# Smoke test: create a simple ITK image via Python
ImageType = itk.Image[itk.F, 3]
image = ImageType.New()
region = itk.ImageRegion[3]()
region.SetSize([10, 10, 10])
image.SetRegions(region)
image.Allocate()
print(f"ITK Python wrapping OK — version: {itk.Version.GetITKVersion()}")
