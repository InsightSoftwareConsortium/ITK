import itk

Dimension = 2
PixelType = itk.UC

ImageType = itk.Image[PixelType, Dimension]

image_size = [10, 10]

image = ImageType.New()
image.SetRegions(image_size)
image.Allocate()

duplicate_image = itk.image_duplicator(image)

print(duplicate_image)

def itkSizeToList(size):
  l = []
  for i in range(size.GetSizeDimension()):
    l.append(size[i])
  return l

assert itkSizeToList(itk.size(duplicate_image)) == image_size
