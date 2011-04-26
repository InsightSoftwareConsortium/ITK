#include "itkImage.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkBinaryBallStructuringElement.h"

typedef itk::Image<int, 2>   ImageType;
typedef ImageType::PixelType PixelType;

void CreateImagex(ImageType::Pointer image);

int itkShapedIteratorFromStructuringElementTest(int, char*[])
{
  ImageType::Pointer image = ImageType::New();
  CreateImagex(image);

  typedef itk::BinaryBallStructuringElement<PixelType, 2>
    StructuringElementType;
  StructuringElementType::RadiusType elementRadius;
  elementRadius.Fill(2);

  StructuringElementType structuringElement;
    structuringElement.SetRadius(elementRadius);
    structuringElement.CreateStructuringElement();

  typedef itk::ShapedNeighborhoodIterator<ImageType> IteratorType;
  IteratorType siterator(structuringElement.GetRadius(),
                         image,
                         image->GetLargestPossibleRegion());

  siterator.CreateActiveListFromNeighborhood(structuringElement);
  siterator.NeedToUseBoundaryConditionOff();

  IteratorType::IndexType location;
  location[0] = 4;
  location[1] = 5;
  siterator.SetLocation(location);
  IteratorType::Iterator i;
  for (i = siterator.Begin(); !i.IsAtEnd(); ++i)
    {
    i.Set(1);
    }

  // Now show the results
  typedef itk::ImageRegionConstIterator<ImageType> ImageIteratorType;
  ImageIteratorType imit(image, image->GetLargestPossibleRegion());
  imit.GoToBegin();
  unsigned int col = 0;
  while( !imit.IsAtEnd() )
    {
    PixelType value = imit.Get();
    ++imit;
    ++col;
    std::cout << value << " ";
    if ((col % 10) == 0)
      {
      std::cout << std::endl;
      }
    }
  // Check for radius mismatch between shaped iterator and
  // neighborhood
  IteratorType biterator(structuringElement.GetRadius(),
                         image,
                         image->GetLargestPossibleRegion());
  elementRadius.Fill(3);
  structuringElement.SetRadius(elementRadius);

  bool caught = false;
  try
    {
    biterator.CreateActiveListFromNeighborhood(structuringElement);
    }
  catch (itk::ExceptionObject& e)
    {
    caught = true;
    std::cout << "Caught expected exception." << e << std::endl;
    }
  if (!caught)
    {
    std::cout << "Faile to catch expected exception." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

void CreateImagex(ImageType::Pointer image)
{
  ImageType::IndexType start;
  start.Fill(0);

  ImageType::SizeType size;
  size.Fill(10);

  ImageType::RegionType region(start,size);

  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(0);
}
