#include "itkPolyLineParametricPath.h"
#include "itkImage.h"
#include "itkExtractOrthogonalSwath2DImageFilter.h"

int itkExtractOrthogonalSwath2DImageFilterTest(int, char*[])
{
  typedef itk::Image<double, 2>                             ImageType; 
  typedef itk::PolyLineParametricPath<2>                    PathType;  

  typedef itk::ExtractOrthogonalSwath2DImageFilter<ImageType> FilterType;
  
  // Setup the inputs
  ImageType::Pointer  inImage = ImageType::New();
  PathType::Pointer   inPath  = PathType::New();
  
  // Setup the filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetImageInput(inImage);
  filter->SetPathInput(inPath);
  
  // Setup the output
  ImageType::Pointer outImage;
  outImage=filter->GetOutput();
  
  // Test only compilation right now...
  return EXIT_SUCCESS;
}


