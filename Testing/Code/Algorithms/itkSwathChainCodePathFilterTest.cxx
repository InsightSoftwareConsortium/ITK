#include "itkChainCodePath2D.h"
#include "itkImage.h"
#include "itkSwathChainCodePathFilter.h"

int itkSwathChainCodePathFilterTest(int, char*[])
{
  typedef itk::ChainCodePath2D                                PathType;  
  typedef itk::Image<double, 2>                               ImageType; 
  typedef itk::SwathChainCodePathFilter <PathType,ImageType>  FilterType;
  
  // Setup the inputs
  PathType::Pointer   inPath  = PathType::New();
  ImageType::Pointer  inImage = ImageType::New();
  
  // Setup the filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetPathInput(inPath);
  filter->SetImageInput(inImage);
  
  // Setup the output
  PathType::Pointer outPath = filter->GetOutput();
  
  return EXIT_SUCCESS;
}


