#include "itkOrthogonallyCorrected2DParametricPath.h"
#include "itkImage.h"
#include "itkPolyLineParametricPath.h"
#include "itkOrthogonalSwath2DPathFilter.h"

int itkOrthogonalSwath2DPathFilterTest(int, char*[])
{
  typedef itk::PolyLineParametricPath<2>                    InputPathType;  
  typedef itk::Image<double, 2>                             InputImageType; 
  typedef itk::OrthogonallyCorrected2DParametricPath        OutputPathType; 

  typedef itk::OrthogonalSwath2DPathFilter
                        <InputPathType,InputImageType>     FilterType;
  
  // Setup the inputs
  InputPathType::Pointer   inPath  = InputPathType::New();
  InputImageType::Pointer  inImage = InputImageType::New();
  
  // Setup the filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetPathInput(inPath);
  filter->SetImageInput(inImage);
  
  // Setup the output
  OutputPathType::Pointer            outPath;
  outPath=filter->GetOutput();
  
  return EXIT_SUCCESS;
}


