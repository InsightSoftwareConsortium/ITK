#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <iostream>

int itkDiffusionTensor3DReconstructionImageFilterTest(int, char*[])
{
  typedef short int          ReferencePixelType;
  typedef short int          GradientPixelType;
  typedef double             TensorPrecisionType;

  typedef itk::DiffusionTensor3DReconstructionImageFilter< 
      ReferencePixelType, GradientPixelType, TensorPrecisionType > 
        TensorReconstructionImageFilterType;
  typedef TensorReconstructionImageFilterType::GradientImageType GradientImageType;
  TensorReconstructionImageFilterType::Pointer tensorReconstructionFilter = 
    TensorReconstructionImageFilterType::New();
  
  // Create a reference image
  //
  typedef TensorReconstructionImageFilterType::ReferenceImageType ReferenceImageType;
  ReferenceImageType::Pointer referenceImage = ReferenceImageType::New();
  typedef ReferenceImageType::RegionType ReferenceRegionType;
  typedef ReferenceRegionType::IndexType ReferenceIndexType;
  typedef ReferenceRegionType::SizeType  ReferenceSizeType;
  ReferenceSizeType  sizeReferenceImage  = {{ 4, 4, 4 }};
  ReferenceIndexType indexReferenceImage = {{ 0, 0, 0 }};
  ReferenceRegionType     regionReferenceImage;
  regionReferenceImage.SetSize(  sizeReferenceImage );
  regionReferenceImage.SetIndex( indexReferenceImage);
  referenceImage->SetRegions( regionReferenceImage );
  referenceImage->Allocate();
  referenceImage->FillBuffer( 100 );
  
  
  const unsigned int numberOfGradientImages = 6;

  // Assign gradient directions
  //
  double  gradientDirections[6][3] = 
    {
      {-1.000000,   0.000000  ,      0.000000},
      {-0.166000,   0.986000  ,      0.000000},
      {0.110000 ,   0.664000  ,      0.740000},
      {-0.901000,   -0.419000 ,      -0.110000},
      {0.169000 ,   -0.601000 ,      0.781000},
      {0.815000 ,   -0.386000 ,      0.433000}
    };
    
  
  // Create gradient images
  //
  typedef GradientImageType::Pointer GradientImagePointer;
  typedef TensorReconstructionImageFilterType::GradientImageType GradientImageType;
  typedef GradientImageType::RegionType  GradientRegionType;
  typedef GradientRegionType::IndexType  GradientIndexType;
  typedef GradientRegionType::SizeType   GradientSizeType;
  typedef ReferenceRegionType::IndexType ReferenceIndexType;
  
  for( unsigned int i=0; i < numberOfGradientImages; i++ )
    { 
    typedef TensorReconstructionImageFilterType::GradientImageType GradientImageType;
    GradientImageType::Pointer gradientImage = GradientImageType::New();
    GradientSizeType  sizeGradientImage  = {{ 4, 4, 4 }};
    GradientIndexType indexGradientImage = {{ 0, 0, 0 }};
    GradientRegionType     regionGradientImage;
    regionGradientImage.SetSize(  sizeGradientImage );
    regionGradientImage.SetIndex( indexGradientImage);
    gradientImage->SetRegions( regionGradientImage );
    gradientImage->Allocate();
  
    itk::ImageRegionIteratorWithIndex< GradientImageType > git( 
        gradientImage, regionGradientImage );
    git.GoToBegin();
    while( !git.IsAtEnd() )
      {
      GradientPixelType fancyGradientValue = 
        static_cast< short int >((i+1) * (i+1) * (i+1));
      git.Set( fancyGradientValue );
      ++git;
      }
    
    TensorReconstructionImageFilterType::GradientDirectionType gradientDirection;
    gradientDirection[0] = gradientDirections[i][0];
    gradientDirection[1] = gradientDirections[i][1];
    gradientDirection[2] = gradientDirections[i][2];
    tensorReconstructionFilter->AddGradientImage( gradientDirection, gradientImage );   
    std::cout << "Gradient directions: " << gradientDirection << std::endl;
    }

  tensorReconstructionFilter->SetReferenceImage( referenceImage );
  // TODO: remove this when netlib is made thread safe
  tensorReconstructionFilter->SetNumberOfThreads( 1 ); 

  // Also see if vnl_svd is thread safe now...
  std::cout << std::endl << "This filter is using " << 
   tensorReconstructionFilter->GetNumberOfThreads() << " threads " << std::endl;
  
  tensorReconstructionFilter->Update();

  typedef TensorReconstructionImageFilterType::TensorImageType TensorImageType;
  TensorImageType::Pointer tensorImage = tensorReconstructionFilter->GetOutput();
  typedef TensorImageType::IndexType TensorImageIndexType;
  
  TensorImageIndexType tensorImageIndex    = {{3,3,3}};
  GradientIndexType    gradientImageIndex  = {{3,3,3}};
  ReferenceIndexType   referenceImageIndex = {{3,3,3}};

  std::cout << std::endl << "Pixels at index: " << tensorImageIndex << std::endl;
  std::cout << "Reference pixel " 
    << referenceImage->GetPixel( referenceImageIndex ) << std::endl;
  
  for( unsigned int i=0; i < numberOfGradientImages; i++ )
    { 
    std::cout << "Gradient image " << i << " pixel : " 
      << tensorReconstructionFilter->GetGradientImage(i)->GetPixel(gradientImageIndex)
      << std::endl;
    }

  double  expectedResult[3][3] = 
    {
      {4.60517, -2.66978, -8.40705},
      {-2.66978, 1.56848, 0.899627},
      {-8.40705, 0.899627, 2.62356}
    };
  
  std::cout << std::endl << "Reconstructed tensor : " << std::endl; 
  bool passed = true;
  double precision = 0.0001;
  for( unsigned int i = 0; i<3; i++ )
    {
    std::cout << "\t";
    for( unsigned int j = 0; j<3; j++ )
      {
      std::cout << tensorImage->GetPixel(tensorImageIndex)(i,j) << " ";
      if( (tensorImage->GetPixel(tensorImageIndex)(i,j) - expectedResult[i][j]) > precision )
        {
        passed = false;
        }
      }
    std::cout << std::endl;
    }

  if( !passed ) 
    {
    std::cout << "[FAILED]" << std::endl;
    
    std::cout << "Expected tensor : " << std::endl;
    for( unsigned int i = 0; i<3; i++ )
      {
      std::cout << "\t";
      for( unsigned int j = 0; j<3; j++ )
        {
        std::cout << expectedResult[i][j] << " ";
        }
      std::cout << std::endl;
      }
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
      
  return EXIT_SUCCESS;
}

     
    
    
    
