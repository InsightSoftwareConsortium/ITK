#include "itkArray.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkFixedArray.h"
#include "itkTimeProbe.h"
#include "itkVectorImageToImageAdaptor.h"

int itkVectorImageTest( int, char* [] )
{
  
  const unsigned int VectorLength = 6;
  const unsigned int Dimension    = 3;
  typedef float PixelType;

  // Three images.. for crude timing analysis.
  typedef itk::Image< itk::Array< PixelType >, Dimension > ArrayImageType;
  typedef itk::Image< itk::FixedArray< PixelType, VectorLength >, 
                                      Dimension > FixedArrayImageType;
  typedef itk::VectorImage< PixelType, Dimension >   VectorImageType;
  
  
  
  // Using image of Array< PixelType >
  {
  typedef itk::Array< PixelType > InternalPixelType;

  itk::TimeProbe clock;
  clock.Start();
  
  ArrayImageType::Pointer image = ArrayImageType::New();
  ArrayImageType::IndexType start;
  InternalPixelType f( VectorLength );
  for( unsigned int i=0; i<VectorLength; i++ ) { f[i] = i; }
  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z
  ArrayImageType::SizeType  size;
  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z
  ArrayImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( f );

  clock.Stop();
  double timeTaken = clock.GetMeanTime();
  std::cout << "Allocating an image of itk::Array of length " <<  VectorLength 
          << " with image size " << size << " took " << timeTaken << " s." << std::endl;
  }

  
  // Using image of FixedArray< PixelType, VectorLength >
  {
  itk::TimeProbe clock;
  clock.Start();
  
  typedef itk::FixedArray< PixelType, VectorLength > InternalPixelType;
  
  FixedArrayImageType::Pointer image = FixedArrayImageType::New();
  FixedArrayImageType::IndexType start;
  InternalPixelType f;
  for( unsigned int i=0; i<VectorLength; i++ ) { f[i] = i; }
  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z
  FixedArrayImageType::SizeType  size;
  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z
  FixedArrayImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( f );

  clock.Stop();
  double timeTaken = clock.GetMeanTime();
  std::cout << "Allocating an image of itk::Array of length " <<  VectorLength 
          << " with image size " << size << " took " << timeTaken << " s." << std::endl;
  }

  // Using VectorImage< PixelType, Dimension > 
  {
  itk::TimeProbe clock;
  clock.Start();
  
  VectorImageType::Pointer vectorImage = VectorImageType::New();
  VectorImageType::IndexType start;
  itk::Array< PixelType > f( VectorLength );
  for( unsigned int i=0; i<VectorLength; i++ ) { f[i] = i; }
  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z
  VectorImageType::SizeType  size;
  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z
  VectorImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
  vectorImage->SetVectorLength( VectorLength );
  vectorImage->SetRegions( region );
  vectorImage->Allocate();
  vectorImage->FillBuffer( f );

  clock.Stop();
  double timeTaken = clock.GetMeanTime();
  std::cout << "Allocating an image of itk::Array of length " <<  VectorLength 
     << " with image size " << size << " took " << timeTaken << " s." << std::endl;


  
  std::cout << "---------------------------------------------------------------" << std::endl;
  std::cout << "Testing VectorImageToImageAdaptor to extract a component from the vector image" << std::endl;


  const unsigned int componentToExtract = 4;
  typedef itk::VectorImageToImageAdaptor< PixelType, Dimension > AdaptorType;
  AdaptorType::Pointer vectorImageToImageAdaptor = AdaptorType::New();
  vectorImageToImageAdaptor->SetExtractComponentIdx( componentToExtract );
  vectorImageToImageAdaptor->SetImage( vectorImage );
  vectorImageToImageAdaptor->Update();
 
  typedef itk::Image< PixelType , Dimension > AdaptedImageType;
  AdaptedImageType::IndexType index;
  index[0]=10;
  index[1]=10;
  index[2]=10;

  if(   (vectorImageToImageAdaptor->GetPixel(index) !=  vectorImage->GetPixel( index )[componentToExtract]) 
     || (vectorImage->GetPixel( index )[componentToExtract] != componentToExtract ))
    {
    std::cout << "[FAILED]" << std::endl;
    }
  else 
    {
    std::cout << "[PASSED]" << std::endl;
    }
    
    
    
  
  
  }
  


  
  return EXIT_SUCCESS;
 }

  
