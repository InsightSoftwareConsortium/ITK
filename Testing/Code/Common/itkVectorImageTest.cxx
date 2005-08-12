#include "itkArray.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkFixedArray.h"
#include "itkTimeProbe.h"
#include "itkVectorImageToImageAdaptor.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"

int itkVectorImageTest( int, char* [] )
{

  // Test 1.
  //
  // Create an Image of Array, FixedArray, VectorImage of length 6 and compare
  // times.
  //   
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

    // Const iterator over the image...
    {
    clock.Start();
    typedef itk::ImageRegionConstIterator< ArrayImageType > IteratorType;
    IteratorType it( image, image->GetBufferedRegion() );
    it.Begin();
    while( !it.IsAtEnd() )
      {
      it.Get();
      ++it;
      }
    clock.Stop();
    std::cout << "ConstIterator Get() over the entire image took : " << 
      clock.GetMeanTime() << " s." << std::endl;
    }
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
  std::cout << "Allocating an image of itk::FixedArray of length " <<  VectorLength 
          << " with image size " << size << " took " << timeTaken << " s." << std::endl;
  
    
    {
    // Test and compare times with iterators
    //
    // First set some pixel
    for( unsigned int i=0; i<VectorLength; i++ ) { f[i] = i*0.1; }
    FixedArrayImageType::IndexType idx;
    idx[0]=4; idx[1]=4;idx[2]=12; 
    image->SetPixel( idx, f );
    }

    {
    clock.Start();
    typedef itk::ImageRegionConstIterator< FixedArrayImageType > IteratorType;
    IteratorType it( image, image->GetBufferedRegion() );
    it.Begin();
    while( !it.IsAtEnd() )
      {
      it.Get();
      ++it;
      }
    clock.Stop();
    std::cout << "ConstIterator Get() over the entire image took : " << 
      clock.GetMeanTime() << " s." << std::endl;
    }
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
  std::cout << "Allocating an image of itk::VectorImage with pixels length " <<  VectorLength 
     << " with image size " << size << " took " << timeTaken << " s." << std::endl;


  // Iterator tests on the vector image.
  //
  // Const iterator over the vector image...
  {
    clock.Start();
    typedef itk::ImageRegionConstIterator< VectorImageType > IteratorType;
    IteratorType it( vectorImage, vectorImage->GetBufferedRegion() );
    it.Begin();
    while( !it.IsAtEnd() )
      {
      it.Get();
      ++it;
      }
    clock.Stop();
    std::cout << "ConstIterator Get() over the entire vectorImage took : " << 
      clock.GetMeanTime() << " s." << std::endl;
  }


  
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
    
  

    // Iterate, set some pixels and check validity
    //
    VectorImageType::IndexType idx;
    idx[0]=4; idx[1]=4;idx[2]=12; 
    VectorImageType::IndexType idx2;
    idx2[0]=5; idx2[1]=4;idx2[2]=12; 
    itk::Array< PixelType > g( VectorLength );
    for( unsigned int i=0; i<VectorLength; i++ ) { g[i] = i*0.1; }
    
    {
    typedef itk::ImageRegionIterator< VectorImageType > IteratorType;
    IteratorType it( vectorImage, vectorImage->GetBufferedRegion() );
    
    vectorImage->SetPixel( idx, g );
    
    typedef itk::ImageRegionConstIterator< VectorImageType > ConstIteratorType;
    ConstIteratorType cit( vectorImage, vectorImage->GetBufferedRegion() );
    unsigned long ctr = 0;
    cit.Begin();
    while( !cit.IsAtEnd() )
      {
      itk::Array< PixelType > value = cit.Get();
      ++cit;
      if( ctr == (12*200*200 + 4*200 + 4) )
        {
        if( value != g )
          { 
          std::cerr << 
            "ImageRegionConstIteratorTest on VectorImage [FAILED]" << std::endl;
          return EXIT_FAILURE;
          }
        }
      else
        {
        if( value != f )
          {
          std::cerr << 
            "ImageRegionConstIteratorTest on VectorImage [FAILED]" << std::endl;
          return EXIT_FAILURE;
          }
        }
      ++ctr;
      }
    std::cout << "ImageRegionConstIteratorTest on VectorImage [PASSED]" << std::endl;
    } 

    
    {
    // Test itkImageLinearIteratorWithIndex
    typedef itk::ImageLinearConstIteratorWithIndex< VectorImageType > LinearConstIteratorType;
    typedef itk::ImageLinearIteratorWithIndex< VectorImageType > LinearIteratorType;
    
    LinearConstIteratorType lcit( vectorImage, vectorImage->GetBufferedRegion() );
    lcit.SetDirection( 2 );
    lcit.GoToBegin();
    itk::Array< PixelType > value;
    while( !lcit.IsAtEnd() )
      {
      while( !lcit.IsAtEndOfLine() )
        {
        value = lcit.Get();  
        ++lcit;
        }
      lcit.NextLine();
      } 

    lcit.SetIndex( idx );
    value = lcit.Get();
    if( value != g )
      {
      std::cerr << 
        "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    
    g[0] = 12.3;
    LinearIteratorType lit( vectorImage, vectorImage->GetBufferedRegion() );
    lit.SetIndex( idx );
    lit.Set( g );
    
    lcit.SetIndex( idx );
    value = lcit.Get();
    if( value != g )
      {
      std::cerr << 
        "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    
    lcit.SetIndex( idx2 );
    itk::Array< PixelType > value2 = lcit.Get();
    if( value2 != f )
      {
      std::cerr << 
        "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
     
    std::cout << "ImageLinearConstIteratorWithIndex on VectorImage [PASSED]" << std::endl;
    }
    

    
  }
   


  
  return EXIT_SUCCESS;
 }

  
