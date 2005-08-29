#include <iostream>
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
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"


// This test tests:
// VectorImage, a few iterators on the image, 
// Timing tests comparing it to similar images using FixedArray and Array
// IO support.

int itkVectorImageTest( int, char* argv[] )
{
  bool failed = false;

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
  vectorImageToImageAdaptor->SetExtractComponentIndex( componentToExtract );
  if( vectorImageToImageAdaptor->GetExtractComponentIndex() != componentToExtract )
    {
    std::cerr << "[FAILED]" << std::endl;
    
    } 
    
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
    std::cerr << "[FAILED]" << std::endl;
    failed = true;
    }
  else 
    {
    std::cout << "[PASSED]" << std::endl;
    }
  }

  // Test with Region and Linear iterators...
  {
    // Create a  small image
    VectorImageType::Pointer vectorImage = VectorImageType::New();
    VectorImageType::IndexType start;
    itk::Array< PixelType > f( VectorLength );
    itk::Array< PixelType > ZeroPixel( VectorLength );
    ZeroPixel.Fill( itk::NumericTraits< PixelType >::Zero );
    for( unsigned int i=0; i<VectorLength; i++ ) { f[i] = i; }
    start[0] =   0;  // first index on X
    start[1] =   0;  // first index on Y
    start[2] =   0;  // first index on Z
    VectorImageType::SizeType  size;
    size[0]  = 10;  // size along X
    size[1]  = 10;  // size along Y
    size[2]  = 5;  // size along Z
    VectorImageType::RegionType region( start, size );
    vectorImage->SetVectorLength( VectorLength );
    vectorImage->SetRegions( region );
    vectorImage->Allocate();
    vectorImage->FillBuffer( ZeroPixel );

    start[0]=3; start[1]=3;start[2]=2;
    size[0]=4;size[1]=4;size[2]=2;
    VectorImageType::RegionType subRegion( start, size );
    typedef itk::ImageRegionIterator< VectorImageType > ImageRegionIteratorType;
    ImageRegionIteratorType rit( vectorImage, subRegion );
    rit.GoToBegin();

    while( !rit.IsAtEnd() )
      {
      rit.Set( f );
      ++rit;
      }
    
    typedef itk::ImageRegionConstIterator< VectorImageType > ConstIteratorType;
    ConstIteratorType cit( vectorImage, vectorImage->GetBufferedRegion() );
    unsigned long ctr = 0;
    cit.Begin();
    while( !cit.IsAtEnd() )
      {
      itk::Array< PixelType > value = cit.Get();
      ++cit;
      if( ctr == (3*10*10 + 5*10 + 5) )
        {
        if( value != f )
          { 
          std::cerr << 
            "ImageRegionConstIteratorTest on VectorImage [FAILED]" << std::endl;
          failed = true;
          }
        }
      ++ctr;
      }
    std::cout << "ImageRegionConstIteratorTest on VectorImage [PASSED]" << std::endl;

    
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
        if( subRegion.IsInside( lcit.GetIndex() ) )
          {
          if( value!=f )
            {
            std::cerr << 
              "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
            failed = true;
            }
          }
        else
          {
          if( value!=ZeroPixel )
            {
            std::cerr << 
              "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
            failed = true;
            }
          }            
        ++lcit;
        }
      lcit.NextLine();
      } 

    VectorImageType::IndexType idx;
    idx[0]=1;idx[1]=1;idx[2]=1;
    LinearIteratorType lit( vectorImage, vectorImage->GetBufferedRegion() );
    lit.SetIndex( idx );
    lit.Set( f );
    
    lcit.SetIndex( idx );
    value = lcit.Get();
    if( value != f )
      {
      std::cerr << 
        "ImageLinearConstIteratorWithIndex on VectorImage [FAILED]" << std::endl;
      failed = true;
      }
    
    std::cout << "ImageLinearConstIteratorWithIndex on VectorImage [PASSED]" << std::endl;
    std::cout << "ImageLinearIteratorWithIndex on VectorImage [PASSED]" << std::endl;
    }

  }

  }


  // Test IO support.
  const unsigned int VectorLength = 6;
  const unsigned int Dimension    = 3;
  typedef float PixelType;


  {
  // Create an image using itk::Vector
  typedef itk::Vector< PixelType, VectorLength > VectorPixelType;
  typedef itk::Image< itk::Vector< PixelType, VectorLength >, 
                                      Dimension > VectorImageType;
  VectorImageType::Pointer image = VectorImageType::New();
  VectorImageType::IndexType start;
  start[0]=0; start[1]=0; start[2]=0;
  VectorImageType::SizeType size;
  size[0]=5; size[1]=5; size[2]=5;
  VectorImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();
  
  typedef itk::ImageRegionIteratorWithIndex< VectorImageType > IteratorType;
  IteratorType it( image, region );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    VectorPixelType f;
    f[0] = it.GetIndex()[0];
    f[1] = it.GetIndex()[1];
    f[2] = it.GetIndex()[2];
    f[3] = it.GetIndex()[0];
    f[4] = it.GetIndex()[1];
    f[5] = it.GetIndex()[2];
    it.Set( f );
    ++it;
    }

  typedef itk::ImageFileWriter< VectorImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( "Vector.mhd" );
  writer->Update();

  writer->SetFileName( "Vector.nrrd");
  writer->Update();
  }
  
  {
  // Now read it as a itk::VectorImage.
  typedef itk::VectorImage< PixelType, Dimension > VectorImageType;
  typedef itk::ImageFileReader< VectorImageType, itk::DefaultConvertPixelTraits< PixelType > > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( "Vector.nrrd" );
  reader->Update();

  VectorImageType::Pointer vectorImage = reader->GetOutput();
  
  typedef itk::ImageRegionConstIteratorWithIndex< VectorImageType > IteratorType;
  IteratorType cit( vectorImage, vectorImage->GetBufferedRegion() );
  cit.GoToBegin();

  bool failed1 = false;
  while( !cit.IsAtEnd() )
    {
    if(   (cit.Get()[0] != cit.GetIndex()[0])
       || (cit.Get()[1] != cit.GetIndex()[1])
       || (cit.Get()[2] != cit.GetIndex()[2])
       || (cit.Get()[3] != cit.GetIndex()[0])
       || (cit.Get()[4] != cit.GetIndex()[1])
       || (cit.Get()[5] != cit.GetIndex()[2]))
      {
      failed1 = true;
      }
    ++cit;
    }

  if( failed1 )
    {
    std::cerr << "Read VectorImage [FAILED]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "Read VectorImage [PASSED]" << std::endl;
    }

  
  // Now write this out this VectorImage and read it again
  typedef itk::ImageFileWriter< VectorImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( vectorImage );
  writer->SetFileName(argv[1]);
  writer->Update();
  }
  
  
  {
  // Now read it as a itk::VectorImage.
  typedef itk::VectorImage< PixelType, Dimension > VectorImageType;
  typedef itk::ImageFileReader< VectorImageType, itk::DefaultConvertPixelTraits< PixelType > > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  VectorImageType::Pointer vectorImage = reader->GetOutput();
  
  typedef itk::ImageRegionConstIteratorWithIndex< VectorImageType > IteratorType;
  IteratorType cit( vectorImage, vectorImage->GetBufferedRegion() );
  cit.GoToBegin();

  bool failed1 = false;
  while( !cit.IsAtEnd() )
    {
    if(   (cit.Get()[0] != cit.GetIndex()[0])
       || (cit.Get()[1] != cit.GetIndex()[1])
       || (cit.Get()[2] != cit.GetIndex()[2])
       || (cit.Get()[3] != cit.GetIndex()[0])
       || (cit.Get()[4] != cit.GetIndex()[1])
       || (cit.Get()[5] != cit.GetIndex()[2]))
      {
      failed1 = true;
      }
    ++cit;
    }

  if( failed1 )
    {
    std::cerr << "Write VectorImage [FAILED]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "Write VectorImage [PASSED]" << std::endl;
    }
  }

  if( failed )
    {
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
}

  
