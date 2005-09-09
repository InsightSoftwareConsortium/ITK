/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorImageTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

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
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"


// This test tests:
// VectorImage, iterators on the image, 
// Timing tests comparing it to similar images using FixedArray and Array
// IO support.

int itkVectorImageTest( int, char* argv[] )
{
  bool failed = false;

  const unsigned int VectorLength = 6;
  const unsigned int Dimension    = 3;
  typedef float PixelType;

  {
  // Test 1.
  //
  // Create an Image of Array, FixedArray, VectorImage of length 6 and compare
  // times.
  //   
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
  size[0]  = 50;  // size along X
  size[1]  = 50;  // size along Y
  size[2]  = 50;  // size along Z
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
  size[0]  = 50;  // size along X
  size[1]  = 50;  // size along Y
  size[2]  = 50;  // size along Z
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
  size[0]  = 50;  // size along X
  size[1]  = 50;  // size along Y
  size[2]  = 50;  // size along Z
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

  
    {
    // Check support for Neighborhood Iterators
    //
    // 1. Test ConstNeighborhoodIterator
    // 
    std::cout << "Testing ConstNeighborhoodIterator...." << std::endl;
    
    typedef itk::ConstNeighborhoodIterator< VectorImageType > 
                                         ConstNeighborhoodIteratorType;
    ConstNeighborhoodIteratorType::RadiusType radius;
    radius[0] = radius[1] = radius[2] = 1;

    ConstNeighborhoodIteratorType::RegionType region 
                            = vectorImage->GetBufferedRegion();
    ConstNeighborhoodIteratorType::SizeType size;
    size[0]= size[1] = size[2] = 4;
    ConstNeighborhoodIteratorType::IndexType index;
    index[0] = index[1] = index[2] = 1; 
    region.SetIndex(index); 
    region.SetSize( size );
    
    ConstNeighborhoodIteratorType cNit(radius, vectorImage, region);

    // Move Iterator to a point and see if it reads out the right value
    //
    const unsigned int centerIndex = static_cast< unsigned int >(
                ((radius[0]*2+1)*(radius[1]*2+1)*(radius[2]*2+1))/2);

    ConstNeighborhoodIteratorType::IndexType location;
    location[0] = 1; location[1] = 2;  location[2] = 3;
    cNit.SetLocation( location );
    
    if( cNit.GetPixel(centerIndex) != vectorImage->GetPixel( location ) )
      {
      std::cerr << "  SetLocation [FAILED]" << std::endl;
      failed=true;
      }

    // Test GoToBegin()
    cNit.GoToBegin();
    if( cNit.GetPixel(centerIndex) != vectorImage->GetPixel( index ) )
      {
      std::cerr << "  GoToBegin [FAILED]" << std::endl;
      failed=true;
      }

    // Test GoToEnd()
    cNit.GoToEnd(); 
    --cNit;
    ConstNeighborhoodIteratorType::IndexType endIndex;
    endIndex[0] = endIndex[1] = endIndex[2] = 4;
    if( cNit.GetPixel(centerIndex) != vectorImage->GetPixel( endIndex ) )
      {
      std::cerr << "  GoToEnd [FAILED]" << std::endl;
      failed=true;
      }

    // Test IsAtEnd()
    if( !((++cNit).IsAtEnd()) )
      {
      std::cerr << "  IsAtEnd() [FAILED]" << std::endl;  
      failed = true;
      }
    cNit.GoToBegin();
    unsigned int numPixelsTraversed = size[0]*size[1]*size[2];
    while (! cNit.IsAtEnd())
      {
      ++cNit;
      --numPixelsTraversed;
      }
    if( numPixelsTraversed ) { std::cerr << "  IsAtEnd() [FAILED]" << std::endl; }

    // Test operator-
    --cNit;
    ConstNeighborhoodIteratorType::OffsetType offset;
    offset[0] = offset[1] = offset[2] = 1;
    cNit -= offset;
    itk::Array< PixelType > pixel = cNit.GetCenterPixel();
    itk::Array< PixelType > correctAnswer( VectorLength );
    correctAnswer.Fill( 3 );
    if( pixel != correctAnswer ) 
      {
      std::cerr << "  operator- [FAILED]" << std::endl;
      failed = true;
      }

    // Test GetNeighborhood()
    cNit.SetLocation( location );
    ConstNeighborhoodIteratorType::NeighborhoodType 
                    neighborhood = cNit.GetNeighborhood();
    const unsigned int neighborhoodSize = neighborhood.Size();
    //for( unsigned int i=0; i< neighborhoodSize; i++)
    //  { std::cout << neighborhood[i] << std::endl; }
    if( (neighborhood[0][0] != 0) || (neighborhood[0][5] != 2))
      {
      std::cerr << "  GetNeighborhood() on ConstNeighborhoodIterator [FAILED]" << std::endl;
      failed = true;
      }



    // 
    // 2. Test NeighborhoodIterator on VectorImage
    //
    
    std::cout << "Testing NeighborhoodIterator..." << std::endl;
    
    typedef itk::NeighborhoodIterator< VectorImageType > NeighborhoodIteratorType;
    NeighborhoodIteratorType nit(radius, vectorImage, region);
    nit.SetLocation( location );
    itk::Array< PixelType > p( VectorLength );
    p.Fill( 100.0 );
    nit.SetNext( 1, 1, p );
    
    // Test SetNext()
    NeighborhoodIteratorType::IndexType index1;
    index1 = location; index1[1] = location[1] + 1;
    nit.SetLocation( index1 );

    if( nit.GetCenterPixel() != p )
      {
      std::cerr << "  SetNext() [FAILED]" << std::endl;
      failed = true;
      }
    p[0] = p[3] = (float)index1[0];  
    p[1] = p[4] = (float)index1[1]; 
    p[2] = p[5] = (float)index1[2]; 
    nit.SetCenterPixel( p );
    if( nit.GetCenterPixel() != p )
      {
      std::cerr << "  SetCenterPixel() [FAILED]" << std::endl;
      failed = true;
      }
    
    // Test SetNeighborhood() and GetPrevious()
    nit.SetLocation( index1 );
    nit.SetNeighborhood( neighborhood );
    p[0] = p[3] = 1; p[1] = p[4] = 2; p[2] = p[5] = 2;
    if( nit.GetPrevious( 2, 1 ) != p )
      {
      std::cerr << "  SetNeighborhood() or GetPrevious() [FAILED]" << std::endl;
      failed = true;
      }
    

    // 
    // 3. Testing ConstShapedNeighborhoodIterator on VectorImage
    //

    // Go back to original image, where pixel values tell us the indices
    std::cout << "Testing ConstShapedNeighborhoodIterator on VectorImage..." 
                                                                  << std::endl;
    reader->SetFileName( "dummy.nrrd");
    reader->SetFileName( argv[1] );
    reader->Update();
    vectorImage = reader->GetOutput(); 

    typedef itk::ConstShapedNeighborhoodIterator< VectorImageType > 
                                   ConstShapedNeighborhoodIteratorType;
    ConstShapedNeighborhoodIteratorType cSnit( radius, vectorImage, region );
    cSnit.SetLocation( location );
    ConstShapedNeighborhoodIteratorType::OffsetType offset1;
    offset1[0] = 0; offset1[1] = 0; offset1[2] = 0;
    cSnit.ActivateOffset( offset1 ); //activate the center
    // activate the top plane
    offset1[0] = 0; offset1[1] = 0; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = 0; offset1[1] = 1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = 0; offset1[1] = -1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = 1; offset1[1] = 0; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = 1; offset1[1] = 1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = 1; offset1[1] = -1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = -1; offset1[1] = 0; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = -1; offset1[1] = 1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
    offset1[0] = -1; offset1[1] = -1; offset1[2] = -1;
    cSnit.ActivateOffset( offset1 ); 
 
    ConstShapedNeighborhoodIteratorType::IndexListType l
                                  = cSnit.GetActiveIndexList();
    ConstShapedNeighborhoodIteratorType::IndexListType::const_iterator 
                                                        ali = l.begin();
    while (ali != l.end())
      {
      std::cout << *ali << " ";
      ++ali;
      }
    std::cout << std::endl;

    ConstShapedNeighborhoodIteratorType::ConstIterator ci = cSnit.Begin();
    while (! ci.IsAtEnd())
      {
      ConstShapedNeighborhoodIteratorType::OffsetType offset2 = ci.GetNeighborhoodOffset();
      if( (offset2[0] == -1) && (offset2[1]== -1) && (offset2[2]== -1) )
        { 
        if( ci.GetNeighborhoodIndex() != 0 )
          {
          failed = true;
          std::cerr << "GetNeighborhoodOffset() on ConstShapedNeighborhoodIterato [FAILED]" 
                                                                              << std::endl;
          }
        if( (ci.Get()[0]!=0) || (ci.Get()[1]!=1) || (ci.Get()[2]!=2) )
          {
          failed=true;
          std::cerr 
            << "ConstShapedNeighborhoodIterator returned incorrect index [FAILED]" 
                                                                        << std::endl;
          }
        }
      ci++;
      }

    //
    // 4. Test ShapedNeighborhoodIterator 
    //
    typedef itk::ShapedNeighborhoodIterator< VectorImageType > 
                                      ShapedNeighborhoodIteratorType;
    ShapedNeighborhoodIteratorType sNit( radius, vectorImage, region );
    sNit.SetLocation( location );
    ShapedNeighborhoodIteratorType::Iterator shit = sNit.Begin();
    shit = sNit.Begin(); 
    p[0] = p[3] = 10; p[1] = p[4] = 20; p[2] = p[5] = 30;
    shit.Set( p );
    index[0]=location[0]-1; index[1]=location[1]-1; index[2]=location[2]-1;
    cNit.SetLocation( index );
    if( cNit.GetCenterPixel() != p )
      {
      std::cerr << "ShapedNeighborhoodIterator Set() [FAILED]" << std::endl;
      failed=true;
      }

    
    }  // End Testing Neighborhood Iterators on VectorImage

  }

  if( failed )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

  
