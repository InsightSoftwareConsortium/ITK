/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkApproximateSignedDistanceMapImageFilterTest.cxx
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

#include "itkApproximateSignedDistanceMapImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "vnl/vnl_math.h"

// For debugging
//#include "itkImageFileWriter.h"

namespace{

// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 32 );
  double radius = 16;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += vnl_math_sqr( p[j] - center[j] );
    }
  accum = vcl_sqrt( accum );
  return ( accum - radius );
}

}



int itkApproximateSignedDistanceMapImageFilterTest(int, char* [] )
{

  const unsigned int ImageDimension = 2;
  typedef unsigned int InputPixelType;
  typedef float OutputPixelType;
  
  typedef itk::Image<InputPixelType,ImageDimension> InputImageType;
  typedef itk::Image<OutputPixelType,ImageDimension> OutputImageType;
  typedef InputImageType::IndexType IndexType;
  typedef itk::Point<double,ImageDimension> PointType;

  // Make a binary input image based on the signed distance function
  // using the inside and outside values
  const InputPixelType InsideValue = 100;
  const InputPixelType OutsideValue = 0;

  InputImageType::Pointer image = InputImageType::New();
  InputImageType::SizeType size;
  size.Fill( 64 );
  InputImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<InputImageType> InputIterator;
  InputIterator iter( image, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) > 0 ? OutsideValue : InsideValue );
    ++iter;
    }

  
  // Set up  image filter
  typedef itk::ApproximateSignedDistanceMapImageFilter<InputImageType,OutputImageType> DistanceType;
  DistanceType::Pointer distance = DistanceType::New();
  distance->SetInput( image );
  distance->SetInsideValue(InsideValue);
  distance->SetOutsideValue(OutsideValue);
  
  try
    {
  
//    typedef itk::ImageFileWriter<InputImageType> InputWriterType;
//    InputWriterType::Pointer iwriter = InputWriterType::New();
//    iwriter->SetInput( image );
//    iwriter->SetFileName( "input.mhd" );
//    iwriter->Update();
   
    distance->Update();
      
//    typedef itk::ImageFileWriter<OutputImageType> OutputWriterType;
//    OutputWriterType::Pointer owriter = OutputWriterType::New();
//    owriter->SetInput( distance->GetOutput() );
//    owriter->SetFileName( "output.mhd" );
//    owriter->Update();
    }
  catch (itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }
    
  OutputPixelType maxDeviation = 0;
  
  typedef itk::ImageRegionConstIteratorWithIndex<OutputImageType> OutputIterator;
  OutputIterator oiter( distance->GetOutput(), 
                       distance->GetOutput()->GetLargestPossibleRegion() );
  oiter.GoToBegin();
  
  while( !oiter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( oiter.GetIndex(), point );
    OutputPixelType deviation = 
      vnl_math_abs(oiter.Get() - SimpleSignedDistance(point) );
    if (deviation > maxDeviation )
      {
      maxDeviation = deviation;
      }
    ++oiter;
    }
  std::cout << "dev " << maxDeviation << std::endl;

  // Exercise other member functions
  distance->Print( std::cout );
 
  std::cout << "The maximum error was " << maxDeviation << std::endl;
  if ( maxDeviation > 2 )
    { 
    std::cout << "The output image had pixels too far away from the correct distance. ";
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }  
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
