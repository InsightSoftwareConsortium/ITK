/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocitiesImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkExtensionVelocitiesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShiftScaleImageFilter.h"
#include "itkDifferenceImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

#include "itkCommand.h"
#include "vnl/vnl_math.h"

// For debugging
#include "itkImageFileWriter.h"

namespace{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
}

// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 50 );
  double radius = 19.5;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += vnl_math_sqr( p[j] - center[j] );
    }
  accum = vcl_sqrt( accum );
  return ( accum - radius );

}


int itkExtensionVelocitiesImageFilterTest(int, char* [] )
{

  const unsigned int ImageDimension = 2;
  typedef float PixelType;
  
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef ImageType::IndexType IndexType;
  typedef itk::Point<double,ImageDimension> PointType;

  // Fill an input image with simple signed distance function
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 128 );
  ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) );
    ++iter;
    }

  // Squash up the level sets by mulitplying with a scalar
  typedef itk::ShiftScaleImageFilter<ImageType,ImageType> MultiplierType;
  MultiplierType::Pointer multiplier = MultiplierType::New();
  multiplier->SetInput( image );
  multiplier->SetScale( 0.5 );

  // Set up auxiliary variables
  ImageType::Pointer aux1 = ImageType::New();
  aux1->SetRegions( region );
  aux1->Allocate();

  aux1->FillBuffer( 1.0 );

  ImageType::Pointer aux2 = ImageType::New();
  aux2->SetRegions( region );
  aux2->Allocate();

  aux2->FillBuffer( 2.0 );


  // Set up reinitialize level set image filter
  const unsigned int AuxDimension = 2;
  typedef itk::ExtensionVelocitiesImageFilter<ImageType,float,AuxDimension> ReinitializerType;
  ReinitializerType::Pointer reinitializer = ReinitializerType::New();
  reinitializer->SetInput( multiplier->GetOutput() );
  reinitializer->SetInputVelocityImage( aux1, 0 );
  reinitializer->SetInputVelocityImage( aux2, 1 );

  ShowProgressObject progressWatch(reinitializer);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  reinitializer->AddObserver( itk::ProgressEvent(), command);

  // For debugging
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reinitializer->GetOutputVelocityImage( 1 ) );
  writer->SetFileName( "output.mhd" );
  writer->Write();

  // Check the output
  typedef itk::DifferenceImageFilter<ImageType,ImageType> DifferenceType;
  DifferenceType::Pointer difference = DifferenceType::New();
  difference->SetTestInput( image );
  difference->SetValidInput( reinitializer->GetOutput() );
  difference->Update();

  typedef itk::MinimumMaximumImageCalculator<ImageType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage( difference->GetOutput() );
  calculator->Compute();

  double maxAbsDifference = calculator->GetMaximum();
  IndexType maxAbsDifferenceIndex = calculator->GetIndexOfMaximum();

  std::cout << "Max. abs. difference = " << maxAbsDifference;
  std::cout << " at " << maxAbsDifferenceIndex << std::endl;

  if ( maxAbsDifference > 1.0 )
    {
    std::cout << "Difference above threshold of 1.0" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other member functions
  reinitializer->Print( std::cout );
 
  reinitializer->SetLevelSetValue( 1.0 );
  reinitializer->NarrowBandingOn();
  reinitializer->SetNarrowBandwidth( 8 );
  reinitializer->Update();
 
  typedef ReinitializerType::NodeContainerPointer NodeContainerPointer;
  NodeContainerPointer nodes = reinitializer->GetOutputNarrowBand();

  std::cout << "Level set value = " << reinitializer->GetLevelSetValue() << std::endl;
  std::cout << "Narrow banding = " << reinitializer->GetNarrowBanding() << std::endl;
  std::cout << "Narrow bandwidth = " << reinitializer->GetOutputNarrowBandwidth() << std::endl;
  std::cout << "No. nodes = " << nodes->Size() << std::endl;


  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}

