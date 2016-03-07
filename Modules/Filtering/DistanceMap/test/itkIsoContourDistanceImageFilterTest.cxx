/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkIsoContourDistanceImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkMath.h"

#include "itkCommand.h"

// For debugging

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
    accum += itk::Math::sqr( p[j] - static_cast< double >( center[j] ) );
    }
  accum = std::sqrt( accum );
  return ( accum - radius );

}

}

int itkIsoContourDistanceImageFilterTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;
  typedef float PixelType;

  typedef itk::Image<PixelType,ImageDimension>      ImageType;
  typedef itk::Image<unsigned char,ImageDimension>  OutputImageType;
  typedef itk::Point<double,ImageDimension>         PointType;

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
  //multiplier->SetShift( 0.0 );

  // Set up  image filter
  typedef itk::IsoContourDistanceImageFilter<ImageType,ImageType> IsoContourType;
  IsoContourType::Pointer isocontour = IsoContourType::New();
  isocontour->SetInput( multiplier->GetOutput() );
  isocontour->SetFarValue(10);
  //  isocontour->SetNumberOfThreads(8);

  if( itk::Math::NotAlmostEquals( isocontour->GetFarValue(), 10 ) )
    {
    std::cout << "isocontour->GetFarValue() != 10" << std::endl;
    return EXIT_FAILURE;
    }

  ShowProgressObject progressWatch(isocontour);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  isocontour->AddObserver( itk::ProgressEvent(), command);

  // For debugging
  typedef itk::RescaleIntensityImageFilter<
                                ImageType,
                                OutputImageType >   CastFilterType;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput(isocontour->GetOutput());

  try
    {
    caster->Update();
    }
  catch (itk::ExceptionObject &err)
    {
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
    }

  // Create narrowband
  typedef IsoContourType::BandNodeType    BandNodeType;
  typedef IsoContourType::NarrowBandType  NarrowBandType;

  NarrowBandType::Pointer band = NarrowBandType::New();
  // Create nodes
  BandNodeType node;

  iter.GoToBegin();
  while (!iter.IsAtEnd())
    {
    if (itk::Math::abs(iter.Get()) < 5)
      {
      node.m_Index=iter.GetIndex();
      band->PushBack(node);
      }
    ++iter;
    }

  // Run isocontour with narrowband
  isocontour->NarrowBandingOn();
  // isocontour->SetNumberOfThreads(8);
  isocontour->SetNarrowBand(band.GetPointer());

  try
    {
    isocontour->Update();
    }
  catch (itk::ExceptionObject &err)
    {
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
    }

  // Check if inside/outside points remain the same after reinitialization
  typedef itk::MultiplyImageFilter<ImageType,ImageType,ImageType> CheckerType;
  CheckerType::Pointer checker = CheckerType::New();
  checker->SetInput1( image );
  checker->SetInput2( isocontour->GetOutput() );
  checker->Update();

  typedef itk::MinimumMaximumImageCalculator<ImageType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage( checker->GetOutput() );
  calculator->Compute();
  double minValue = calculator->GetMinimum();
  double maxValue = calculator->GetMaximum();

  std::cout << "Min. product = " << minValue << std::endl;
  std::cout << "Max. product = " << maxValue << std::endl;

  if ( minValue < 0.0 )
    {
    std::cout << "Inside/Outside mismatch at ";
    std::cout << calculator->GetIndexOfMinimum() << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other member functions
  isocontour->Print( std::cout );

  // Exercise the narrowband version
  isocontour->SetLevelSetValue( 1.0 );
  isocontour->SetLevelSetValue( 0.0 );
  isocontour->NarrowBandingOff();
  isocontour->Update();


  std::cout << "Level set value = " << isocontour->GetLevelSetValue() << std::endl;
  std::cout << "Narrow banding = " << isocontour->GetNarrowBanding() << std::endl;

  // We will use the output narrowband from the last run as the input narrowband
  //isocontour->SetInputNarrowBand( nodes );
  //isocontour->Update();


  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
