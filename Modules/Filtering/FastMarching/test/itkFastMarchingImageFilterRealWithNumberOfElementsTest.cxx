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


#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingNumberOfElementsStoppingCriterion.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkImageRegionConstIterator.h"


int itkFastMarchingImageFilterRealWithNumberOfElementsTest(int , char* [] )
{
  // create a fastmarching object
  typedef float PixelType;
  const unsigned Dimension = 2;

  typedef itk::Image< PixelType, Dimension > FloatImageType;

  typedef itk::FastMarchingNumberOfElementsStoppingCriterion< FloatImageType, FloatImageType >
      CriterionType;

  typedef itk::FastMarchingImageFilterBase< FloatImageType, FloatImageType >
    FastMarchingType;

  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetTargetNumberOfElements( 100 );

  FastMarchingType::Pointer marcher = FastMarchingType::New();
  marcher->SetStoppingCriterion( criterion );

  typedef FastMarchingType::NodePairType  NodePairType;
//  typedef FastMarchingType::NodeContainerType NodeContainerType;
  typedef FastMarchingType::NodePairContainerType NodePairContainerType;

  // setup alive points
  NodePairContainerType::Pointer alive = NodePairContainerType::New();

  NodePairType node_pair;

  FloatImageType::OffsetType offset0 = {{28,35}};

  itk::Index<2> index;
  index.Fill(0);

  node_pair.SetValue( 0.0 );
  node_pair.SetNode( index + offset0 );
  alive->push_back( node_pair );

  node_pair.SetValue( 42.0 );
  index.Fill( 200 );
  node_pair.SetNode( index ); // this node is out of range

  alive->push_back( node_pair );

  marcher->SetAlivePoints( alive );

  // setup trial points
  NodePairContainerType::Pointer trial = NodePairContainerType::New();
  node_pair.SetValue( 1.0 );

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  index[0] -= 1;
  index[1] += 1;
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  index[0] -= 1;
  index[1] -= 1;
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  index[0] += 1;
  index[1] -= 1;
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  node_pair.SetValue( 42.0 );
  index.Fill( 300 ); // this node is out of ranage
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  marcher->SetTrialPoints( trial );

  // specify the size of the output image
  FloatImageType::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // setup a speed image of ones
  FloatImageType::Pointer speedImage = FloatImageType::New();
  FloatImageType::RegionType region;
  region.SetSize( size );
  speedImage->SetLargestPossibleRegion( region );
  speedImage->SetBufferedRegion( region );
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImageType>
    speedIter( speedImage, speedImage->GetBufferedRegion() );
  while ( !speedIter.IsAtEnd() )
    {
    speedIter.Set( 1.0 );
    ++speedIter;
    }

  marcher->SetInput( speedImage );

  try
    {
    // update the marcher
    marcher->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  typedef char OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::BinaryThresholdImageFilter<FloatImageType, OutputImageType> ThresholdingFilterType;

  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();

  thresholder->SetLowerThreshold( 0.0 );
  thresholder->SetUpperThreshold( 100.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 1 );
  thresholder->SetInput( marcher->GetOutput() );

  try
    {
    thresholder->Update();
    }
   catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  OutputImageType::Pointer output = thresholder->GetOutput();

  typedef itk::ImageRegionConstIterator< OutputImageType > OutputIteratorType;

  OutputIteratorType it( output, output->GetLargestPossibleRegion() );
  it.GoToBegin();

  unsigned int counter = 0;

  while( !it.IsAtEnd() )
    {
    if( it.Get() == 1 )
      {
      ++counter;
      }
    ++it;
    }

  if( counter >= 100 )
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}
