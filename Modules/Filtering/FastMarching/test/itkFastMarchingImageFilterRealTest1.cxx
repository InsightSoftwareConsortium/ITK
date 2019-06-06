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
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"
#include "itkCommand.h"


namespace{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject( itk::ProcessObject* o )
    { m_Process = o; }
  void ShowProgress()
    { std::cout << "Progress " << m_Process->GetProgress() << std::endl; }
  itk::ProcessObject::Pointer m_Process;
};
}

int itkFastMarchingImageFilterRealTest1( int itkNotUsed( argc ), char* itkNotUsed( argv )[] )
{

  itk::OutputWindow::SetInstance( itk::TextOutput::New().GetPointer() );

  // Create a Fast Marching image filter object
  using PixelType = float;
  constexpr unsigned Dimension  = 2;

  using FloatImageType = itk::Image< PixelType, Dimension >;

  using CriterionType =
      itk::FastMarchingThresholdStoppingCriterion< FloatImageType, FloatImageType >;

  using FastMarchingType =
      itk::FastMarchingImageFilterBase< FloatImageType, FloatImageType >;

  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetThreshold( 100. );

  FastMarchingType::Pointer marcher = FastMarchingType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( marcher, FastMarchingImageFilterBase,
    FastMarchingBase );


  marcher->SetStoppingCriterion( criterion );

  ShowProgressObject progressWatch( marcher );
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command =
    itk::SimpleMemberCommand< ShowProgressObject >::New();
  command->SetCallbackFunction( &progressWatch,
                               &ShowProgressObject::ShowProgress );
  marcher->AddObserver( itk::ProgressEvent(), command );

  using NodePairType = FastMarchingType::NodePairType;
  using NodePairContainerType = FastMarchingType::NodePairContainerType;

  // Set up alive points
  NodePairContainerType::Pointer alive = NodePairContainerType::New();

  NodePairType node_pair;

  FloatImageType::OffsetType offset0 = {{28, 35}};

  itk::Index< Dimension > index;
  index.Fill( 0 );

  node_pair.SetValue( 0.0 );
  node_pair.SetNode( index + offset0 );
  alive->push_back( node_pair );

  node_pair.SetValue( 42.0 );
  index.Fill( 200 );
  node_pair.SetNode( index ); // this node is out of range

  alive->push_back( node_pair );

  marcher->SetAlivePoints( alive );

  // Set up trial points
  NodePairContainerType::Pointer trial = NodePairContainerType::New();
  node_pair.SetValue( 1.0 );

  index.Fill (0 );
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
  index.Fill( 300 ); // this node is out of range
  node_pair.SetNode( index );
  trial->push_back( node_pair );

  marcher->SetTrialPoints( trial );

  // Specify the size of the output image
  FloatImageType::SizeType size = {{64, 64}};
  marcher->SetOutputSize( size );

  // Set up a speed image of ones
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

  // Turn on debugging
  marcher->DebugOn();

  // Update the Fast Marching filter
  ITK_TRY_EXPECT_NO_EXCEPTION( marcher->Update() );


  // Check the results
  FloatImageType::Pointer output = marcher->GetOutput();

  itk::ImageRegionIterator< FloatImageType >
    iterator( output, output->GetBufferedRegion() );

  bool passed = true;

  double threshold = 1.42;
  while ( !iterator.IsAtEnd() )
    {
    FloatImageType::IndexType tempIndex = iterator.GetIndex();
    tempIndex -= offset0;

    double distance = 0.0;
    for ( unsigned int j = 0; j < Dimension; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = std::sqrt( distance );

    auto outputValue = static_cast< double >( iterator.Get() );

    if ( distance > itk::NumericTraits< double >::epsilon() )
      {
      if ( itk::Math::abs( outputValue ) / distance > threshold )
        {
        std::cout << "Error at index [" << iterator.GetIndex() << "]"
          << std::endl;
        std::cout << "Expected scaled output value be less than: " << threshold
          <<", but got: " << itk::Math::abs( outputValue ) / distance
          << ", where output: " << itk::Math::abs( outputValue )
          << "; scale factor: " << distance << std::endl;
        passed = false;
        }
      }
    ++iterator;
    }


  if ( passed )
    {
    std::cout << "Test passed!" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

}
