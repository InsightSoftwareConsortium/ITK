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
#include "itkCommand.h"


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

int itkFastMarchingImageFilterRealTest1(int argc, char* argv[] )
{
  (void) argc;
  (void) argv;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // create a fastmarching object
  typedef float PixelType;
  const unsigned Dimension = 2;

  typedef itk::Image< PixelType, Dimension > FloatImageType;

  typedef itk::FastMarchingThresholdStoppingCriterion< FloatImageType, FloatImageType >
      CriterionType;

  typedef itk::FastMarchingImageFilterBase< FloatImageType, FloatImageType >
    FastMarchingType;

  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetThreshold( 100. );

  FastMarchingType::Pointer marcher = FastMarchingType::New();
  marcher->SetStoppingCriterion( criterion );

  ShowProgressObject progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  marcher->AddObserver( itk::ProgressEvent(), command);

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

  speedImage->Print( std::cout );
  marcher->SetInput( speedImage );

  // turn on debugging
  marcher->DebugOn();

  // update the marcher
  marcher->Update();

  // check the results
  FloatImageType::Pointer output = marcher->GetOutput();

  itk::ImageRegionIterator<FloatImageType>
    iterator( output, output->GetBufferedRegion() );

  bool passed = true;

  while ( !iterator.IsAtEnd() )
    {
    FloatImageType::IndexType tempIndex = iterator.GetIndex();
    tempIndex -= offset0;

    double distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = std::sqrt( distance );

    double outputValue = static_cast< double >( iterator.Get() );

    //std::cout << iterator.GetIndex() <<" ** " <<outputValue <<std::endl;
    if (distance > itk::NumericTraits< double >::epsilon() )
      {
      if ( itk::Math::abs( outputValue ) / distance > 1.42 )
        {
        std::cout << iterator.GetIndex() << " ";
        std::cout << itk::Math::abs( outputValue ) / distance << " ";
        std::cout << itk::Math::abs( outputValue ) << " " << distance << std::endl;
        passed = false;
        }
      }
    ++iterator;
    }

  std::cout << "SpeedConstant: " << marcher->GetSpeedConstant() << std::endl;
  std::cout << "StoppingValue: " << marcher->GetTargetReachedValue() << std::endl;
  std::cout << "SpeedImage: " << marcher->GetInput() << std::endl;

  // Exercise other member functions
  /*
  std::cout << "CollectPoints: " << marcher->GetCollectPoints() << std::endl;

  marcher->SetNormalizationFactor( 2.0 );
  std::cout << "NormalizationFactor: " << marcher->GetNormalizationFactor();
  std::cout << std::endl;


  std::cout << std::endl;

  marcher->Print( std::cout );*/

  if ( passed )
    {
    std::cout << "Fast Marching test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching test failed" << std::endl;
    return EXIT_FAILURE;
    }

}
