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


#include "itkFastMarchingExtensionImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkCommand.h"
#include "itkMath.h"


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

int itkFastMarchingExtensionImageFilterTest(int, char* [] )
{
  // create a fastmarching object
  const unsigned int Dimension = 2;
  typedef float PixelType;

  typedef itk::Image< PixelType, Dimension > FloatImageType;

  typedef itk::FastMarchingThresholdStoppingCriterion< FloatImageType, FloatImageType >
      CriterionType;
  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetThreshold( 100. );

  typedef itk::FastMarchingExtensionImageFilterBase<
      FloatImageType, FloatImageType, unsigned char, 1 >
      MarcherType;

  MarcherType::Pointer marcher = MarcherType::New();
  marcher->SetStoppingCriterion( criterion );

  ShowProgressObject progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  marcher->AddObserver( itk::ProgressEvent(), command);


  bool passed;

  // setup trial points
  typedef MarcherType::NodePairType           NodePairType;
  typedef MarcherType::NodePairContainerType  NodePairContainerType;

  // setup alive points
  NodePairContainerType::Pointer AlivePoints = NodePairContainerType::New();

  FloatImageType::OffsetType offset0 = {{28,35}};

  itk::Index<2> index;
  index.Fill(0);

  AlivePoints->push_back( NodePairType( index + offset0, 0. ) );

  index.Fill( 200 );
  AlivePoints->push_back( NodePairType( index, 42. ) );

  marcher->SetAlivePoints( AlivePoints );


  // setup trial points
  NodePairContainerType::Pointer TrialPoints = NodePairContainerType::New();

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  TrialPoints->push_back( NodePairType( index, 1. ) );

  index[0] -= 1;
  index[1] += 1;
  TrialPoints->push_back( NodePairType( index, 1. ) );

  index[0] -= 1;
  index[1] -= 1;
  TrialPoints->push_back( NodePairType( index, 1. ) );

  index[0] += 1;
  index[1] -= 1;
  TrialPoints->push_back( NodePairType( index, 1. ) );

  index.Fill( 300 ); // this node is out of ranage
  TrialPoints->push_back( NodePairType( index, 42. ) );

  marcher->SetTrialPoints( TrialPoints );

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

  // deliberately cause an exception by not setting AuxAliveValues
  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }

  typedef MarcherType::AuxValueVectorType     VectorType;
  typedef MarcherType::AuxValueContainerType  AuxValueContainerType;

  AuxValueContainerType::Pointer auxAliveValues = AuxValueContainerType::New();

  // deliberately cause an exception setting AuxAliveValues of the wrong size
  marcher->SetAuxiliaryAliveValues( auxAliveValues );

  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  VectorType vector;
  vector[0] = 48;

  auxAliveValues->push_back( vector );
  auxAliveValues->push_back( vector );

  marcher->SetAuxiliaryAliveValues( auxAliveValues );

  // deliberately cause an exception by not setting AuxTrialValues
  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }

  AuxValueContainerType::Pointer auxTrialValues = AuxValueContainerType::New();

  // deliberately cause an exception setting AuxTrialValues of the wrong size
  marcher->SetAuxiliaryTrialValues( auxTrialValues );

  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  auxTrialValues->push_back( vector );
  auxTrialValues->push_back( vector );
  auxTrialValues->push_back( vector );
  auxTrialValues->push_back( vector );
  auxTrialValues->push_back( vector );

  marcher->SetAuxiliaryTrialValues( auxTrialValues );

  // run the algorithm
  passed = true;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = false;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  // check the results
  passed = true;
  FloatImageType::Pointer output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImageType>
    iterator( output, output->GetBufferedRegion() );

  typedef MarcherType::AuxImageType AuxImageType;
  AuxImageType::Pointer auxImage = marcher->GetAuxiliaryImage(0);
  itk::ImageRegionIterator<AuxImageType>
    auxIterator( auxImage, auxImage->GetBufferedRegion() );

  while( !iterator.IsAtEnd() )
    {
    FloatImageType::IndexType tempIndex;
    double distance;
    float outputValue;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = std::sqrt( distance );

    outputValue = (float) iterator.Get();

    if ( itk::Math::NotAlmostEquals( distance, 0.0 ) )
      {
      if ( itk::Math::abs( outputValue ) / distance > 1.42 )
        {
        std::cout << iterator.GetIndex() << " ";
        std::cout << itk::Math::abs( outputValue ) / distance << " ";
        std::cout << itk::Math::abs( outputValue ) << " " << distance << std::endl;
        passed = false;
        break;
        }

      if ( auxIterator.Get() != vector[0] )
        {
        std::cout << auxIterator.GetIndex()
                  << " got aux value of " << (double) auxIterator.Get()
                  << " but it should be  " << (double) vector[0]
                  << std::endl;
        passed = false;
        break;
        }
      }
    ++iterator;
    ++auxIterator;
    }

  // Exercise other member functions
  //std::cout << "Auxiliary alive values: " << marcher->GetAuxiliaryAliveValues();
  std::cout << std::endl;

  //std::cout << "Auxiliary trial values: " << marcher->GetAuxiliaryTrialValues();
  std::cout << std::endl;

  marcher->Print( std::cout );

  if ( marcher->GetAuxiliaryImage(2) )
    {
    std::cout << "GetAuxiliaryImage(2) should have returned ITK_NULLPTR";
    std::cout << std::endl;
    passed = false;
    }

  if ( passed )
    {
    std::cout << "Fast Marching test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  std::cout << "Fast Marching test failed" << std::endl;
  return EXIT_FAILURE;
}
