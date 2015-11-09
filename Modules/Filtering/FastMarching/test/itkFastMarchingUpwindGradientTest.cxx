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

#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMath.h"


//namespace{
//// The following class is used to support callbacks
//// on the filter in the pipeline that follows later
//class ShowProgressObject
//{
//public:
//  ShowProgressObject(itk::ProcessObject* o)
//    {m_Process = o;}
//  void ShowProgress()
//    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
//  itk::ProcessObject::Pointer m_Process;
//};
//}

int itkFastMarchingUpwindGradientTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // create a fastmarching object
  typedef float                   PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingUpwindGradientImageFilter<FloatImage,FloatImage>
                                  FloatFMType;

  FloatFMType::Pointer marcher = FloatFMType::New();

//   ShowProgressObject progressWatch(marcher);
//   itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
//   command = itk::SimpleMemberCommand<ShowProgressObject>::New();
//   command->SetCallbackFunction(&progressWatch,
//                                &ShowProgressObject::ShowProgress);
//   marcher->AddObserver( itk::ProgressEvent(), command);

  itk::SimpleFilterWatcher MarcherWatcher( marcher );

  typedef FloatFMType::NodeType      NodeType;
  typedef FloatFMType::NodeContainer NodeContainer;

  // setup alive points
  NodeContainer::Pointer alivePoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = {{28,35}};

  itk::Index<2> index;
  index.Fill(0);

  node.SetValue( 0.0 );
  node.SetIndex( index + offset0 );
  alivePoints->InsertElement(0, node);

  node.SetValue( 42.0 );
  index.Fill( 200 );
  node.SetIndex( index ); // this node is out of range
  alivePoints->InsertElement(1, node);

  marcher->SetAlivePoints( alivePoints );


  // setup trial points
  NodeContainer::Pointer trialPoints = NodeContainer::New();

  node.SetValue( 1.0 );

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  node.SetIndex( index );
  trialPoints->InsertElement(0, node);

  index[0] -= 1;
  index[1] += 1;
  node.SetIndex( index );
  trialPoints->InsertElement(1, node);

  index[0] -= 1;
  index[1] -= 1;
  node.SetIndex( index );
  trialPoints->InsertElement(2, node);

  index[0] += 1;
  index[1] -= 1;
  node.SetIndex( index );
  trialPoints->InsertElement(3, node);

  node.SetValue( 42.0 );
  index.Fill( 300 ); // this node is out of ranage
  node.SetIndex( index );
  trialPoints->InsertElement(4, node);

  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // setup a speed image of ones
  FloatImage::Pointer speedImage = FloatImage::New();
  FloatImage::RegionType region;
  region.SetSize( size );
  speedImage->SetLargestPossibleRegion( region );
  speedImage->SetBufferedRegion( region );
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImage>
    speedIter( speedImage, speedImage->GetBufferedRegion() );
  for (; !speedIter.IsAtEnd(); ++speedIter )
    {
    speedIter.Set( 1.0 );
    }

//  speedImage->Print( std::cout );
  marcher->SetInput( speedImage );
  double stoppingValue = 100.0;
  marcher->SetStoppingValue( stoppingValue );
  marcher->GenerateGradientImageOn();

  // Exercise this member function.
  // It is also necessary that the TargetOffset be set to 0.0 for the TargetReached
  // tests to pass.
  marcher->SetTargetOffset( 0.0 );

  // turn on debugging
// marcher->DebugOn();

  // update the marcher
  marcher->Update();

  // check the results
  typedef FloatFMType::GradientImageType FloatGradientImage;
  typedef FloatGradientImage::PixelType  GradientPixelType;
  FloatGradientImage::Pointer gradientOutput = marcher->GetGradientImage();
  itk::ImageRegionIterator<FloatGradientImage>
    iterator( gradientOutput, gradientOutput->GetBufferedRegion() );

  bool passed = true;

  for (; !iterator.IsAtEnd(); ++iterator )
    {

    FloatGradientImage::IndexType tempIndex;
    double distance;
    GradientPixelType outputPixel;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = std::sqrt( distance );

    outputPixel = iterator.Get();

    double outputPixelNorm = (double) outputPixel.GetNorm();

    if (distance == 0.0)
      {
      continue;
      }

    // for test to pass, gradient vectors must have norm = 1
    // (equal to the rhs of the Eikonal equation)
    // and must be oriented radially from the seed point

    double dot = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      dot += tempIndex[j] / distance * outputPixel[j];
      }

    if ( outputPixelNorm < 0.9999 || outputPixelNorm > 1.0001 ||
         dot < 0.99 || dot > 1.01 )
      {
      std::cout << iterator.GetIndex() << " ";
      std::cout << outputPixelNorm << " ";
      std::cout << dot << std::endl;
      passed = false;
      }

    }


  // Test that the stopping value of the algorithm is the one passed in.
  if( itk::Math::NotAlmostEquals( marcher->GetStoppingValue(), stoppingValue ) )
    {
    std::cerr << "ERROR: Output stopping value does not equal initial stopping value!" << std::endl;
    passed = false;
    }

  // Set up target points.
  // The algorithm will stop when it reaches these points.
  // This point is closest to the AlivePoint:
  FloatImage::OffsetType offset2 = {{40,40}};
  FloatImage::OffsetType offset1 = {{50,50}};
  // This point is farthest from the AlivePoint:
  FloatImage::OffsetType offset3 = {{0,0}};
  std::vector< FloatImage::OffsetType > targetOffsets;
  targetOffsets.push_back( offset1 );
  targetOffsets.push_back( offset2 );
  targetOffsets.push_back( offset3 );

  index.Fill(0);
  node.SetValue( 0.0 );
  NodeContainer::Pointer targetPoints = NodeContainer::New();
  for( unsigned int i = 0; i < targetOffsets.size(); i++ )
    {
    node.SetIndex( index + targetOffsets[i] );
    targetPoints->InsertElement(i, node);
    }
  marcher->SetTargetPoints( targetPoints );

  // Stop the algorithm when ONE of the targets has been reached.
  marcher->SetTargetReachedModeToOneTarget();
  marcher->Update();

  // Find the smallest reaching time of the TargetPoints.  This is the time of the closest
  // TargetPoint.
  FloatFMType::PixelType smallestReachingTime = itk::NumericTraits< PixelType >::max();
  for( unsigned int i = 0; i < targetOffsets.size(); i++ )
    {
    if( marcher->GetOutput()->GetPixel( index + targetOffsets[i] ) < smallestReachingTime )
      {
      smallestReachingTime = marcher->GetOutput()->GetPixel( index + targetOffsets[i] );
      }
    }

  // Since the algorithm is in OneTarget mode and the
  // TargetOffset is set to 0, the TargetValue should be equal to
  // the reaching time of the closest TargetPoint.
  if( itk::Math::NotAlmostEquals( smallestReachingTime, marcher->GetTargetValue() ) )
    {
    std::cerr << "ERROR: TargetValue does not equal reaching time of closest point!" << std::endl;
    passed = false;
    }


  // Now stop the algorithm once ALL of the targets have been reached.
  marcher->SetTargetReachedModeToAllTargets();
  marcher->Update();

  // Find the largest reaching time of the TargetPoints.  This is the largest time of
  // all of the target points.
  FloatFMType::PixelType largestReachingTime = itk::NumericTraits< PixelType >::NonpositiveMin();
  for( unsigned int i = 0; i < targetOffsets.size(); i++ )
    {
    if( marcher->GetOutput()->GetPixel( index + targetOffsets[i] ) > largestReachingTime )
      {
      largestReachingTime = marcher->GetOutput()->GetPixel( index + targetOffsets[i] );
      }
    }

  // Since the algorithm is now in AllTargets mode and the
  // TargetOffset is set to 0, the TargetValue should be equal to
  // the largest reaching time of the TargetPoints.
  if( itk::Math::NotAlmostEquals( largestReachingTime, marcher->GetTargetValue() ) )
    {
    std::cerr << "ERROR: TargetValue does not equal reaching time of farthest point!" << std::endl;
    passed = false;
    }

  // Now check to make sure that stoppingValue is reset correctly.
  marcher->SetTargetReachedModeToNoTargets();
  double newStoppingValue = 10.0;
  marcher->SetStoppingValue( newStoppingValue );
  marcher->Update();

  if( itk::Math::NotExactlyEquals(marcher->GetStoppingValue(), newStoppingValue) )
    {
    std::cerr << "ERROR: Output stopping value does not equal new stopping value!" << std::endl;
    passed = false;
    }


  // Exercise other member functions
  std::cout << "TargetOffset: " << marcher->GetTargetOffset() << std::endl;
  std::cout << "SpeedConstant: " << marcher->GetSpeedConstant() << std::endl;
  std::cout << "StoppingValue: " << marcher->GetStoppingValue() << std::endl;
  std::cout << "CollectPoints: " << marcher->GetCollectPoints() << std::endl;

  marcher->SetNormalizationFactor( 2.0 );
  std::cout << "NormalizationFactor: " << marcher->GetNormalizationFactor();
  std::cout << std::endl;

  std::cout << "SpeedImage: " << marcher->GetInput();
  std::cout << std::endl;

//  marcher->Print( std::cout );


  if ( passed )
    {
    std::cout << "Fast Marching Upwind Gradient test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching Upwind Gradient test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
