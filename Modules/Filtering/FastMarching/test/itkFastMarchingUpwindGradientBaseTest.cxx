/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include "itkFastMarchingUpwindGradientImageFilterBase.h"
#include "itkFastMarchingReachedTargetNodesStoppingCriterion.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// namespace{
//// The following class is used to support callbacks
//// on the filter in the pipeline that follows later
// class ShowProgressObject
//{
// public:
//  ShowProgressObject(itk::ProcessObject* o)
//    {m_Process = o;}
//  void ShowProgress()
//    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
//  itk::ProcessObject::Pointer m_Process;
//};
//}

int
itkFastMarchingUpwindGradientBaseTest(int, char *[])
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // create a fastmarching object
  using PixelType = float;
  constexpr unsigned int Dimension = 2;

  using FloatImageType = itk::Image<PixelType, Dimension>;

  using CriterionType = itk::FastMarchingReachedTargetNodesStoppingCriterion<FloatImageType, FloatImageType>;

  auto criterion = CriterionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    criterion, FastMarchingReachedTargetNodesStoppingCriterion, FastMarchingStoppingCriterionBase);


  using FloatFMType = itk::FastMarchingUpwindGradientImageFilterBase<FloatImageType, FloatImageType>;

  auto marcher = FloatFMType::New();

  //   ShowProgressObject progressWatch(marcher);
  //   itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  //   command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  //   command->SetCallbackFunction(&progressWatch,
  //                                &ShowProgressObject::ShowProgress);
  //   marcher->AddObserver( itk::ProgressEvent(), command);

  itk::SimpleFilterWatcher MarcherWatcher(marcher);

  using NodeType = FloatFMType::NodeType;
  using NodePairType = FloatFMType::NodePairType;

  using NodePairContainerType = FloatFMType::NodePairContainerType;

  // setup alive points
  auto AlivePoints = NodePairContainerType::New();

  FloatImageType::OffsetType offset0 = { { 28, 35 } };

  itk::Index<2> index;
  index.Fill(0);

  AlivePoints->push_back(NodePairType(index + offset0, 0.));
  AlivePoints->push_back(NodePairType(index + offset0, 42.));

  marcher->SetAlivePoints(AlivePoints);

  // setup trial points
  auto TrialPoints = NodePairContainerType::New();

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  TrialPoints->push_back(NodePairType(index, 1.));

  index[0] -= 1;
  index[1] += 1;
  TrialPoints->push_back(NodePairType(index, 1.));

  index[0] -= 1;
  index[1] -= 1;
  TrialPoints->push_back(NodePairType(index, 1.));

  index[0] += 1;
  index[1] -= 1;
  TrialPoints->push_back(NodePairType(index, 1.));

  index.Fill(300); // this node is out of range
  TrialPoints->push_back(NodePairType(index, 42.));

  marcher->SetTrialPoints(TrialPoints);

  // specify the size of the output image
  FloatImageType::SizeType size = { { 64, 64 } };
  marcher->SetOutputSize(size);

  // setup a speed image of ones
  auto                       speedImage = FloatImageType::New();
  FloatImageType::RegionType region;
  region.SetSize(size);
  speedImage->SetLargestPossibleRegion(region);
  speedImage->SetBufferedRegion(region);
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImageType> speedIter(speedImage, speedImage->GetBufferedRegion());

  while (!speedIter.IsAtEnd())
  {
    speedIter.Set(1.0);
    ++speedIter;
  }

  //  speedImage->Print( std::cout );
  marcher->SetInput(speedImage);

  // check the results
  using FloatGradientImage = FloatFMType::GradientImageType;
  using GradientPixelType = FloatGradientImage::PixelType;
  FloatGradientImage::Pointer                  gradientOutput = marcher->GetGradientImage();
  itk::ImageRegionIterator<FloatGradientImage> iterator(gradientOutput, gradientOutput->GetBufferedRegion());

  bool passed = true;

  while (!iterator.IsAtEnd())
  {
    FloatGradientImage::IndexType tempIndex;
    double                        distance;
    GradientPixelType             outputPixel;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for (int j = 0; j < 2; ++j)
    {
      distance += tempIndex[j] * tempIndex[j];
    }
    distance = std::sqrt(distance);

    outputPixel = iterator.Get();

    double outputPixelNorm{ outputPixel.GetNorm() };

    if (itk::Math::AlmostEquals(distance, 0.0))
    {
      continue;
    }

    // for test to pass, gradient vectors must have norm = 1
    // (equal to the rhs of the Eikonal equation)
    // and must be oriented radially from the seed point

    double dot = 0.0;
    for (int j = 0; j < 2; ++j)
    {
      dot += tempIndex[j] / distance * outputPixel[j];
    }

    if ((outputPixelNorm < 0.9999) || (outputPixelNorm > 1.0001) || (dot < 0.99) || (dot > 1.01))
    {
      std::cout << iterator.GetIndex() << " ";
      std::cout << outputPixelNorm << " ";
      std::cout << dot << std::endl;
      passed = false;
    }

    ++iterator;
  }

  // Set up target points.
  // The algorithm will stop when it reaches these points.
  // This point is closest to the AlivePoint:
  constexpr FloatImageType::OffsetType offset1 = { { 50, 50 } };
  constexpr FloatImageType::OffsetType offset2 = { { 40, 40 } };
  // This point is farthest from the AlivePoint:
  constexpr FloatImageType::OffsetType          offset3 = { { 0, 0 } };
  const std::vector<FloatImageType::OffsetType> targetOffsets{ offset1, offset2, offset3 };

  std::vector<NodeType> TargetNodes;
  for (const auto targetOffset : targetOffsets)
  {
    TargetNodes.push_back(index + targetOffset);
  }
  criterion->SetTargetNodes(TargetNodes);

  auto targetOffset = itk::NumericTraits<typename CriterionType::OutputPixelType>::ZeroValue();
  criterion->SetTargetOffset(targetOffset);
  ITK_TEST_SET_GET_VALUE(targetOffset, criterion->GetTargetOffset());

  // Stop the algorithm when ONE of the targets has been reached.
  auto targetCondition = CriterionType::TargetConditionEnum::OneTarget;
  criterion->SetTargetCondition(targetCondition);
  ITK_TEST_SET_GET_VALUE(targetCondition, criterion->GetTargetCondition());


  std::cout << "Criterion description: " << criterion->GetDescription() << std::endl;

  marcher->SetStoppingCriterion(criterion);

  marcher->Update();

  // Test streaming enumeration for FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition elements
  const std::set<itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition> allTargetCondition{
    itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::OneTarget,
    itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::SomeTargets,
    itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::AllTargets
  };
  for (const auto & ee : allTargetCondition)
  {
    std::cout << "STREAMED ENUM VALUE FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition: " << ee
              << std::endl;
  }

  if (passed)
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
