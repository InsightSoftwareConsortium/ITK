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

#include "itkFastMarchingImageFilter.h"
#include "itkTextOutput.h"
#include "itkCommand.h"
#include "itkTestingMacros.h"


namespace
{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};
} // namespace

int
itkFastMarchingTest(int argc, char * argv[])
{
  if (argc != 9)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " speedConstant normalizationFactor stoppingValue collectPoints outputIndexValue outputSpacingValue "
                 "outputOriginValue overrideOutputInformation "
              << std::endl;
    return EXIT_FAILURE;
  }

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // create a fastmarching object
  using PixelType = float;
  using FloatImage = itk::Image<PixelType, 2>;
  using FloatFMType = itk::FastMarchingImageFilter<FloatImage, FloatImage>;

  auto marcher = FloatFMType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(marcher, FastMarchingImageFilter, ImageToImageFilter);


  ShowProgressObject                                    progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  marcher->AddObserver(itk::ProgressEvent(), command);

  using NodeType = FloatFMType::NodeType;
  using NodeContainer = FloatFMType::NodeContainer;

  // setup alive points
  auto alivePoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = { { 28, 35 } };

  itk::Index<2> index;
  index.Fill(0);

  node.SetValue(0.0);
  node.SetIndex(index + offset0);
  alivePoints->InsertElement(0, node);

  node.SetValue(42.0);
  index.Fill(200);
  node.SetIndex(index); // this node is out of range
  alivePoints->InsertElement(1, node);

  marcher->SetAlivePoints(alivePoints);
  ITK_TEST_SET_GET_VALUE(alivePoints, marcher->GetAlivePoints());


  // setup trial points
  auto trialPoints = NodeContainer::New();

  node.SetValue(1.0);

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  node.SetIndex(index);
  trialPoints->InsertElement(0, node);

  index[0] -= 1;
  index[1] += 1;
  node.SetIndex(index);
  trialPoints->InsertElement(1, node);

  index[0] -= 1;
  index[1] -= 1;
  node.SetIndex(index);
  trialPoints->InsertElement(2, node);

  index[0] += 1;
  index[1] -= 1;
  node.SetIndex(index);
  trialPoints->InsertElement(3, node);

  node.SetValue(42.0);
  index.Fill(300); // this node is out of range
  node.SetIndex(index);
  trialPoints->InsertElement(4, node);

  marcher->SetTrialPoints(trialPoints);
  ITK_TEST_SET_GET_VALUE(trialPoints, marcher->GetTrialPoints());

  auto speedConstant = std::stod(argv[1]);
  marcher->SetSpeedConstant(speedConstant);
  ITK_TEST_SET_GET_VALUE(speedConstant, marcher->GetSpeedConstant());

  auto normalizationFactor = std::stod(argv[2]);
  marcher->SetNormalizationFactor(normalizationFactor);
  ITK_TEST_SET_GET_VALUE(normalizationFactor, marcher->GetNormalizationFactor());

  auto stoppingValue = std::stod(argv[3]);
  marcher->SetStoppingValue(stoppingValue);
  ITK_TEST_SET_GET_VALUE(stoppingValue, marcher->GetStoppingValue());

  auto collectPoints = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(marcher, CollectPoints, collectPoints);

  typename FloatImage::SizeType size = { { 64, 64 } };
  marcher->SetOutputSize(size);
  ITK_TEST_SET_GET_VALUE(size, marcher->GetOutputSize());

  auto outputRegionIndexValue =
    static_cast<typename FloatFMType::LevelSetImageType::IndexType::IndexValueType>(std::stoi(argv[5]));
  typename FloatFMType::LevelSetImageType::IndexType outputRegionIndex;
  outputRegionIndex.Fill(outputRegionIndexValue);
  typename FloatFMType::OutputRegionType outputRegion;
  outputRegion.SetSize(size);
  outputRegion.SetIndex(outputRegionIndex);
  marcher->SetOutputRegion(outputRegion);
  ITK_TEST_SET_GET_VALUE(outputRegion, marcher->GetOutputRegion());

  auto outputSpacingValue = static_cast<typename FloatFMType::OutputSpacingType::ValueType>(std::stod(argv[6]));
  auto outputSpacing = itk::MakeFilled<typename FloatFMType::OutputSpacingType>(outputSpacingValue);
  marcher->SetOutputSpacing(outputSpacing);
  ITK_TEST_SET_GET_VALUE(outputSpacing, marcher->GetOutputSpacing());

  typename FloatFMType::OutputDirectionType outputDirection;
  outputDirection.SetIdentity();
  marcher->SetOutputDirection(outputDirection);
  ITK_TEST_SET_GET_VALUE(outputDirection, marcher->GetOutputDirection());

  auto outputOriginValue = static_cast<typename FloatFMType::OutputPointType::ValueType>(std::stod(argv[7]));
  auto outputOrigin = itk::MakeFilled<typename FloatFMType::OutputPointType>(outputOriginValue);
  marcher->SetOutputOrigin(outputOrigin);
  ITK_TEST_SET_GET_VALUE(outputOrigin, marcher->GetOutputOrigin());

  auto overrideOutputInformation = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(marcher, OverrideOutputInformation, overrideOutputInformation);

  // setup a speed image of ones
  auto                   speedImage = FloatImage::New();
  FloatImage::RegionType region;
  region.SetSize(size);
  speedImage->SetLargestPossibleRegion(region);
  speedImage->SetBufferedRegion(region);
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImage> speedIter(speedImage, speedImage->GetBufferedRegion());
  for (; !speedIter.IsAtEnd(); ++speedIter)
  {
    speedIter.Set(1.0);
  }

  marcher->SetInput(speedImage);
  ITK_TEST_SET_GET_VALUE(speedImage, marcher->GetInput());

  // turn on debugging
  marcher->DebugOn();

  // update the marcher
  marcher->Update();

  // check the results
  FloatImage::Pointer                  output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage> iterator(output, output->GetBufferedRegion());

  bool passed = true;

  for (; !iterator.IsAtEnd(); ++iterator)
  {

    FloatImage::IndexType tempIndex;
    double                distance;
    float                 outputValue;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for (int j = 0; j < 2; ++j)
    {
      distance += tempIndex[j] * tempIndex[j];
    }
    distance = std::sqrt(distance);

    outputValue = static_cast<float>(iterator.Get());

    if (distance < itk::NumericTraits<double>::epsilon())
    {
      continue;
    }
    if (itk::Math::abs(outputValue) / distance > 1.42)
    {
      std::cout << iterator.GetIndex() << " ";
      std::cout << itk::Math::abs(outputValue) / distance << " ";
      std::cout << itk::Math::abs(outputValue) << " " << distance << std::endl;
      passed = false;
    }
  }

  // Test streaming enumeration for FastMarchingImageFilterEnums::Label elements
  const std::set<itk::FastMarchingImageFilterEnums::Label> allLabel{
    itk::FastMarchingImageFilterEnums::Label::FarPoint,
    itk::FastMarchingImageFilterEnums::Label::AlivePoint,
    itk::FastMarchingImageFilterEnums::Label::TrialPoint,
    itk::FastMarchingImageFilterEnums::Label::InitialTrialPoint,
    itk::FastMarchingImageFilterEnums::Label::OutsidePoint
  };
  for (const auto & ee : allLabel)
  {
    std::cout << "STREAMED ENUM VALUE FastMarchingImageFilterEnums::Label: " << ee << std::endl;
  }

  if (passed)
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
