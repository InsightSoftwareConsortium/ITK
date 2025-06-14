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

#include "itkExtensionVelocitiesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"


// For debugging
#include "itkImageFileWriter.h"

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

// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance(const TPoint & p)
{
  auto             center = itk::MakeFilled<TPoint>(50);
  constexpr double radius = 19.5;

  double accum = 0.0;
  for (unsigned int j = 0; j < TPoint::PointDimension; ++j)
  {
    accum += itk::Math::sqr(p[j] - center[j]);
  }
  accum = std::sqrt(accum);
  return (accum - radius);
}

// simple velocity function to be extended
template <typename TPoint>
double
SimpleVelocity(const TPoint & p)
{
  auto center = itk::MakeFilled<TPoint>(50);

  double       value = 0.0;
  const double x = p[0] - center[0];
  const double y = p[1] - center[1];

  if (y == 0.0)
  {
    if (x >= 0.0)
    {
      value = 0.0;
    }
    else
    {
      value = itk::Math::pi;
    }
  }
  else if (x == 0.0)
  {
    if (y > 0.0)
    {
      value = itk::Math::pi_over_2;
    }
    else // if ( y < 0.0 )
    {
      value = itk::Math::pi + itk::Math::pi_over_2;
    }
  }
  else
  {
    value = std::atan(y / x);
    if (value < 0.0)
    {
      value += itk::Math::pi;
    }

    if (y <= 0.0)
    {
      value += itk::Math::pi;
    }
  }

  return (10 * std::sin(value));
}

} // namespace

int
itkExtensionVelocitiesImageFilterTest(int, char *[])
{

  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using IndexType = ImageType::IndexType;
  using PointType = itk::Point<double, ImageDimension>;

  // Fill an input image with simple signed distance function
  auto                        image = ImageType::New();
  auto                        size = ImageType::SizeType::Filled(128);
  const ImageType::RegionType region(size);

  image->SetRegions(region);
  image->Allocate();

  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(image, region);
  iter.GoToBegin();

  while (!iter.IsAtEnd())
  {
    PointType point;
    image->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
    iter.Set(SimpleSignedDistance(point));
    ++iter;
  }

  // Squash up the level sets by multiplying with a scalar
  using MultiplierType = itk::ShiftScaleImageFilter<ImageType, ImageType>;
  auto multiplier = MultiplierType::New();
  multiplier->SetInput(image);
  multiplier->SetScale(0.5);

  // Set up auxiliary variables
  auto aux1 = ImageType::New();
  aux1->SetRegions(region);
  aux1->Allocate();

  aux1->FillBuffer(1.0);

  auto aux2 = ImageType::New();
  aux2->SetRegions(region);
  aux2->Allocate();

  iter = Iterator(aux2, region);
  iter.GoToBegin();

  while (!iter.IsAtEnd())
  {
    PointType point;
    aux2->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
    iter.Set(SimpleVelocity(point));
    ++iter;
  }
  /*
    {
    // For debugging
    using WriterType = itk::ImageFileWriter<ImageType>;
    auto writer = WriterType::New();
    writer->SetInput( aux2 );
    writer->SetFileName( "input.mhd" );
    writer->Write();
    }
  */

  // Set up reinitialize level set image filter
  constexpr unsigned int AuxDimension = 2;
  using ReinitializerType = itk::ExtensionVelocitiesImageFilter<ImageType, float, AuxDimension>;
  auto reinitializer = ReinitializerType::New();
  reinitializer->SetInput(multiplier->GetOutput());
  reinitializer->SetInputVelocityImage(aux1, 0);
  reinitializer->SetInputVelocityImage(aux2, 1);

  ShowProgressObject                                          progressWatch(reinitializer);
  const itk::SimpleMemberCommand<ShowProgressObject>::Pointer command =
    itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  reinitializer->AddObserver(itk::ProgressEvent(), command);

  /*
    {
    // For debugging
    using WriterType = itk::ImageFileWriter<ImageType>;
    auto writer = WriterType::New();
    writer->SetInput( reinitializer->GetOutputVelocityImage( 1 ) );
    writer->SetFileName( "output.mhd" );
    writer->Write();
    }
  */

  // Check the output
  using DifferenceType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto difference = DifferenceType::New();
  difference->SetTestInput(aux2);
  difference->SetValidInput(reinitializer->GetOutputVelocityImage(1));
  difference->Update();

  // mask out the peak at near the center point
  auto                        centerIndex = ImageType::IndexType::Filled(50 - 8);
  auto                        centerSize = ImageType::SizeType::Filled(17);
  const ImageType::RegionType centerRegion{ centerIndex, centerSize };

  iter = Iterator(difference->GetOutput(), centerRegion);
  iter.GoToBegin();

  while (!iter.IsAtEnd())
  {
    iter.Set(0.0);
    ++iter;
  }

  using CalculatorType = itk::MinimumMaximumImageCalculator<ImageType>;
  auto calculator = CalculatorType::New();
  calculator->SetImage(difference->GetOutput());
  calculator->Compute();

  const double    maxAbsDifference = calculator->GetMaximum();
  const IndexType maxAbsDifferenceIndex = calculator->GetIndexOfMaximum();

  std::cout << "Max. abs. difference = " << maxAbsDifference;
  std::cout << " at " << maxAbsDifferenceIndex << std::endl;

  if (maxAbsDifference > 0.6)
  {
    std::cout << "Difference above threshold of 0.6" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise other member functions
  reinitializer->Print(std::cout);
  reinitializer->SetLevelSetValue(1.0);

  // Exercise the narrowband version
  reinitializer->SetLevelSetValue(0.0);
  reinitializer->NarrowBandingOn();
  reinitializer->SetNarrowBandwidth(8);
  reinitializer->Update();

  // We will use the output narrowband from the last run as the input narrowband
  reinitializer->SetInputNarrowBand(reinitializer->GetOutputNarrowBand());
  reinitializer->Update();

  // Check the output by iterating through the output narrowband
  using NodeContainerPointer = ReinitializerType::NodeContainerPointer;
  using NodeContainerType = ReinitializerType::NodeContainer;
  using ContainerIterator = NodeContainerType::ConstIterator;

  const NodeContainerPointer nodes = reinitializer->GetOutputNarrowBand();
  ContainerIterator          nodeIter = nodes->Begin();
  const ContainerIterator    nodeEnd = nodes->End();

  while (nodeIter != nodeEnd)
  {
    const ImageType::IndexType nodeIndex = nodeIter.Value().GetIndex();
    const double               absDiff =
      itk::Math::abs(aux2->GetPixel(nodeIndex) - reinitializer->GetOutputVelocityImage(1)->GetPixel(nodeIndex));
    if (absDiff > 0.6)
    {
      std::cout << "Abs diff: " << absDiff;
      std::cout << " at: " << nodeIndex << std::endl;
      std::cout << "Difference above threshold of 0.6" << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
    }
    nodeIter++;
  }

  // Test setting/getting velocity beyond index
  reinitializer->SetInputVelocityImage(aux1, 2);

  if (reinitializer->GetInputVelocityImage(2))
  {
    std::cout << "GetInputVelocityImage(2) should have returned nullptr" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  if (reinitializer->GetOutputVelocityImage(2))
  {
    std::cout << "GetOutputVelocityImage(2) should have returned nullptr" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
