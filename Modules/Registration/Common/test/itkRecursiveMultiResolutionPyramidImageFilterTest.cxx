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
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"

#include <iostream>
#include "itkMath.h"
#include "itkStreamingImageFilter.h"

namespace
{

/**
 * This function defines the test image pattern.
 * The pattern is a 3D gaussian in the middle
 * and some directional pattern on the outside.
 */
double
F(double x, double y, double z)
{
  constexpr double s = 50;
  double           value = 200.0 * std::exp(-(x * x + y * y + z * z) / (s * s));
  x -= 8;
  y += 3;
  z += 0;
  const double r = std::sqrt(x * x + y * y + z * z);
  if (r > 35)
  {
    value = 2 * (itk::Math::abs(x) + 0.8 * itk::Math::abs(y) + 0.5 * itk::Math::abs(z));
  }
  if (r < 4)
  {
    value = 400;
  }

  return value;
}


// The following three classes are used to support callbacks
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
itkRecursiveMultiResolutionPyramidImageFilterTest(int argc, char * argv[])
{
  // Create a simple image

  // Allocate Images
  using PixelType = short;
  using InputImageType = itk::Image<PixelType, 3>;
  using OutputImageType = itk::Image<float, 3>;
  enum
  {
    ImageDimension = InputImageType::ImageDimension
  };
  bool useShrinkFilter(false);
  if (argc > 1)
  {
    const std::string s(argv[1]);
    if (s == "Shrink")
    {
      useShrinkFilter = true;
      std::cout << "true";
    }
    else
    {
      std::cout << "false";
    }
    std::cout << std::endl;
  }

  InputImageType::SizeType            size = { { 100, 100, 40 } };
  constexpr InputImageType::IndexType index = { { 0, 0, 0 } };
  const InputImageType::RegionType    region{ index, size };

  auto imgTarget = InputImageType::New();
  imgTarget->SetRegions(region);
  imgTarget->Allocate();

  // Fill images with a 3D gaussian with some directional pattern
  // in the background
  using Iterator = itk::ImageRegionIterator<InputImageType>;

  itk::Point<double, 3> center;
  center[0] = static_cast<double>(region.GetSize()[0]) / 2.0;
  center[1] = static_cast<double>(region.GetSize()[1]) / 2.0;
  center[2] = static_cast<double>(region.GetSize()[2]) / 2.0;


  Iterator ti(imgTarget, region);
  while (!ti.IsAtEnd())
  {
    itk::Point<double, 3> p;
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    p[2] = ti.GetIndex()[2];
    itk::Vector<double, 3> d = p - center;
    const double           x = d[0];
    const double           y = d[1];
    const double           z = d[2];
    ti.Set((PixelType)F(x, y, z));
    ++ti;
  }

  // set image origin to be center of the image
  double transCenter[3];
  for (unsigned int j = 0; j < 3; ++j)
  {
    transCenter[j] = -0.5 * static_cast<double>(size[j]);
  }

  imgTarget->SetOrigin(transCenter);


  // Setup a multi-resolution pyramid
  using PyramidType = itk::RecursiveMultiResolutionPyramidImageFilter<InputImageType, OutputImageType>;
  using ScheduleType = PyramidType::ScheduleType;
  auto pyramid = PyramidType::New();
  pyramid->SetUseShrinkImageFilter(useShrinkFilter);

  pyramid->SetInput(imgTarget);

  bool pass = true;

  // set schedule by specifying the number of levels;
  unsigned int numLevels = 3;
  auto         factors = itk::MakeFilled<itk::Vector<unsigned int, ImageDimension>>(1 << (numLevels - 1));
  pyramid->SetNumberOfLevels(numLevels);

  // check the schedule
  ScheduleType schedule(numLevels, ImageDimension);

  for (unsigned int k = 0; k < numLevels; ++k)
  {
    const unsigned int denominator = 1 << k;
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      schedule[k][j] = factors[j] / denominator;
      if (schedule[k][j] == 0)
      {
        schedule[k][j] = 1;
      }
    }
  }

  if (schedule != pyramid->GetSchedule())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetSchedule" << std::endl;
    std::cerr << "Expected value " << schedule << std::endl;
    std::cerr << " differs from " << pyramid->GetSchedule() << std::endl;
    pass = false;
  }

  // set schedule by specifying the starting shrink factors
  numLevels = 4;
  factors[0] = 8;
  factors[1] = 4;
  factors[2] = 2;
  pyramid->SetNumberOfLevels(numLevels);
  pyramid->SetStartingShrinkFactors(factors.Begin());

  // check the schedule;
  schedule = ScheduleType(numLevels, ImageDimension);
  for (unsigned int k = 0; k < numLevels; ++k)
  {
    const unsigned int denominator = 1 << k;
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      schedule[k][j] = factors[j] / denominator;
      if (schedule[k][j] == 0)
      {
        schedule[k][j] = 1;
      }
    }
  }

  if (schedule != pyramid->GetSchedule())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetSchedule" << std::endl;
    std::cerr << "Expected value " << schedule << std::endl;
    std::cerr << " differs from " << pyramid->GetSchedule() << std::endl;
    pass = false;
  }

  // test start factors
  const unsigned int * ss = pyramid->GetStartingShrinkFactors();
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    if (ss[j] != factors[j])
    {
      pass = false;
      std::cout << "Returned starting factors incorrect" << std::endl;
      break;
    }
  }

  // test divisibility
  if (!PyramidType::IsScheduleDownwardDivisible(pyramid->GetSchedule()))
  {
    pass = false;
    std::cout << "Schedule should be downward divisible" << std::endl;
  }

  // generate output at a level with progress
  std::cout << "Run RecursiveMultiResolutionPyramidImageFilter in standalone mode with progress";
  std::cout << std::endl;

  ShowProgressObject                                          progressWatch(pyramid);
  const itk::SimpleMemberCommand<ShowProgressObject>::Pointer command =
    itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  pyramid->AddObserver(itk::ProgressEvent(), command);

  pyramid->Print(std::cout);

  //  update pyramid at a particular level
  unsigned int testLevel = 2;
  pyramid->GetOutput(testLevel)->Update();

  // test output at another level
  testLevel = 2;

  // check the output image information
  InputImageType::SizeType            inputSize = pyramid->GetInput()->GetLargestPossibleRegion().GetSize();
  const InputImageType::SpacingType & inputSpacing = pyramid->GetInput()->GetSpacing();

  OutputImageType::SizeType            outputSize = pyramid->GetOutput(testLevel)->GetLargestPossibleRegion().GetSize();
  const OutputImageType::SpacingType & outputSpacing = pyramid->GetOutput(testLevel)->GetSpacing();
  {
    unsigned int j = 0;
    for (; j < ImageDimension; ++j)
    {
      if (itk::Math::NotAlmostEquals(outputSpacing[j], inputSpacing[j] * static_cast<double>(schedule[testLevel][j])))
      {
        break;
      }
      unsigned int sz = inputSize[j] / schedule[testLevel][j];
      if (sz == 0)
      {
        sz = 1;
      }
      if (outputSize[j] != sz)
      {
        break;
      }
    }

    if (j != ImageDimension)
    {
      pass = false;
      pyramid->GetInput()->Print(std::cout);
      pyramid->GetOutput(testLevel)->Print(std::cout);
    }
  }


  // run in streamed mode
  std::cout << "Run ImagePyramid with streamer" << std::endl;

  using CasterType = itk::CastImageFilter<InputImageType, InputImageType>;
  auto caster = CasterType::New();

  caster->SetInput(pyramid->GetInput());

  auto pyramid2 = PyramidType::New();
  pyramid2->SetInput(caster->GetOutput());
  pyramid2->SetUseShrinkImageFilter(useShrinkFilter);
  pyramid2->SetNumberOfLevels(pyramid->GetNumberOfLevels());
  pyramid2->SetSchedule(pyramid->GetSchedule());

  using StreamerType = itk::StreamingImageFilter<OutputImageType, OutputImageType>;
  auto streamer = StreamerType::New();
  streamer->SetInput(pyramid2->GetOutput(testLevel));
  streamer->Update();

  std::cout << "Compare standalone and streamed outputs" << std::endl;
  using OutputIterator = itk::ImageRegionIterator<OutputImageType>;
  OutputIterator iter1(pyramid->GetOutput(testLevel), pyramid->GetOutput(testLevel)->GetBufferedRegion());
  OutputIterator iter2(streamer->GetOutput(), streamer->GetOutput()->GetBufferedRegion());

  while (!iter1.IsAtEnd())
  {
    if (!itk::Math::FloatAlmostEqual(iter1.Get(), iter2.Get(), 2))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in streamed output at index [" << iter1.GetIndex() << "]" << std::endl;
      std::cerr << "Expected value " << iter1.Get() << std::endl;
      std::cerr << " differs from " << iter2.Get() << std::endl;
      pass = false;
      // break;
    }
    ++iter1;
    ++iter2;
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Set schedule to all ones and update
  schedule = pyramid->GetSchedule();
  schedule.Fill(1);
  pyramid->SetSchedule(schedule);
  pyramid->Update();


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
