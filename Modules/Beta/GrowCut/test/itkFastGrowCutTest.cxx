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

#include "itkFastGrowCut.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMedianImageFilter.h"
#include "itkTestingMacros.h"

namespace
{
class ShowProgress : public itk::Command
{
public:
  itkNewMacro(ShowProgress);

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override
  {
    if (!itk::ProgressEvent().CheckEvent(&event))
    {
      return;
    }
    const auto * processObject = dynamic_cast<const itk::ProcessObject *>(caller);
    if (!processObject)
    {
      return;
    }
    std::cout << " " << processObject->GetProgress();
  }
};
} // namespace

int
itkFastGrowCutTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage seedsImage noisyLabels [medianLabels]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImage = argv[1];
  const char * seedsImage = argv[2];
  const char * noisyLabels = argv[3];

  constexpr unsigned int Dimension = 3;
  using PixelType = short;
  using ImageType = itk::Image<PixelType, Dimension>;
  using LabelType = itk::Image<unsigned char, Dimension>;

  using FGCType = itk::FastGrowCut<ImageType, LabelType>;
  FGCType::Pointer fgcFilter = FGCType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(fgcFilter, FastGrowCut, ImageToImageFilter);

  // Read the images
  ImageType::Pointer image = itk::ReadImage<ImageType>(inputImage);
  LabelType::Pointer seeds = itk::ReadImage<LabelType>(seedsImage);

  ShowProgress::Pointer showProgress = ShowProgress::New();
  fgcFilter->AddObserver(itk::ProgressEvent(), showProgress);

  // Filter the original, possibly noisy image
  fgcFilter->SetInput(image);
  fgcFilter->SetSeedImage(seeds);
  fgcFilter->Update();
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(fgcFilter->GetOutput(), noisyLabels, true));

  if (argc >= 5)
  {
    const char * medianLabels = argv[4];
    // Now median denoise the input and pass that through the filter
    using MedianType = itk::MedianImageFilter<ImageType, ImageType>;
    MedianType::Pointer medianFilter = MedianType::New();
    medianFilter->SetInput(image);
    medianFilter->SetRadius(1);
    medianFilter->Update();

    fgcFilter->SetInput(medianFilter->GetOutput());
    fgcFilter->Update();
    ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(fgcFilter->GetOutput(), medianLabels, true));
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
