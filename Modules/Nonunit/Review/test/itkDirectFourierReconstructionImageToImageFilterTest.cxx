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

#include "itkImageFileReader.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkImageFileWriter.h"

#include "itkDirectFourierReconstructionImageToImageFilter.h"
#include "itkTestingMacros.h"

using InternalPixelType = double;
using TestOutputPixelType = short;

using OutputImageType = itk::Image<TestOutputPixelType, 3>;
using InternalImageType = itk::Image<InternalPixelType, 3>;

using ReconstructionFilterType =
  itk::DirectFourierReconstructionImageToImageFilter<InternalImageType, InternalImageType>;

using SmootherType = itk::RecursiveGaussianImageFilter<InternalImageType, InternalImageType>;
using RescalerType = itk::RescaleIntensityImageFilter<InternalImageType, OutputImageType>;
using ROIFilterType = itk::RegionOfInterestImageFilter<OutputImageType, OutputImageType>;
using ReaderType = itk::ImageFileReader<InternalImageType>;
using WriterType = itk::ImageFileWriter<OutputImageType>;


class CommandProgressUpdate : public itk::Command
{
public:
  using Self = CommandProgressUpdate;
  using Superclass = itk::Command;

  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(Self);

protected:
  CommandProgressUpdate() = default;

  using ReconstructionFilterPointer = const ReconstructionFilterType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override
  {
    auto reconstructor = static_cast<ReconstructionFilterPointer>(caller);

    if (!itk::ProgressEvent().CheckEvent(&event))
    {
      return;
    }

    std::cout << static_cast<int>(100 * reconstructor->GetProgress()) << "%" << std::endl;
  }
};


int
itkDirectFourierReconstructionImageToImageFilterTest(int argc, char * argv[])
{

  if (argc != 18)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output r_dir z_dir alpha_dir nz ng fc nb alpha_range x y z sx sy sz sigma" << std::endl;
    return EXIT_FAILURE;
  }

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);


  auto smoother = SmootherType::New();
  smoother->SetInput(reader->GetOutput());
  smoother->SetSigma(std::stod(argv[17]));
  smoother->SetDirection(std::stoi(argv[3]));


  auto reconstruct = ReconstructionFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reconstruct, DirectFourierReconstructionImageToImageFilter, ImageToImageFilter);


  if (std::stod(argv[17]) == 0)
  {
    reconstruct->SetInput(reader->GetOutput());
  }
  else
  {
    reconstruct->SetInput(smoother->GetOutput());
  }

  unsigned short rDirection = std::stoi(argv[3]);
  reconstruct->SetRDirection(rDirection);
  ITK_TEST_SET_GET_VALUE(rDirection, reconstruct->GetRDirection());

  unsigned short zDirection = std::stoi(argv[4]);
  reconstruct->SetZDirection(zDirection);
  ITK_TEST_SET_GET_VALUE(zDirection, reconstruct->GetZDirection());

  unsigned short alphaDirection = std::stoi(argv[5]);
  reconstruct->SetAlphaDirection(alphaDirection);
  ITK_TEST_SET_GET_VALUE(alphaDirection, reconstruct->GetAlphaDirection());

  unsigned short zeroPadding = std::stoi(argv[6]);
  reconstruct->SetZeroPadding(zeroPadding);
  ITK_TEST_SET_GET_VALUE(zeroPadding, reconstruct->GetZeroPadding());

  unsigned short overSampling = std::stoi(argv[7]);
  reconstruct->SetOverSampling(overSampling);
  ITK_TEST_SET_GET_VALUE(overSampling, reconstruct->GetOverSampling());

  auto cutoff = std::stod(argv[8]);
  reconstruct->SetCutoff(cutoff);
  ITK_TEST_SET_GET_VALUE(cutoff, reconstruct->GetCutoff());

  unsigned short radialSplineOrder = std::stoi(argv[9]);
  reconstruct->SetRadialSplineOrder(radialSplineOrder);
  ITK_TEST_SET_GET_VALUE(radialSplineOrder, reconstruct->GetRadialSplineOrder());

  auto alphaRange = std::stod(argv[10]);
  reconstruct->SetAlphaRange(alphaRange);
  ITK_TEST_SET_GET_VALUE(alphaRange, reconstruct->GetAlphaRange());

  auto observer = CommandProgressUpdate::New();
  reconstruct->AddObserver(itk::ProgressEvent(), observer);

  auto rescaler = RescalerType::New();
  rescaler->SetInput(reconstruct->GetOutput());
  rescaler->SetOutputMinimum(itk::NumericTraits<TestOutputPixelType>::min());
  rescaler->SetOutputMaximum(itk::NumericTraits<TestOutputPixelType>::max());


  auto ROIFilter = ROIFilterType::New();
  ROIFilter->SetInput(rescaler->GetOutput());

  ROIFilterType::IndexType start;
  ROIFilterType::SizeType  size;

  start[0] = std::stoi(argv[11]);
  start[1] = std::stoi(argv[12]);
  start[2] = std::stoi(argv[13]);

  size[0] = std::stoi(argv[14]);
  size[1] = std::stoi(argv[15]);
  size[2] = std::stoi(argv[16]);

  ROIFilterType::RegionType requestedRegion;
  requestedRegion.SetIndex(start);
  requestedRegion.SetSize(size);

  ROIFilter->SetRegionOfInterest(requestedRegion);


  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput(ROIFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
