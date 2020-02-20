/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkImageFileReader.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkImageFileWriter.h"

#include "itkDirectFourierReconstructionImageToImageFilter.h"

using InternalPixelType = double;
using TestOutputPixelType = short int;

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

    std::cout << (int)(100 * reconstructor->GetProgress()) << "%" << std::endl;
  }
};


int
itkDirectFourierReconstructionImageToImageFilterTest(int argc, char * argv[])
{

  if (argc != 18)
  {
    std::cerr << "Wrong number of input arguments" << std::endl;
    std::cerr << "Usage : " << std::endl << "\t";
    std::cerr << argv[0] << " input output r_dir z_dir alpha_dir nz ng fc nb alpha_range x y z sx sy sz sigma"
              << std::endl;
    return 1;
  }

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);


  SmootherType::Pointer smoother = SmootherType::New();
  smoother->SetInput(reader->GetOutput());
  smoother->SetSigma(std::stod(argv[17]));
  smoother->SetDirection(std::stoi(argv[3]));


  ReconstructionFilterType::Pointer reconstruct = ReconstructionFilterType::New();
  if (std::stod(argv[17]) == 0)
  {
    reconstruct->SetInput(reader->GetOutput());
  }
  else
  {
    reconstruct->SetInput(smoother->GetOutput());
  }
  reconstruct->SetRDirection(std::stoi(argv[3]));
  reconstruct->SetZDirection(std::stoi(argv[4]));
  reconstruct->SetAlphaDirection(std::stoi(argv[5]));
  reconstruct->SetZeroPadding(std::stoi(argv[6]));
  reconstruct->SetOverSampling(std::stoi(argv[7]));
  reconstruct->SetCutoff(std::stod(argv[8]));
  reconstruct->SetRadialSplineOrder(std::stoi(argv[9]));
  reconstruct->SetAlphaRange(std::stoi(argv[10]));

  CommandProgressUpdate::Pointer observer = CommandProgressUpdate::New();
  reconstruct->AddObserver(itk::ProgressEvent(), observer);

  RescalerType::Pointer rescaler = RescalerType::New();
  rescaler->SetInput(reconstruct->GetOutput());
  rescaler->SetOutputMinimum(itk::NumericTraits<TestOutputPixelType>::min());
  rescaler->SetOutputMaximum(itk::NumericTraits<TestOutputPixelType>::max());


  ROIFilterType::Pointer ROIFilter = ROIFilterType::New();
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


  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput(ROIFilter->GetOutput());


  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "An error occurred somewhere:" << std::endl;
    std::cerr << err << std::endl;
    return 2;
  }

  std::cout << "Done" << std::endl;

  std::cout << reconstruct << std::endl;

  return 0;

} // main
