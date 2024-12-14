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

#include "itkCurvatureFlowImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkVTKImageIO.h"
#include "itkTextOutput.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"


namespace
{
// The following three classes are used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << '\n';
  }
  itk::ProcessObject::Pointer m_Process;
};

} // namespace

namespace itk
{
// Dummy difference function for error testing

template <typename TImageType>
class DummyFunction : public FiniteDifferenceFunction<TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DummyFunction);

  using Self = DummyFunction;
  using Superclass = FiniteDifferenceFunction<TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  using typename Superclass::NeighborhoodType;
  using typename Superclass::FloatOffsetType;
  using typename Superclass::PixelType;
  using typename Superclass::TimeStepType;

  PixelType
  ComputeUpdate(const NeighborhoodType &, void *, const FloatOffsetType &) override
  {
    return 0;
  }

  TimeStepType
  ComputeGlobalTimeStep(void *) const override
  {
    return 0;
  }

  void *
  GetGlobalDataPointer() const override
  {
    return nullptr;
  }

  void
  ReleaseGlobalDataPointer(void *) const override
  {}

protected:
  DummyFunction() = default;
  ~DummyFunction() override = default;
};

} // namespace itk


int
itkCurvatureFlowTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Usage: " << '\n';
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  outputFile" << '\n';
    return EXIT_FAILURE;
  }

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());


  using PixelType = float;
  enum
  {
    ImageDimension = 2
  };
  using ImageType = itk::Image<PixelType, ImageDimension>;

  //------------------------------------------------------------------------

  std::cout << "Test error handling." << '\n';
  using FilterType = itk::CurvatureFlowImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(nullptr);

  bool passed = false;
  try
  {
    std::cout << "Test when input is nullptr." << '\n';
    filter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected error." << '\n';
    std::cout << err << '\n';
    passed = true;
  }

  if (!passed)
  {
    std::cout << "Test failed." << '\n';
    return EXIT_FAILURE;
  }

  // ---------------------------------------------------------------------------

  try
  {
    std::cout << "Test when wrong function type." << '\n';
    using FunctionType = itk::DummyFunction<ImageType>;
    filter = FilterType::New();
    auto                        function = FunctionType::New();
    auto                        dummy = ImageType::New();
    auto                        size = ImageType::SizeType::Filled(3);
    const ImageType::RegionType region(size);
    dummy->SetRegions(region);
    dummy->Allocate();
    dummy->FillBuffer(0.2);

    filter->SetInput(dummy);
    filter->SetNumberOfIterations(2);
    filter->SetDifferenceFunction(function);
    filter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected error." << '\n';
    std::cout << err << '\n';
  }

  //-----------------------------------------------------------------------

  std::cout << "Create input image using RandomImageSource" << '\n';
  using SourceType = itk::RandomImageSource<ImageType>;

  auto source = SourceType::New();

  ImageType::SizeValueType size[ImageDimension] = { 64, 64 };
  source->SetSize(size);
  source->SetMin(0.0);
  source->SetMax(1.0);
  source->Update();


  std::cout << "Run CurvatureFlowImageFiler with progress cout's" << '\n';
  using DenoiserType = itk::CurvatureFlowImageFilter<ImageType, ImageType>;

  auto denoiser = DenoiserType::New();

  denoiser->SetInput(source->GetOutput());
  denoiser->SetTimeStep(0.05);
  denoiser->SetNumberOfIterations(8);

  ShowProgressObject                                    progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  denoiser->AddObserver(itk::ProgressEvent(), command);

  denoiser->Update();

  std::cout << "Run CurvatureFlowImageFilter using streamer" << '\n';
  using CasterType = itk::CastImageFilter<ImageType, ImageType>;
  auto caster = CasterType::New();
  caster->SetInput(denoiser->GetInput());

  auto denoiser2 = DenoiserType::New();
  denoiser2->SetInput(caster->GetOutput());
  denoiser2->SetTimeStep(denoiser->GetTimeStep());
  denoiser2->SetNumberOfIterations(denoiser->GetNumberOfIterations());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamerType::New();
  streamer->SetInput(denoiser2->GetOutput());
  streamer->SetNumberOfStreamDivisions(3);
  streamer->Update();

  std::cout << "Compare stand-alone and streamer outputs" << '\n';
  using IteratorType = itk::ImageRegionIterator<ImageType>;
  IteratorType it1(denoiser->GetOutput(), denoiser->GetOutput()->GetBufferedRegion());
  IteratorType it2(streamer->GetOutput(), streamer->GetOutput()->GetBufferedRegion());

  bool         testPass = true;
  unsigned int failedPixels = 0;
  while (!it1.IsAtEnd())
  {
    if (itk::Math::NotAlmostEquals(it1.Get(), it2.Get()))
    {
      if (failedPixels == 0)
      {
        std::cout << "it1.Get() != it2.Get(): " << it1.Get() << " != " << it2.Get() << '\n';
      }
      failedPixels++;
      testPass = false;
    }
    ++it1;
    ++it2;
  }

  if (!testPass)
  {
    std::cout << "Test failed." << '\n';
    std::cout << "Number of failed pixels: " << failedPixels << '\n';
    return EXIT_FAILURE;
  }

  // Exercise other member functions here
  denoiser->Print(std::cout);

  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto writer = WriterType::New();
  writer->SetInput(streamer->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(vtkIO);
  writer->Write();


  std::cout << "Test passed." << '\n';
  return EXIT_SUCCESS;
}
