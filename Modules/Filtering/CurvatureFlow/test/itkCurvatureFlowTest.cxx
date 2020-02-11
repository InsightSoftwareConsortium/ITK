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
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
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
  ITK_DISALLOW_COPY_AND_ASSIGN(DummyFunction);

  using Self = DummyFunction;
  using Superclass = FiniteDifferenceFunction<TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using PixelType = typename Superclass::PixelType;
  using TimeStepType = typename Superclass::TimeStepType;

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
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  outputFile" << std::endl;
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

  std::cout << "Test error handling." << std::endl;
  using FilterType = itk::CurvatureFlowImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(nullptr);

  bool passed = false;
  try
  {
    std::cout << "Test when input is nullptr." << std::endl;
    filter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
    passed = true;
  }

  if (!passed)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // ---------------------------------------------------------------------------

  try
  {
    std::cout << "Test when wrong function type." << std::endl;
    using FunctionType = itk::DummyFunction<ImageType>;
    filter = FilterType::New();
    FunctionType::Pointer function = FunctionType::New();
    ImageType::Pointer    dummy = ImageType::New();
    ImageType::SizeType   size;
    size.Fill(3);
    ImageType::RegionType region(size);
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
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
  }

  //-----------------------------------------------------------------------

  std::cout << "Create input image using RandomImageSource" << std::endl;
  using SourceType = itk::RandomImageSource<ImageType>;

  SourceType::Pointer source = SourceType::New();

  ImageType::SizeValueType size[ImageDimension] = { 64, 64 };
  source->SetSize(size);
  source->SetMin(0.0);
  source->SetMax(1.0);
  source->Update();


  std::cout << "Run CurvatureFlowImageFiler with progress cout's" << std::endl;
  using DenoiserType = itk::CurvatureFlowImageFilter<ImageType, ImageType>;

  DenoiserType::Pointer denoiser = DenoiserType::New();

  denoiser->SetInput(source->GetOutput());
  denoiser->SetTimeStep(0.05);
  denoiser->SetNumberOfIterations(8);

  ShowProgressObject                                    progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  denoiser->AddObserver(itk::ProgressEvent(), command);

  denoiser->Update();

  std::cout << "Run CurvatureFlowImageFilter using streamer" << std::endl;
  using CasterType = itk::CastImageFilter<ImageType, ImageType>;
  CasterType::Pointer caster = CasterType::New();
  caster->SetInput(denoiser->GetInput());

  DenoiserType::Pointer denoiser2 = DenoiserType::New();
  denoiser2->SetInput(caster->GetOutput());
  denoiser2->SetTimeStep(denoiser->GetTimeStep());
  denoiser2->SetNumberOfIterations(denoiser->GetNumberOfIterations());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput(denoiser2->GetOutput());
  streamer->SetNumberOfStreamDivisions(3);
  streamer->Update();

  std::cout << "Compare stand-alone and streamer outputs" << std::endl;
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
        std::cout << "it1.Get() != it2.Get(): " << it1.Get() << " != " << it2.Get() << std::endl;
      }
      failedPixels++;
      testPass = false;
    }
    ++it1;
    ++it2;
  }

  if (!testPass)
  {
    std::cout << "Test failed." << std::endl;
    std::cout << "Number of failed pixels: " << failedPixels << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise other member functions here
  denoiser->Print(std::cout);

  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  using WriterType = itk::ImageFileWriter<ImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(streamer->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(vtkIO);
  writer->Write();


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
