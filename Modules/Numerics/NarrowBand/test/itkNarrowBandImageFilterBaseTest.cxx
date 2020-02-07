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

#include <iostream>
#include "itkImageFileWriter.h"
#include "itkNarrowBandImageFilterBase.h"
#include "itkCurvatureFlowFunction.h"
#include "itkRandomImageSource.h"
#include "itkAddImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

namespace itk
{
template <typename TInputImageType, typename TOutputImageType>
class NbTestClass : public NarrowBandImageFilterBase<TInputImageType, TOutputImageType>
{
public:
  using Self = NbTestClass;

  using Superclass = NarrowBandImageFilterBase<TInputImageType, TOutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ImageType = TOutputImageType;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time class information. */
  itkTypeMacro(NbTestClass, NarrowBandImageFilterBase);

  using FiniteFunctionType = CurvatureFlowFunction<TOutputImageType>;


protected:
  typename FiniteFunctionType::Pointer m_Function;

  NbTestClass()
  {
    m_Function = FiniteFunctionType::New();
    this->SetDifferenceFunction(m_Function);
  }

  bool
  Halt() override
  {
    if (this->GetElapsedIterations() == 20)
    {
      return true;
    }
    else
    {
      return false;
    }
  }

  void
  CreateNarrowBand() override
  {
    // Create a band
    typename ImageType::SizeType   sz = this->GetInput()->GetRequestedRegion().GetSize();
    typename ImageType::IndexType  tl = this->GetInput()->GetRequestedRegion().GetIndex();
    typename Superclass::IndexType in;

    for (in [0] = 32 + tl[0]; in[0] < tl[0] + (long int)(sz[0]); in[0]++)
    {
      for (in [1] = tl[1] + 32; in[1] < tl[1] + (long int)(sz[1]); in[1]++)
      {
        this->InsertNarrowBandNode(in);
      }
    }
  }
};
} // namespace itk

namespace
{
// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance(const TPoint & p)
{
  TPoint center;
  center.Fill(32);
  double radius = 19.5;

  double accum = 0.0;
  for (unsigned int j = 0; j < TPoint::PointDimension; j++)
  {
    accum += itk::Math::sqr(p[j] - center[j]);
  }
  accum = std::sqrt(accum);
  return (accum - radius);
}
} // namespace


int
itkNarrowBandImageFilterBaseTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " OutputImage\n";
    return EXIT_FAILURE;
  }

  using PixelType = float;
  using WriterPixelType = unsigned char;
  constexpr unsigned int ImageDimension = 2;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using WriterImageType = itk::Image<WriterPixelType, ImageDimension>;
  using PointType = itk::Point<double, ImageDimension>;

  ImageType::SizeType   size = { { 64, 64 } };
  ImageType::IndexType  index = { { 0, 0 } };
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->SetRequestedRegion(region);
  inputImage->Allocate();

  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(inputImage, region);
  iter.GoToBegin();

  while (!iter.IsAtEnd())
  {
    PointType point;
    inputImage->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
    iter.Set(SimpleSignedDistance(point));
    ++iter;
  }

  using RandomSourceType = itk::RandomImageSource<ImageType>;
  RandomSourceType::Pointer randomSource = RandomSourceType::New();
  ImageType::SizeValueType  tam[2];
  tam[0] = 64;
  tam[1] = 64;
  randomSource->SetSize(tam);
  randomSource->SetMin(-2);
  randomSource->SetMax(2);
  //  For testing purposes we want random source to produce
  //  deterministic values. This is accompished by restricting the
  //  number of threads to 1.
  randomSource->SetNumberOfWorkUnits(1);

  using AddFilterType = itk::AddImageFilter<ImageType, ImageType, ImageType>;
  AddFilterType::Pointer addFilter = AddFilterType::New();
  addFilter->SetInput1(inputImage);
  addFilter->SetInput2(randomSource->GetOutput());

  using FilterType = itk::NbTestClass<ImageType, ImageType>;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(addFilter->GetOutput());
  filter->Print(std::cout);
  try
  {
    using RescaleType = itk::RescaleIntensityImageFilter<ImageType, WriterImageType>;
    RescaleType::Pointer rescale = RescaleType::New();
    rescale->SetInput(filter->GetOutput());
    rescale->SetOutputMinimum(0);
    rescale->SetOutputMaximum(255);

    using WriterType = itk::ImageFileWriter<WriterImageType>;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(rescale->GetOutput());
    writer->SetFileName(argv[1]);
    writer->Write();
  }
  catch (const itk::ExceptionObject & err)
  {
    (&err)->Print(std::cerr);
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;
}
