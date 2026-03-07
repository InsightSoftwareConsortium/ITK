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

/**
 * Demonstration of filter dispatching.  The general problem is to provide
 * filters with specialized implementations for each dimension, but in
 * only one class, and without partial specialization.
 */

#include "itkImageToImageFilter.h"
#include "itkGTest.h"
#include <sstream>

namespace
{

template <typename TInputImage, typename TOutputImage>
class ExampleImageFilter : public itk::ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ExampleImageFilter);

  using Self = ExampleImageFilter;
  using Superclass = itk::ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  enum
  {
    ImageDimension = InputImageType::ImageDimension
  };

  void
  Update() override;

  itkNewMacro(Self);

protected:
  ExampleImageFilter() = default;
  ~ExampleImageFilter() override = default;

private:
  struct DispatchBase
  {};
  template <unsigned long V>
  struct Dispatch : public DispatchBase
  {};

  void
       Execute(const DispatchBase &);
  void Execute(Dispatch<2>);
  void Execute(Dispatch<3>);
  void Execute(Dispatch<0>);
};

template <typename TInputImage, typename TOutputImage>
void
ExampleImageFilter<TInputImage, TOutputImage>::Update()
{
  this->Execute(Dispatch<ImageDimension>());
}

template <typename TInputImage, typename TOutputImage>
void
ExampleImageFilter<TInputImage, TOutputImage>::Execute(const DispatchBase &)
{
  std::cout << "General N-d Execute() has been called." << std::endl;
  if ((ImageDimension == 2) || (ImageDimension == 3))
  {
    std::ostringstream err;
    err << "Error: N-d filter implementation called for " << ImageDimension
        << "-d filter, even though specific implementation exists." << std::endl;
    throw std::string(err.str().c_str());
  }
}

template <typename TInputImage, typename TOutputImage>
void
ExampleImageFilter<TInputImage, TOutputImage>::Execute(Dispatch<2>)
{
  std::cout << "2d-specific Execute() has been called." << std::endl;
  if (ImageDimension != 2)
  {
    std::ostringstream err;
    err << "Error: 2-d filter implementation called for " << ImageDimension << "-d filter." << std::endl;
    throw std::string(err.str().c_str());
  }
}

template <typename TInputImage, typename TOutputImage>
void
ExampleImageFilter<TInputImage, TOutputImage>::Execute(Dispatch<3>)
{
  std::cout << "3d-specific Execute() has been called." << std::endl;
  if (ImageDimension != 3)
  {
    std::ostringstream err;
    err << "Error: 3-d filter implementation called for " << ImageDimension << "-d filter." << std::endl;
    throw std::string(err.str().c_str());
  }
}

template <typename TInputImage, typename TOutputImage>
void
ExampleImageFilter<TInputImage, TOutputImage>::Execute(Dispatch<0>)
{
  throw std::string("The 0-Dispatch method should not have been called.");
}

} // namespace


TEST(FilterDispatch, DimensionSpecificDispatch)
{
  using Image2d = itk::Image<float, 2>;
  using Image3d = itk::Image<float, 3>;
  using Image4d = itk::Image<float, 4>;
  using Image5d = itk::Image<float, 5>;

  auto filter2d = ExampleImageFilter<Image2d, Image2d>::New();
  auto filter3d = ExampleImageFilter<Image3d, Image3d>::New();
  auto filter4d = ExampleImageFilter<Image4d, Image4d>::New();
  auto filter5d = ExampleImageFilter<Image5d, Image5d>::New();

  std::cout << "Executing 2-d filter: ";
  EXPECT_NO_THROW(filter2d->Update());

  std::cout << "Executing 3-d filter: ";
  EXPECT_NO_THROW(filter3d->Update());

  std::cout << "Executing 4-d filter: ";
  EXPECT_NO_THROW(filter4d->Update());

  std::cout << "Executing 5-d filter: ";
  EXPECT_NO_THROW(filter5d->Update());
}
