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
#ifndef itkBinaryNotImageFilter_h
#define itkBinaryNotImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{

/**
 *\class BinaryNotImageFilter
 * \brief Implements the BinaryNot logical operator pixel-wise between two images.
 *
 * This class is parameterized over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The total operation over one pixel will be
 *
 *  output_pixel = static_cast<PixelType>( input1_pixel != input2_pixel )
 *
 * Where "!=" is the equality operator in C++.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/InvertImageUsingBinaryNot,Invert Image Using Binary Not Operation}
 * \endsphinx
 */
namespace Functor
{

template <typename TPixel>
class BinaryNot
{
public:
  BinaryNot() = default;
  ~BinaryNot() = default;
  bool
  operator!=(const BinaryNot &) const
  {
    return false;
  }
  bool
  operator==(const BinaryNot & other) const
  {
    return !(*this != other);
  }
  inline TPixel
  operator()(const TPixel & A) const
  {
    bool a = (A == m_ForegroundValue);
    if (!a)
    {
      return m_ForegroundValue;
    }
    return m_BackgroundValue;
  }

  TPixel m_ForegroundValue;
  TPixel m_BackgroundValue;
};

} // namespace Functor
template <typename TImage>
class BinaryNotImageFilter
  : public UnaryFunctorImageFilter<TImage, TImage, Functor::BinaryNot<typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryNotImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryNotImageFilter;
  using Superclass = UnaryFunctorImageFilter<TImage, TImage, Functor::BinaryNot<typename TImage::PixelType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryNotImageFilter, UnaryFunctorImageFilter);

  using PixelType = typename TImage::PixelType;

  /** Set/Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ForegroundValue, PixelType);
  itkGetConstMacro(ForegroundValue, PixelType);

  /** Set the value used as "background". Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkSetMacro(BackgroundValue, PixelType);

  /** Get the value used as "background". Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkGetConstMacro(BackgroundValue, PixelType);

protected:
  BinaryNotImageFilter()
  {
    m_ForegroundValue = NumericTraits<PixelType>::max();
    m_BackgroundValue = NumericTraits<PixelType>::NonpositiveMin();
  }
  ~BinaryNotImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    using PixelPrintType = typename NumericTraits<PixelType>::PrintType;

    os << indent << "ForegroundValue: " << static_cast<PixelPrintType>(m_ForegroundValue) << std::endl;

    os << indent << "BackgroundValue: " << static_cast<PixelPrintType>(m_BackgroundValue) << std::endl;
  }

  void
  GenerateData() override
  {
    this->GetFunctor().m_ForegroundValue = m_ForegroundValue;
    this->GetFunctor().m_BackgroundValue = m_BackgroundValue;
    Superclass::GenerateData();
  }

private:
  PixelType m_ForegroundValue;
  PixelType m_BackgroundValue;
};

} // end namespace itk


#endif
