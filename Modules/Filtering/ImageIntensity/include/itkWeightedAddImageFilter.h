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
#ifndef itkWeightedAddImageFilter_h
#define itkWeightedAddImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class WeightedAdd2
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2, typename TOutput>
class WeightedAdd2
{
public:
  using AccumulatorType = typename NumericTraits<TInput1>::AccumulateType;
  using RealType = typename NumericTraits<TInput1>::RealType;
  WeightedAdd2()
    : m_Alpha(0.0)
    , m_Beta(0.0)
  {}
  ~WeightedAdd2() = default;
  bool
  operator!=(const WeightedAdd2 & other) const
  {
    if (Math::NotExactlyEquals(m_Alpha, other.m_Alpha))
    {
      return true;
    }
    return false;
  }

  bool
  operator==(const WeightedAdd2 & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    const RealType sum1 = A * m_Alpha;
    const RealType sum2 = B * m_Beta;

    return static_cast<TOutput>(sum1 + sum2);
  }

  void
  SetAlpha(RealType alpha)
  {
    m_Alpha = alpha;
    m_Beta = NumericTraits<RealType>::OneValue() - m_Alpha;
  }

  RealType
  GetAlpha() const
  {
    return m_Alpha;
  }

private:
  RealType m_Alpha;
  RealType m_Beta; // auxiliary var to avoid a subtraction at every pixel
};
} // namespace Functor

/** \class WeightedAddImageFilter
 * \brief Computes a weighted sum of two images pixel-wise.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 1 image must have a valid definition of
 * the operator+ with a pixel type of the image 2. This condition is
 * required because internally this filter will perform the operation
 *
   \code
          pixel_from_image_1 * alpha +  pixel_from_image_2 * (1.0 - alpha)
   \endcode
 *
 * Additionally the type resulting from the sum will be cast to
 * the pixel type of the output image.
 *
 * The total operation over one pixel will be
 *
   \code
    output_pixel = static_cast<OutputPixelType>(
        input1_pixel * alpha + input2_pixel * (1-alpha) )
   \endcode
 *
 * The alpha parameter is set using SetAlpha.
 *
 * \warning No numeric overflow checking is performed in this filter.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class WeightedAddImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WeightedAddImageFilter);

  /** Standard class type aliases. */
  using Self = WeightedAddImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;


  using FunctorType = Functor::
    WeightedAdd2<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RealType = typename FunctorType::RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(WeightedAddImageFilter, BinaryGeneratorImageFilter);

  /** Set the weight for the first operand of the weighted addition */
  void
  SetAlpha(RealType alpha)
  {
    this->GetFunctor().SetAlpha(alpha);
    this->Modified();
  }

  /** Returns the current alpha value setting */
  RealType
  GetAlpha() const
  {
    return this->GetFunctor().GetAlpha();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1HasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage1::PixelType>));
  itkConceptMacro(Input1RealTypeMultiplyCheck,
                  (Concept::MultiplyOperator<typename TInputImage1::PixelType, RealType, RealType>));
  itkConceptMacro(Input2RealTypeMultiplyCheck,
                  (Concept::MultiplyOperator<typename TInputImage2::PixelType, RealType, RealType>));
  // End concept checking
#endif

protected:
  WeightedAddImageFilter() = default;
  ~WeightedAddImageFilter() override = default;

  void
  BeforeThreadedGenerateData() override
  {
    this->SetFunctor(this->GetFunctor());
  }

private:
  itkGetConstReferenceMacro(Functor, FunctorType);
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  FunctorType m_Functor;
};
} // end namespace itk

#endif
