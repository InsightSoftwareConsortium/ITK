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
#ifndef itkInvertIntensityImageFilter_h
#define itkInvertIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class InvertIntensityTransform
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT InvertIntensityTransform
{
public:
  using RealType = typename NumericTraits<TInput>::RealType;
  InvertIntensityTransform() { m_Maximum = NumericTraits<TInput>::max(); }
  ~InvertIntensityTransform() = default;

  void
  SetMaximum(TOutput max)
  {
    m_Maximum = max;
  }

  bool
  operator!=(const InvertIntensityTransform & other) const
  {
    if (m_Maximum != other.m_Maximum)
    {
      return true;
    }
    return false;
  }

  bool
  operator==(const InvertIntensityTransform & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & x) const
  {
    auto result = static_cast<TOutput>(m_Maximum - x);

    return result;
  }

private:
  TInput m_Maximum;
};
} // end namespace Functor

/** \class InvertIntensityImageFilter
 * \brief Invert the intensity of an image.
 *
 * InvertIntensityImageFilter inverts intensity of pixels by
 * subtracting pixel value to a maximum value. The maximum value can
 * be set with SetMaximum and defaults the maximum of input pixel
 * type. This filter can be used to invert, for example, a binary
 * image, a distance map, etc.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa IntensityWindowingImageFilter ShiftScaleImageFilter
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/InvertImage,Invert Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT InvertIntensityImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::InvertIntensityTransform<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(InvertIntensityImageFilter);

  /** Standard class type aliases. */
  using Self = InvertIntensityImageFilter;
  using Superclass = UnaryFunctorImageFilter<
    TInputImage,
    TOutputImage,
    Functor::InvertIntensityTransform<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<InputPixelType>::RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(InvertIntensityImageFilter, UnaryFunctorImageFilter);

  /** Set/Get the maximum intensity value for the inversion. */
  itkSetMacro(Maximum, InputPixelType);
  itkGetConstReferenceMacro(Maximum, InputPixelType);

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Process to execute before entering the multithreaded section */
  void
  BeforeThreadedGenerateData() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  InvertIntensityImageFilter();
  ~InvertIntensityImageFilter() override = default;

private:
  InputPixelType m_Maximum;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInvertIntensityImageFilter.hxx"
#endif

#endif
