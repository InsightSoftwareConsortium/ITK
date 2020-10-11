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
#ifndef itkConstantPadImageFilter_h
#define itkConstantPadImageFilter_h

#include "itkPadImageFilter.h"

#include "itkMath.h"
#include "itkConstantBoundaryCondition.h"

namespace itk
{
/** \class ConstantPadImageFilter
 * \brief Increase the image size by padding with a constant value.
 *
 * ConstantPadImageFilter changes the output image region.  If the output
 * image region is larger than the input image region, the extra pixels are
 * filled in by a constant value.  The output image region must be specified.
 *
 * \image html PadImageFilter.png "Visual explanation of padding regions."
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilter, MirrorPadImageFilter
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/PadAnImageWithAConstant,Pad An Image With A Constant}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ConstantPadImageFilter : public PadImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConstantPadImageFilter);

  /** Standard class type aliases. */
  using Self = ConstantPadImageFilter;
  using Superclass = PadImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConstantPadImageFilter, PadImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  using OutputImageIndexType = typename Superclass::OutputImageIndexType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using OutputImageSizeType = typename Superclass::OutputImageSizeType;
  using InputImageSizeType = typename Superclass::InputImageSizeType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get the pad value.  Default is Zero. */
  void
  SetConstant(OutputImagePixelType constant)
  {
    if (Math::NotExactlyEquals(constant, m_InternalBoundaryCondition.GetConstant()))
    {
      m_InternalBoundaryCondition.SetConstant(constant);
      this->Modified();
    }
  }
  OutputImagePixelType
  GetConstant() const
  {
    return m_InternalBoundaryCondition.GetConstant();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  ConstantPadImageFilter();
  ~ConstantPadImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ConstantBoundaryCondition<TInputImage, TOutputImage> m_InternalBoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConstantPadImageFilter.hxx"
#endif

#endif
