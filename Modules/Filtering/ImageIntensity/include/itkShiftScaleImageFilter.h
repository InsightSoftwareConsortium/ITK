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
#ifndef itkShiftScaleImageFilter_h
#define itkShiftScaleImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkArray.h"

namespace itk
{
/** \class ShiftScaleImageFilter
 * \brief Shift and scale the pixels in an image.
 *
 * ShiftScaleImageFilter shifts the input pixel by Shift (default 0.0)
 * and then scales the pixel by Scale (default 1.0). All computations
 * are performed in the precision of the input pixel's RealType. Before
 * assigning the computed value to the output pixel, the value is clamped
 * at the NonpositiveMin and max of the pixel type.
 * \ingroup IntensityImageFilters
 *
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ShiftScaleImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShiftScaleImageFilter);

  /** Standard class type aliases. */
  using Self = ShiftScaleImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output and input image region types. */
  using InputImageRegionType = typename TInputImage::RegionType;
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Typedef to describe the pointer to the input/output. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Typedef to describe the type of pixel. */
  using InputImagePixelType = typename TInputImage::PixelType;
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /** Typedef to describe the output and input image index and size types. */
  using InputImageIndexType = typename TInputImage::IndexType;
  using InputImageSizeType = typename TInputImage::SizeType;
  using InputImageOffsetType = typename TInputImage::OffsetType;
  using OutputImageIndexType = typename TOutputImage::IndexType;
  using OutputImageSizeType = typename TOutputImage::SizeType;
  using OutputImageOffsetType = typename TOutputImage::OffsetType;

  /** Type to use form computations. */
  using RealType = typename NumericTraits<OutputImagePixelType>::RealType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShiftScaleImageFilter, ImageToImageFilter);

  /** Set/Get the amount to Shift each Pixel. The shift is followed by a Scale.
   */
  itkSetMacro(Shift, RealType);
  itkGetConstMacro(Shift, RealType);

  /** Set/Get the amount to Scale each Pixel. The Scale is applied after the
    Shift. */
  itkSetMacro(Scale, RealType);
  itkGetConstMacro(Scale, RealType);

  /** Get the number of pixels that underflowed and overflowed. */
  itkGetConstMacro(UnderflowCount, long);
  itkGetConstMacro(OverflowCount, long);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputImagePixelType>));
  itkConceptMacro(InputPlusRealTypeCheck, (Concept::AdditiveOperators<InputImagePixelType, RealType, RealType>));
  itkConceptMacro(RealTypeMultiplyOperatorCheck, (Concept::MultiplyOperator<RealType>));
  // End concept checking
#endif

protected:
  ShiftScaleImageFilter();
  ~ShiftScaleImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize some accumulators before the threads run. */
  void
  BeforeThreadedGenerateData() override;

  /** Tally accumulated in threads. */
  void
  AfterThreadedGenerateData() override;

  /** Multi-thread version GenerateData. */
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override
  {
    itkExceptionMacro("This class requires threadId so it must use classic multi-threading model");
  }

private:
  RealType m_Shift;
  RealType m_Scale;

  long m_UnderflowCount;
  long m_OverflowCount;

  Array<long> m_ThreadUnderflow;
  Array<long> m_ThreadOverflow;

  const TInputImage * m_InputImage;
  TOutputImage *      m_OutputImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShiftScaleImageFilter.hxx"
#endif

#endif
