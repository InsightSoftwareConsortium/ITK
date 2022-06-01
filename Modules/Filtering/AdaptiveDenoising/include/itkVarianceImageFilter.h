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
#ifndef itkVarianceImageFilter_h
#define itkVarianceImageFilter_h

#include "itkBoxImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class VarianceImageFilter
 * \brief Applies an averaging filter to an image
 *
 * Computes an image where a given pixel is the variance value of the
 * the pixels in a neighborhood about the corresponding input pixel.
 *
 * A variacne filter is one of the family of linear filters.
 *
 * \ingroup AdaptiveDenoising
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT VarianceImageFilter final : public BoxImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VarianceImageFilter);

  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Standard class typedefs. */
  typedef VarianceImageFilter                             Self;
  typedef BoxImageFilter<InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VarianceImageFilter, BoxImageFilter);

  /** Image typedef support. */
  typedef typename InputImageType::PixelType               InputPixelType;
  typedef typename OutputImageType::PixelType              OutputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType InputRealType;

  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename InputImageType::SizeType InputSizeType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  VarianceImageFilter();
  ~VarianceImageFilter() override = default;

  /** VarianceImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa BoxImageFilter::ThreadedGenerateData(),
   *     BoxImageFilter::GenerateData() */
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVarianceImageFilter.hxx"
#endif

#endif
