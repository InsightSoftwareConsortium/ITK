/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkMaskedImageToHistogramFilter_h
#define itkMaskedImageToHistogramFilter_h

#include "itkHistogram.h"
#include "itkImageToHistogramFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class MaskedImageToHistogramFilter
 *  \brief This class generates an histogram from an image.
 *
 *  The concept of Histogram in ITK is quite generic. It has been designed to
 *  manage multiple components data. This class facilitates the computation of
 *  an histogram from an image. Internally it creates a List that is feed into
 *  the SampleToHistogramFilter.
 *
 * \ingroup ITKStatistics
 */

template< typename TImage, typename TMaskImage >
class ITK_TEMPLATE_EXPORT MaskedImageToHistogramFilter
  : public ImageToHistogramFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskedImageToHistogramFilter);

  /** Standard type alias */
  using Self = MaskedImageToHistogramFilter;
  using Superclass = ImageToHistogramFilter<TImage>;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskedImageToHistogramFilter, ImageToHistogramFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using ValueType = typename NumericTraits< PixelType >::ValueType;
  using ValueRealType = typename NumericTraits< ValueType >::RealType;

  using HistogramType = Histogram< ValueRealType >;
  using HistogramPointer = typename HistogramType::Pointer;
  using HistogramConstPointer = typename HistogramType::ConstPointer;
  using HistogramSizeType = typename HistogramType::SizeType;
  using HistogramMeasurementType = typename HistogramType::MeasurementType;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;

  using MaskImageType = TMaskImage;
  using MaskPixelType = typename MaskImageType::PixelType;

  /** Method to set/get the mask */
  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  /** Set the pixel value treated as on in the mask.
   * Only pixels with this value will be added to the histogram.
   */
  itkSetGetDecoratedInputMacro(MaskValue, MaskPixelType);

protected:
  MaskedImageToHistogramFilter();
  ~MaskedImageToHistogramFilter() override = default;

  void ThreadedStreamedGenerateData( const RegionType & inputRegionForThread ) override;
  void ThreadedComputeMinimumAndMaximum( const RegionType & inputRegionForThread) override;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedImageToHistogramFilter.hxx"
#endif

#endif
