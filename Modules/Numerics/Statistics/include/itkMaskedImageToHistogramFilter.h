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
#include "itkBarrier.h"
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
class ITK_TEMPLATE_EXPORT MaskedImageToHistogramFilter:public ImageToHistogramFilter<TImage>
{
public:
  /** Standard typedefs */
  typedef MaskedImageToHistogramFilter     Self;
  typedef ImageToHistogramFilter<TImage>   Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskedImageToHistogramFilter, ImageToHistogramFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  typedef TImage                                         ImageType;
  typedef typename ImageType::PixelType                  PixelType;
  typedef typename ImageType::RegionType                 RegionType;
  typedef typename NumericTraits< PixelType >::ValueType ValueType;
  typedef typename NumericTraits< ValueType >::RealType  ValueRealType;

  typedef Histogram< ValueRealType >                    HistogramType;
  typedef typename HistogramType::Pointer               HistogramPointer;
  typedef typename HistogramType::ConstPointer          HistogramConstPointer;
  typedef typename HistogramType::SizeType              HistogramSizeType;
  typedef typename HistogramType::MeasurementType       HistogramMeasurementType;
  typedef typename HistogramType::MeasurementVectorType HistogramMeasurementVectorType;

  typedef TMaskImage                                     MaskImageType;
  typedef typename MaskImageType::PixelType              MaskPixelType;

  /** Method to set/get the mask */
  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  /** Set the pixel value treated as on in the mask.
   * Only pixels with this value will be added to the histogram.
   */
  itkSetGetDecoratedInputMacro(MaskValue, MaskPixelType);

protected:
  MaskedImageToHistogramFilter();
  virtual ~MaskedImageToHistogramFilter() ITK_OVERRIDE {}

  virtual void ThreadedComputeMinimumAndMaximum( const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress ) ITK_OVERRIDE;
  virtual void ThreadedComputeHistogram( const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskedImageToHistogramFilter);

};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedImageToHistogramFilter.hxx"
#endif

#endif
