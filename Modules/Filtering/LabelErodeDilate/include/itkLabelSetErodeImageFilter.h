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
#ifndef itkLabelSetErodeImageFilter_h
#define itkLabelSetErodeImageFilter_h

#include "itkLabelSetMorphBaseImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LabelSetErodeImageFilter
 * \brief Class for binary morphological erosion of label images.
 *
 * This filter will separate touching labels. If you don't want this
 * then use a conventional binary erosion to mask the label image.
 * This filter is threaded.
 *
 * \sa itkLabelSetDilateImageFilter
 *
 * \ingroup LabelErodeDilate
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
**/
template< typename TInputImage,
          typename TOutputImage = TInputImage >
class ITK_EXPORT LabelSetErodeImageFilter:
  public LabelSetMorphBaseImageFilter< TInputImage, false, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelSetErodeImageFilter                                         Self;
  typedef LabelSetMorphBaseImageFilter< TInputImage, false, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                             Pointer;
  typedef SmartPointer< const Self >                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetErodeImageFilter, LabelSetMorphBaseImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                         InputImageType;
  typedef TOutputImage                                        OutputImageType;
  typedef typename TInputImage::PixelType                     PixelType;
  typedef typename NumericTraits< PixelType >::FloatType      RealType;
  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename NumericTraits< PixelType >::ScalarRealType ScalarRealType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;
  typedef typename TInputImage::SizeType     InputSizeType;
  typedef typename TOutputImage::SizeType    OutputSizeType;

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray< ScalarRealType, TInputImage::ImageDimension > RadiusType;
  /** Image dimension. */

  typedef typename OutputImageType::RegionType OutputImageRegionType;
  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
protected:
  LabelSetErodeImageFilter(){}
  ~LabelSetErodeImageFilter() override {}

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  // Override since the filter produces the entire dataset.
private:
  LabelSetErodeImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented

  typedef typename Superclass::DistanceImageType DistanceImageType;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelSetErodeImageFilter.hxx"
#endif

#endif
