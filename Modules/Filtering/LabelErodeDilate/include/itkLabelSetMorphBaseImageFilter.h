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
#ifndef itkLabelSetMorphBaseImageFilter_h
#define itkLabelSetMorphBaseImageFilter_h

#include "itkNumericTraits.h"
#include "itkImageToImageFilter.h"

namespace itk
{
#if     ITK_VERSION_MAJOR < 4
typedef int ThreadIdType;
typedef int RegionIndexType;
#else
typedef unsigned int RegionIndexType;
#endif
/**
 * \class LabelSetMorphBaseImageFilter
 * \brief Base class for binary morphological erosion of label images.
 *
 * This filter is threaded. This class handles the threading for subclasses.
 *
 * \sa itkLabelSetDilateImageFilter itkLabelSetErodeImageFilter
 *
 * \ingroup LabelErodeDilate
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
**/
template< typename TInputImage, bool doDilate,
          typename TOutputImage = TInputImage >
class ITK_EXPORT LabelSetMorphBaseImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelSetMorphBaseImageFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetMorphBaseImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                         InputImageType;
  typedef TOutputImage                                        OutputImageType;
  typedef typename TInputImage::PixelType                     PixelType;
  typedef typename NumericTraits< PixelType >::FloatType      RealType;
  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename NumericTraits< PixelType >::ScalarRealType ScalarRealType;

  typedef typename OutputImageType::IndexType      OutputIndexType;
  typedef typename OutputImageType::IndexValueType OutputIndexValueType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;
  typedef typename TInputImage::SizeType     InputSizeType;
  typedef typename TOutputImage::SizeType    OutputSizeType;

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray< ScalarRealType, TInputImage::ImageDimension > RadiusType;
  /** Image dimension. */

  typedef typename OutputImageType::RegionType OutputImageRegionType;

  // set all of the scales the same
  void SetRadius(ScalarRealType scale);

  itkSetMacro(Radius, RadiusType);
  itkGetConstReferenceMacro(Radius, RadiusType);

  /**
   * Set/Get whether the scale refers to pixels or world units -
   * default is false
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstReferenceMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  void writeDist(std::string fname);

protected:
  LabelSetMorphBaseImageFilter();
  ~LabelSetMorphBaseImageFilter() override {}

  RegionIndexType SplitRequestedRegion(RegionIndexType i, RegionIndexType num,
                                       OutputImageRegionType & splitRegion) override;

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                    ThreadIdType threadId) override;

  void GenerateData(void) override;

  // Override since the filter produces the entire dataset.
  void EnlargeOutputRequestedRegion(DataObject *output) override;

  bool m_UseImageSpacing;
  void PrintSelf(std::ostream & os, Indent indent) const override;

  RadiusType m_Radius;
  RadiusType m_Scale;
  typedef typename itk::Image< RealType, TInputImage::ImageDimension > DistanceImageType;
  typename TInputImage::PixelType m_Extreme;

  typename DistanceImageType::Pointer m_DistanceImage;

  int  m_MagnitudeSign;
  int  m_CurrentDimension;
  bool m_FirstPassDone;

  // this is the first non-zero entry in the radius. Needed to
  // support elliptical operations
  RealType m_BaseSigma;
private:
  LabelSetMorphBaseImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);               //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelSetMorphBaseImageFilter.hxx"
#endif

#endif
