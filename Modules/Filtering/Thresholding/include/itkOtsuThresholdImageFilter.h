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
#ifndef __itkOtsuThresholdImageFilter_h
#define __itkOtsuThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class OtsuThresholdImageFilter
 * \brief Threshold an image using the Otsu Threshold
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using the OtsuThresholdImageCalculator and
 * applies that theshold to the input image using the
 * BinaryThresholdImageFilter. The NunberOfHistogram bins can be set
 * for the Calculator. The InsideValue and OutsideValue can be set
 * for the BinaryThresholdImageFilter.
 *
 * \sa OtsuThresholdImageCalculator
 * \sa BinaryThresholdImageFilter
 * \ingroup IntensityImageFilters  Multithreaded
 * \ingroup ITK-Thresholding
 *
 * \wiki
 * \wikiexample{Segmentation/OtsuThresholdImageFilter,Separate foreground and background using Otsu's method}
 * \endwiki
 */

template< class TInputImage, class TOutputImage >
class ITK_EXPORT OtsuThresholdImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef OtsuThresholdImageFilter                        Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OtsuThresholdImageFilter, ImageToImageFilter);

  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  typedef typename TInputImage::SizeType    InputSizeType;
  typedef typename TInputImage::IndexType   InputIndexType;
  typedef typename TInputImage::RegionType  InputImageRegionType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::Zero. */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue, OutputPixelType);

  /** Set/Get the number of histogram bins. Defaults is 128. */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType, 1,
                    NumericTraits< SizeValueType >::max() );
  itkGetConstMacro(NumberOfHistogramBins, SizeValueType);

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold, InputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< OutputPixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputPixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  /** End concept checking */
#endif
protected:
  OtsuThresholdImageFilter();
  ~OtsuThresholdImageFilter(){}
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateInputRequestedRegion();

  void GenerateData();

private:
  OtsuThresholdImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented

  InputPixelType  m_Threshold;
  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;
  SizeValueType   m_NumberOfHistogramBins;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuThresholdImageFilter.txx"
#endif

#endif
