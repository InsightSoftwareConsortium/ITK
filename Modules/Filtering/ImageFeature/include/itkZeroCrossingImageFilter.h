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
#ifndef itkZeroCrossingImageFilter_h
#define itkZeroCrossingImageFilter_h

#include "itkImageToImageFilter.h"
namespace itk
{
/** \class ZeroCrossingImageFilter
 * \brief This filter finds the closest pixel to the zero-crossings
 * (sign changes) in a signed itk::Image.
 *
 * Pixels closest to zero-crossings are labeled with a foreground value.
 * All other pixels are marked with a background value. The algorithm works
 * by detecting differences in sign among neighbors using city-block style
 * connectivity (4-neighbors in 2d, 6-neighbors in 3d, etc.).
 *
 *  \par Inputs and Outputs
 *  The input to this filter is an itk::Image of arbitrary dimension.  The
 *  algorithm assumes a signed data type (zero-crossings are not defined for
 *  unsigned data types), and requires that operator>, operator<, operator==,
 *  and operator!= are defined.
 *
 *  \par
 *  The output of the filter is a binary, labeled image of user-specified type.
 *  By default, zero-crossing pixels are labeled with a default "foreground"
 *  value of itk::NumericTraits<OutputDataType>::OneValue(), where OutputDataType is
 *  the data type of the output image.  All other pixels are labeled with a
 *  default "background" value of itk::NumericTraits<OutputDataType>::ZeroValue().
 *
 *  \par Parameters
 *  There are two parameters for this filter.  ForegroundValue is the value
 *  that marks zero-crossing pixels.  The BackgroundValue is the value given to
 *  all other pixels.
 *
 *  \sa Image
 *  \sa Neighborhood
 *  \sa NeighborhoodOperator
 *  \sa NeighborhoodIterator
 *  \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 *
 * \wiki
 * \wikiexample{ImageProcessing/ZeroCrossingImageFilter,Find zero crossings in a signed image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ZeroCrossingImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard "Self" & Superclass typedef. */
  typedef ZeroCrossingImageFilter                         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Image typedef support   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** SmartPointer typedef support  */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Define pixel types  */
  typedef typename TInputImage::PixelType  InputImagePixelType;
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroCrossingImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration   */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** ZeroCrossingImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to do comparisons between the central pixel and ite neighbors).
   * Thus ZeroCrossingImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()   */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Set/Get the label value for zero-crossing pixels. */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /** Set/Get the label value for non-zero-crossing pixels. */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< OutputImagePixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputComparableCheck,
                   ( Concept::Comparable< InputImagePixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  ZeroCrossingImageFilter()
  {
    m_ForegroundValue = NumericTraits< OutputImagePixelType >::OneValue();
    m_BackgroundValue = NumericTraits< OutputImagePixelType >::ZeroValue();
  }

  ~ZeroCrossingImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;

  /**
   * ZeroCrossingImageFilter can be implemented as a multithreaded filter.
   * Therefore,this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ZeroCrossingImageFilter);

};
} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroCrossingImageFilter.hxx"
#endif

#endif
