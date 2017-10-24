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
#ifndef itkSimpleContourExtractorImageFilter_h
#define itkSimpleContourExtractorImageFilter_h

#include "itkBoxImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class SimpleContourExtractorImageFilter
* \brief Computes an image of contours which will be the contour
* of the first image.
*
* A pixel of the source image is considered to belong to the contour
* if its pixel value is equal to the input foreground value and it
* has in its neighborhood at least one pixel which its pixel value is
* equal to the input background value. The output image will have
* pixels which will be set to the output foreground value if they
* belong to the contour, otherwise they will be set to the output
* background value.
*
* The neighborhood "radius" is set thanks to the radius params.
*
* \sa Image
* \sa Neighborhood
* \sa NeighborhoodOperator
* \sa NeighborhoodIterator
*
* \ingroup IntensityImageFilters
  * \ingroup ITKImageFeature
*/
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT SimpleContourExtractorImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Standard class typedefs. */
  typedef SimpleContourExtractorImageFilter                 Self;
  typedef BoxImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleContourExtractorImageFilter, BoxImageFilter);

  /** Image typedef support. */
  typedef typename InputImageType::PixelType                 InputPixelType;
  typedef typename OutputImageType::PixelType                OutputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType InputRealType;

  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename InputImageType::SizeType InputSizeType;

  /** Set the foreground value used in order to identify a foreground
   * pixel in the input image. */
  itkSetMacro(InputForegroundValue, InputPixelType);

  /** Get the foreground value used in order to identify a foreground
   * pixel in the input image. */
  itkGetConstReferenceMacro(InputForegroundValue, InputPixelType);

  /** Set the background value used in order to identify a background
   * pixel in the input image. */
  itkSetMacro(InputBackgroundValue, InputPixelType);

  /** Get the background value used in order to identify a background
   * pixel in the input image. */
  itkGetConstReferenceMacro(InputBackgroundValue, InputPixelType);

  /** Set the foreground value used in order to identify a foreground
   * pixel in the output image. */
  itkSetMacro(OutputForegroundValue, OutputPixelType);

  /** Get the foreground value used in order to identify a foreground
   * pixel in the output image. */
  itkGetConstReferenceMacro(OutputForegroundValue, OutputPixelType);

  /** Set the background value used in order to identify a background
   * pixel in the output image. */
  itkSetMacro(OutputBackgroundValue, OutputPixelType);

  /** Get the background value used in order to identify a background
   * pixel in the output image. */
  itkGetConstReferenceMacro(OutputBackgroundValue, OutputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  SimpleContourExtractorImageFilter();
  virtual ~SimpleContourExtractorImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** SimpleContourExtractorImageFilter can be implemented as a
   *  multithreaded filter. Therefore, this implementation provides a
   *  ThreadedGenerateData() routine which is called for each
   *  processing thread. The output image data is allocated
   *  automatically by the superclass prior to calling
   *  ThreadedGenerateData(). ThreadedGenerateData can only write to
   *  the portion of the output image specified by the parameter
   *  "outputRegionForThread"
   *
   *  \sa ImageToImageFilter::ThreadedGenerateData(),
   *      ImageToImageFilter::GenerateData()
   *
   */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimpleContourExtractorImageFilter);

  InputPixelType  m_InputForegroundValue;
  InputPixelType  m_InputBackgroundValue;
  OutputPixelType m_OutputForegroundValue;
  OutputPixelType m_OutputBackgroundValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleContourExtractorImageFilter.hxx"
#endif

#endif
