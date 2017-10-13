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
#ifndef itkRegionOfInterestImageFilter_h
#define itkRegionOfInterestImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{
/** \class RegionOfInterestImageFilter
 * \brief Extract a region of interest from the input image.
 *
 *  This filter produces an output image of the same dimension as the input
 *  image. The user specifies the region of the input image that will be
 *  contained in the output image. The origin coordinates of the output images
 *  will be computed in such a way that if mapped to physical space, the output
 *  image will overlay the input image with perfect registration. In other
 *  words, a registration process between the output image and the input image
 *  will return an identity transform.
 *
 *  If you are interested in changing the dimension of the image, you may want
 *  to consider the ExtractImageFilter. For example for extracting a 2D image
 *  from a slice of a 3D image.
 *
 *  The region to extract is set using the method SetRegionOfInterest.
 *
 * \sa ExtractImageFilter
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \wiki
 * \wikiexample{ImageProcessing/RegionOfInterestImageFilter,Extract a portion of an image (region of interest)}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT RegionOfInterestImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RegionOfInterestImageFilter                     Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef typename Superclass::InputImageRegionType       InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::SizeType   SizeType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType  InputImagePixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputImagePixelType, OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  RegionOfInterestImageFilter();
  ~RegionOfInterestImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image. As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData(). ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread".
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const RegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionOfInterestImageFilter);

  RegionType m_RegionOfInterest;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionOfInterestImageFilter.hxx"
#endif

#endif
