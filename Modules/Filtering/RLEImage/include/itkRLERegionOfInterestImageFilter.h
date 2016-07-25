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
#ifndef itkRLERegionOfInterestImageFilter_h
#define itkRLERegionOfInterestImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkRLEImage.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{
/** \class RegionOfInterestImageFilter
 * \brief Extract a region of interest from the input image or convert
 *  between itk::Image and itk::RLEImage (a custom region can be used).
 *
 *  This filter produces an output image of the same dimension as the input
 *  image. The user specifies the region of the input image that will be
 *  contained in the output image. The origin coordinates of the output images
 *  will be computed in such a way that if mapped to physical space, the output
 *  image will overlay the input image with perfect registration. In other
 *  words, a registration process between the output image and the input image
 *  will return an identity transform.
 *
 *  The region to extract is set using the method SetRegionOfInterest.
 *
 * Specialized for RLEImage.
 *
 *  \ingroup RLEImage
 *  \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                                  RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageToImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                              RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  /** Standard class typedefs. */
  typedef RegionOfInterestImageFilter                    Self;
  typedef RLEImage<TPixel, VImageDimension, CounterType> RLEImageType;
  typedef RLEImageType                                   ImageType;
  typedef ImageToImageFilter<RLEImageType, RLEImageType> Superclass;
  typedef SmartPointer<Self>                             Pointer;
  typedef SmartPointer<const Self>                       ConstPointer;
  typedef typename Superclass::InputImageRegionType      InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  typedef typename RLEImageType::RegionType RegionType;
  typedef typename RLEImageType::IndexType  IndexType;
  typedef typename RLEImageType::SizeType   SizeType;

  /** Typedef to describe the type of pixel. */
  typedef typename RLEImageType::PixelType OutputImagePixelType;
  typedef typename RLEImageType::PixelType InputImagePixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, VImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() {}
  ~RegionOfInterestImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  virtual void
  GenerateInputRequestedRegion();

  virtual void
  EnlargeOutputRequestedRegion(DataObject * output);

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void
  GenerateOutputInformation();

  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId);

private:
  RegionOfInterestImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  RegionType m_RegionOfInterest;
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageToImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  /** Standard class typedefs. */
  typedef itk::RLEImage<TPixel, VImageDimension, CounterType> RLEImageType;

  typedef RegionOfInterestImageFilter                 Self;
  typedef Image<TPixel, VImageDimension>              ImageType;
  typedef ImageToImageFilter<ImageType, RLEImageType> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  typedef typename RLEImageType::RegionType RegionType;
  typedef typename RLEImageType::IndexType  IndexType;
  typedef typename RLEImageType::SizeType   SizeType;

  /** Typedef to describe the type of pixel. */
  typedef typename RLEImageType::PixelType OutputImagePixelType;
  typedef typename RLEImageType::PixelType InputImagePixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, VImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() {}
  ~RegionOfInterestImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  virtual void
  GenerateInputRequestedRegion();

  virtual void
  EnlargeOutputRequestedRegion(DataObject * output);

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void
  GenerateOutputInformation();

  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId);

private:
  RegionOfInterestImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  RegionType m_RegionOfInterest;
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>
  : public ImageToImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>
{
public:
  /** Standard class typedefs. */
  typedef itk::RLEImage<TPixel, VImageDimension, CounterType> RLEImageType;

  typedef RegionOfInterestImageFilter                 Self;
  typedef Image<TPixel, VImageDimension>              ImageType;
  typedef ImageToImageFilter<RLEImageType, ImageType> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  typedef typename RLEImageType::RegionType RegionType;
  typedef typename RLEImageType::IndexType  IndexType;
  typedef typename RLEImageType::SizeType   SizeType;

  /** Typedef to describe the type of pixel. */
  typedef typename RLEImageType::PixelType OutputImagePixelType;
  typedef typename RLEImageType::PixelType InputImagePixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, VImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() {}
  ~RegionOfInterestImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  virtual void
  GenerateInputRequestedRegion();

  virtual void
  EnlargeOutputRequestedRegion(DataObject * output);

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void
  GenerateOutputInformation();

  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId);

private:
  RegionOfInterestImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  RegionType m_RegionOfInterest;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRLERegionOfInterestImageFilter.hxx"
#endif

#endif // itkRLERegionOfInterestImageFilter_h
