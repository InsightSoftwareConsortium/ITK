/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionOfInterestImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionOfInterestImageFilter_h
#define __itkRegionOfInterestImageFilter_h

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
 * \sa ExtractImageFilter
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT RegionOfInterestImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef RegionOfInterestImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  typedef typename TInputImage::RegionType   RegionType;
  typedef typename TInputImage::IndexType    IndexType;
  typedef typename TInputImage::SizeType     SizeType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType  InputImagePixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
protected:
  RegionOfInterestImageFilter();
  ~RegionOfInterestImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion(DataObject *output);
  
  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation();


  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const RegionType& outputRegionForThread,
                            int threadId );
  
private:
  RegionOfInterestImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  RegionType      m_RegionOfInterest;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionOfInterestImageFilter.txx"
#endif
  
#endif
