/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryErodeImageFilter_h
#define __itkBinaryErodeImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/**
 * \class BinaryErodeImageFilter
 * \brief Binary erosion of an image
 *
 * Erode an image using binary morphology. Gray scale images can be
 * processed as binary images by selecting an "ErodeValue".  Pixel values
 * matching the erode value are considered the "foreground" and all other
 * pixels are "background". This is useful in processing segmented images
 * where all pixels in segment #1 have value 1 and pixels in segment #2
 * have value 2, etc. A particular "segment number" can be processed.
 * ErodeValue defaults to the maximum possible value of the PixelType.
 *
 * Binary erosion will set a pixel as the "ErodeValue" if all of the
 * pixels in the image for "on" structuring element pixels have a
 * value of "ErodeValue".  Otherwise, the center pixel is set to an
 * appropriate "background" value. For lack of something better, the
 * background value used will be the minimum of the pixels that were
 * not the ErodeValue. 
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 ("on" values) are candidates for affecting the center
 * pixel.  A reasonable choice of structuring element is 
 * itk::BinaryBallStructuringElement.
 *
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns either the original pixel value
 *     or the ErodeValue.
 *   - Replace the original value with the specified value
 *
 * \sa MorphologyImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryErodeImageFilter : 
  public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef BinaryErodeImageFilter Self;
  typedef MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(BinaryErodeImageFilter, MorphologyImageFilter);
  
  /** Declaration of pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Neighborhood iterator type. */
  typedef ConstNeighborhoodIterator<TInputImage>  NeighborhoodIteratorType ;

  /** Kernel typedef. */
  typedef TKernel KernelType;
  
  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType ;
  
  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ErodeValue, PixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetMacro(ErodeValue, PixelType);
  
protected:
  BinaryErodeImageFilter();
  ~BinaryErodeImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the ErodeValue if all of the image pixels in the
   * neighborhood have the ErodeValue and that pixel's corresponding
   * element in the structuring element is positive. This version
   * of Evaluate is used for non-boundary pixels. */
  PixelType Evaluate(const NeighborhoodIteratorType &nit,
                     const KernelIteratorType kernelBegin,
                     const KernelIteratorType kernelEnd);

  /** Cache some information that can be used to by each thread and
   * each call to Evaluate() */
  virtual void BeforeThreadedGenerateData();

 private:
  BinaryErodeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PixelType m_ErodeValue;
  
  // Cache whether the center pixel of the kernel is on (for
  // optimization).
  bool m_KernelCenterPixelOn;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryErodeImageFilter.txx"
#endif

#endif


