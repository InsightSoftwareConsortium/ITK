/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryDilateImageFilter_h
#define __itkBinaryDilateImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/** \class BinaryDilateImageFilter
 * \brief binary dilation of an image
 *
 * Dilate an image using binary morphology. Gray scale images can be
 * processed as binary images by selecting a "DilateValue".  Pixel values
 * matching the dilate value are considered the "foreground" and all other
 * pixels are "background". This is useful in processing segmented images
 * where all pixels in segment #1 have value 1 and pixels in segment #2
 * have value 2, etc. A particular "segment number" can be processed.
 * DilateValue defaults to the maximum possible value of the PixelType.
 *
 * Binary dilation will set a pixel as the "DilateValue" if any of the
 * pixels in the image under the structuring element have a value of
 * "DilateValue" and that structuring element value is greater than 0.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * If none of the pixels under the structuring element have
 * DilateValue, the pixel under the center pixel value of the
 * structuring element is unchanged. If the center pixel is "in" the
 * structuring element (value > 0), then leaving the pixel unchanged
 * is the right things to do since that is most appropriate
 * "background" value for the pixel.  If the center pixel is not part
 * of the structuring element (a rare designation), then leaving the
 * pixel unchanged is not correct in the strict morphological
 * definition (operating on an image with multiple background values
 * is not defined in morphology).  Under these conditions, the center
 * pixel should be set to "a" background value.  However, we do not
 * know which background value to set it to.
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns either the original pixel value
 *     or the DilateValue.
 *   - Replace the original value with the specified value
 *
 * \sa MorphologyImageFilter, BinaryFunctionDilateImageFilter
 * \sa BinaryDilateImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryDilateImageFilter : 
  public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef BinaryDilateImageFilter Self;
  typedef MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method */
  itkNewMacro(Self);  

  /** Runtime information support */
  itkTypeMacro(BinaryDilateImageFilter, MorphologyImageFilter);
  
  /** Declaration of Pixel Type */
  typedef typename Superclass::PixelType PixelType;

  /** Neighborhood iterator type */
  typedef ConstNeighborhoodIterator<TInputImage> 
    NeighborhoodIteratorType ;

  /** Kernel typedef */
  typedef TKernel KernelType;
  
  /** Kernel (structuring element) iterator */
  typedef typename KernelType::ConstIterator KernelIteratorType ;
 
  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(DilateValue, PixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetMacro(DilateValue, PixelType);
  
protected:
  BinaryDilateImageFilter();
  ~BinaryDilateImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the DilateValue if any of the image pixels in the
   * neighborhood have the DilateValue and that pixel's corresponding
   * element in the structuring element is positive. This version
   * of Evaluate is used for non-boundary pixels. */
  PixelType Evaluate(const NeighborhoodIteratorType &nit,
                     const KernelType &kernel);

private:
  BinaryDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PixelType m_DilateValue;
  
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryDilateImageFilter.txx"
#endif

#endif


