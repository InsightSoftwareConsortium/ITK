/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFunctionDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleFunctionDilateImageFilter_h
#define __itkGrayscaleFunctionDilateImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/** \class GrayscaleFunctionDilateImageFilter
 * \brief gray scale function dilation of an image
 *
 * Dilate an image using functional grayscale morphology. Function
 * dilation takes the maximum of all the pixels identified by the
 * structuring element plus the structuring element value.
 *
 * The structuring element can be composed of arbitrary nonnegative
 * values (not restricted to zero or one). Element values greater than
 * zero indicate pixels that will be considered during the dilation.
 * The function dilation operation is defined as the maxixum over the
 * element of the image value PLUS the structuring element value.
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns the maximum value among 
 *     the image neighbors plus the kernel value where the kernel has
 *     elements > 0.
 *   - Replace the original value with the max value
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT GrayscaleFunctionDilateImageFilter : 
    public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleFunctionDilateImageFilter Self;
  typedef MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleFunctionDilateImageFilter, 
               MorphologyImageFilter);
  
  /** Declaration of pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Kernel (structuring element) iterator. */
  typedef typename Superclass::KernelIteratorType  KernelIteratorType;
 
  /** Neighborhood iterator type. */
  typedef typename Superclass::NeighborhoodIteratorType NeighborhoodIteratorType ;

  /** Kernel typedef. */
  typedef typename Superclass::KernelType KernelType;

protected:
  GrayscaleFunctionDilateImageFilter() {};
  ~GrayscaleFunctionDilateImageFilter() {};

  /** Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the maximum value of the image pixels plus the
   * structuring element values whose corresponding element in the
   * structuring element is positive. This version of Evaluate is used
   * for non-boundary pixels. */
  PixelType Evaluate(const NeighborhoodIteratorType &nit,
                     const KernelIteratorType kernelBegin,
                     const KernelIteratorType kernelEnd);

private:
  GrayscaleFunctionDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleFunctionDilateImageFilter.txx"
#endif

#endif


