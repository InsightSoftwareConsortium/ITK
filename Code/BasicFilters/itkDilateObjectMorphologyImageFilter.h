/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDilateObjectMorphologyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDilateObjectMorphologyImageFilter_h
#define __itkDilateObjectMorphologyImageFilter_h

#include "itkObjectMorphologyImageFilter.h"

namespace itk {

/** \class DilateObjectMorphologyImageFilter
 * \brief dilation of an object in an image
 *
 * Dilate an image using binary morphology. 
 * Pixel values matching the object value are considered the 
 * "foreground" and all other pixels are "background". This is useful
 * in processing mask images containing only one object.
 *
 * If a pixel's value is equal to the object
 * value and the pixel is adjacent to a non-object valued pixel, then
 * the kernel is centered on the object-value pixel and neighboring 
 * pixels covered by the kernel are assigned the object value.  
 * The structuring element is assumed to be composed of binary values
 * (zero or one). 
 *
 * \sa ObjectMorphologyImageFilter, ErodeObjectMorphologyImageFilter
 * \sa BinaryDilateImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT DilateObjectMorphologyImageFilter : 
    public ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef DilateObjectMorphologyImageFilter Self;
  typedef ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method */
  itkNewMacro(Self);  

  /** Runtime information support */
  itkTypeMacro(DilateObjectMorphologyImageFilter, ObjectMorphologyImageFilter);

  /** duplicates from base class to avoid compiler warnings */
  typedef typename Superclass::PixelType PixelType;

  /** duplicates from base class to avoid compiler warnings */
  typedef TKernel KernelType;

  /** duplicates from base class to avoid compiler warnings */
  typedef typename KernelType::ConstIterator KernelIteratorType;

  /** duplicates from base class to avoid compiler warnings */
  typedef NeighborhoodIterator<TOutputImage>
  OutputNeighborhoodIteratorType ;

protected:
  DilateObjectMorphologyImageFilter();
  ~DilateObjectMorphologyImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Apply the kernel to the neighborhood given.
   *
   * All values in neighborhood covered by the kernel will be set to the
   * object value.  */
  void Evaluate(OutputNeighborhoodIteratorType &nit,
                const KernelType &kernel);

private:
  DilateObjectMorphologyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDilateObjectMorphologyImageFilter.txx"
#endif

#endif


