/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkErodeObjectMorphologyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkErodeObjectMorphologyImageFilter_h
#define __itkErodeObjectMorphologyImageFilter_h

#include "itkObjectMorphologyImageFilter.h"

namespace itk {

/** \class ErodeObjectMorphologyImageFilter
 * \brief Erosion of an object in an image
 *
 * Erosion of an image using binary morphology. 
 * Pixel values matching the object value are considered the 
 * "object" and all other pixels are "background". This is useful
 * in processing mask images containing only one object.
 *
 * If the pixel covered by the center of the kernel has the pixel value
 * ObjectValue and the pixel is adjacent to a non-object valued pixel, then
 * the kernel is centered on the object-value pixel and neighboring 
 * pixels covered by the kernel are assigned the background value.  
 * The structuring element is assumed to be composed of binary values
 * (zero or one). 
 *
 * \sa ObjectMorphologyImageFilter, BinaryFunctionErodeImageFilter
 * \sa BinaryErodeImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT ErodeObjectMorphologyImageFilter : 
    public ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /** Standard class typedefs. */
  typedef ErodeObjectMorphologyImageFilter Self;
  typedef ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard New method */
  itkNewMacro(Self);  

  /** Runtime information support */
  itkTypeMacro(ErodeObjectMorphologyImageFilter, ObjectMorphologyImageFilter);
  
  /** Declaration of Pixel Type */
  typedef typename Superclass::PixelType PixelType;

  /** Kernel typedef */
  typedef TKernel KernelType;
  
  /** Kernel (structuring element) iterator */
  typedef typename KernelType::ConstIterator KernelIteratorType ;
 
  typedef NeighborhoodIterator<TOutputImage>
  OutputNeighborhoodIteratorType ;

  /** Set the value to be assigned to eroded pixels */
  itkSetMacro(BackgroundValue, PixelType);

  /** Get the value to be assigned to eroded pixels */
  itkGetMacro(BackgroundValue, PixelType);

protected:
  ErodeObjectMorphologyImageFilter();
  ~ErodeObjectMorphologyImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Apply the kernel to the neighborhood given.
   *
   * All values in neighborhood covered by the kernel will be set to the
   * background value.  */
  void Evaluate(OutputNeighborhoodIteratorType &nit,
                const KernelType &kernel);

private:
  ErodeObjectMorphologyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PixelType m_BackgroundValue;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkErodeObjectMorphologyImageFilter.txx"
#endif

#endif


