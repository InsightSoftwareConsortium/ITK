/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToArrayCastImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarToArrayCastImageFilter_h
#define __itkScalarToArrayCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class ScalarToArrayCastImageFilter
 *
 * \brief Casts input vector pixels to output vector pixel type.
 *
 * This filter is templated over the input image type and 
 * output image type.
 * 
 * The filter expect the input image' pixel type is of scalar type and 
 * the output image' pixel type is one dimensional array (subclasses of 
 * FixedArray) of the scalar type.
 *
 * \sa FixedArray
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT ScalarToArrayCastImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ScalarToArrayCastImageFilter  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Standard class macros */
  itkNewMacro(Self);
  itkTypeMacro(ScalarToArrayCastImageFilter, ImageToImageFilter) ;

  typedef typename Superclass::OutputImageRegionType OutputImageRegionType ;
  typedef typename TOutputImage::PixelType OutputImagePixelType ;
protected:
  ScalarToArrayCastImageFilter() ;
  virtual ~ScalarToArrayCastImageFilter() {}
  
  void ThreadedGenerateData(const OutputImageRegionType &outputRegionForThread,
                            int threadId) ;
 
private:
  ScalarToArrayCastImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarToArrayCastImageFilter.txx"
#endif

#endif
