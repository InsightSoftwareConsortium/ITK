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

#include "itkImageToImageFilter.h"

namespace itk
{
  
/** \class ScalarToArrayCastImageFilter
 *
 * \brief Creates the output image with vector type pixels filled with
 * the intensity values from one or more input images with scalar
 * pixels.
 *
 * This filter is templated over the input image type and 
 * output image type. The each dimension of the output image pixel is
 * filled with each input image pixel's scalar pixel value. This
 * filter can be used to cast a scalar image to a vector image if
 * there is only one input image.
 *
 * \ingroup Multithreaded
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
