/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNormalizeImageFilter_h
#define __itkNormalizeImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class NormalizeImageFilter 
 * \brief Normalize an image by setting its mean to zero and variance to one.
 *
 * NormalizeImageFilter shifts and scales an image so that the pixels
 * in the image have a zero mean and unit variance. This filter uses
 * StatisticsImageFIlter to compute the mean and variance of the input
 * and then applies ShiftScaleImageFilter to shift and scale the pixels.
 *
 * NB: since this filter normalizes the data to lie within -1 to 1,
 * integral types will produce an image that DOES NOT HAVE a unit variance.
 * \ingroup MathematicalImageFilters
 */
template<class TInputImage,class TOutputImage>
class ITK_EXPORT NormalizeImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard Self typedef */
  typedef NormalizeImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(NormalizeImageFilter, ImageToImageFilter);
  
  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

protected:
  NormalizeImageFilter(){}
  
  /** GenerateData. */
  void  GenerateData ();

 private:
  NormalizeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void SetupProgressMethods(ProcessObject *statistic, ProcessObject *shiftScale);

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizeImageFilter.txx"
#endif

#endif
