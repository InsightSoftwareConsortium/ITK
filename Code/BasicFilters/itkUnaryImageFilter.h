/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkUnaryImageFilter_h
#define __itkUnaryImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{
  
/** \class UnaryImageFilter
 * \brief Implements pixel-wise generic operation on one image.
 *
 * This class is parametrized over the type of the 
 * input image and the type of the output image. 
 * It is also parametrized by the operation to be applied,
 * using a Functor style.
 */

template <class TInputImage, class TOutputImage, class TFunction >
class ITK_EXPORT UnaryImageFilter : public ImageToImageFilter<TInputImage,TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef UnaryImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Method for execute the algorithm
   */
   void GenerateData(void);

  /**
   * Connect the input image
   */
   void SetInput( TInputImage * image);

protected:

  UnaryImageFilter();
  virtual ~UnaryImageFilter() {};
  UnaryImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnaryImageFilter.txx"
#endif

#endif
