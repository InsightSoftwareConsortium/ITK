/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTernaryImageFilter_h
#define __itkTernaryImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{
  
/** \class TernaryImageFilter
 * \brief Implements pixel-wise generic operation of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * It is parametrized too by the operation to be applied. 
 * A Functor style is used.
 */

template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction    >
class ITK_EXPORT TernaryImageFilter :
    public ImageToImage<TInputImage1,TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TernaryImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage1,TOutputImage>   Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>        Pointer;
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
   * Connect one of the operands for pixel-wise addition
   */
   void SetInput1( TInputImage1 *image1);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput2( TInputImage2 *image2);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput3( TInputImage3 *image3);


protected:

  TernaryImageFilter();
  virtual ~TernaryImageFilter() {};
  TernaryImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTernaryImageFilter.txx"
#endif

#endif
