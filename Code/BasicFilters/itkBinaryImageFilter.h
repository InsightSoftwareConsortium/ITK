/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkBinaryImageFilter_h
#define __itkBinaryImageFilter_h

#include "itkImageSource.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{
  
/** \class BinaryImageFilter
 * \brief Implements pixel-wise generic operation of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * It is parametrized too by the operation to be applied. 
 * A Functor style is used.
 */

template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction    >
class ITK_EXPORT BinaryImageFilter :
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BinaryImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

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
   * Connect one of the operands for pixel-wise addition
   */
   void SetInput1( TInputImage1 * image1);

  /**
   * Connect one of the operands for pixel-wise addition
   */
   void SetInput2( TInputImage2 * image2);

protected:

  BinaryImageFilter();
  virtual ~BinaryImageFilter() {};
  BinaryImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryImageFilter.txx"
#endif

#endif
