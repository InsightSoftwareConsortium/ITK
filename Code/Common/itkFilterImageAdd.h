/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAdd.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageAdd_h
#define __itkFilterImageAdd_h

#include "itkImageSource.h"

#include "itkImageRegionSimpleIterator.h"

namespace itk
{
  
/** \class FilterAdd
 * \brief Implements an operator for pixel-wise addition of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT FilterImageAdd :
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageAdd  Self;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   *  Pointer type for first input image
   */
  typedef typename TInputImage1::Pointer Image1Pointer;

  /**
   *  Pointer type for second input image
   */
  typedef typename TInputImage2::Pointer Image2Pointer;

  /**
   *  Pointer type for output image
   */
  typedef typename TOutputImage::Pointer Image3Pointer;

  /**
   *  Iterator type for first input image
   */
  typedef itk::ImageRegionSimpleIterator< typename TInputImage1::PixelType, TInputImage1::ImageDimension> Image1Iterator;

  /**
   *  Iterator type for second input image
   */
  typedef itk::ImageRegionSimpleIterator< typename TInputImage2::PixelType, TInputImage2::ImageDimension> Image2Iterator;

  /**
   *  Iterator type for first input image
   */
  typedef itk::ImageRegionSimpleIterator< typename TOutputImage::PixelType, TOutputImage::ImageDimension> Image3Iterator;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Method for execute the algorithm
   */
   void Execute(void);
  
  /**
   * Connect one of the operands for pixel-wise addition
   */
   void SetInput1( Image1Pointer image1);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput2( Image2Pointer image2);

protected:

  Image1Pointer image1;
  Image2Pointer image2;
  Image3Pointer image3;

  FilterImageAdd();
  virtual ~FilterImageAdd() {};
  FilterImageAdd(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageAdd.txx"
#endif

#endif
