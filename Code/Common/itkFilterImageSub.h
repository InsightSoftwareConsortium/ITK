/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSub.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageSub_h
#define __itkFilterImageSub_h

#include "itkImageSource.h"

#include <itkImageRegionIterator.h> 

namespace itk
{
  
/** \class FilterSub
 * \brief Implements an operator for pixel-wise subtraction of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT FilterImageSub :
    public ImageSource<TOutputImage> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageSub  Self;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;

  /**
   *  Pointer type for first input image
   */
  typedef typename TInputImage1::Pointer InputImage1Pointer;

  /**
   *  Pointer type for second input image
   */
  typedef typename TInputImage2::Pointer InputImage2Pointer;

  /**
   *  Pointer type for output image
   */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /**
   *  Iterator type for first input image
   */
  typedef itk::ImageRegionIterator< typename TInputImage1::PixelType, TInputImage1::ImageDimension> InputImage1Iterator;

  /**
   *  Iterator type for second input image
   */
  typedef itk::ImageRegionIterator< typename TInputImage2::PixelType, TInputImage2::ImageDimension> InputImage2Iterator;

  /**
   *  Iterator type for first input image
   */
  typedef itk::ImageRegionIterator< typename TOutputImage::PixelType, TOutputImage::ImageDimension> OutputImageIterator;

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
   void SetInput1( InputImage1Pointer image1);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput2( InputImage2Pointer image2);

protected:

  InputImage1Pointer inputImage1;
  InputImage2Pointer inputImage2;
  OutputImagePointer outputImage;

  FilterImageSub();
  virtual ~FilterImageSub() {};
  FilterImageSub(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageSub.txx"
#endif

#endif
