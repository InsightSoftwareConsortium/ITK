/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageTernary.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageTernary_h
#define __itkFilterImageTernary_h

#include "itkImageSource.h"
#include "itkImageRegionSimpleIterator.h"

namespace itk
{
  
/** \class FilterTernary
 * \brief Implements pixel-wise generic operation of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * It is parametrized too by the operation to be applied. 
 * A Functor style is used.
 */

template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction    >
class ITK_EXPORT FilterImageTernary :
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageTernary  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>   Superclass;

  /** 
   * Smart pointer typedef support.
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
   *  Pointer type for third input image
   */
  typedef typename TInputImage3::Pointer Image3Pointer;


  /**
   *  Pointer type for output image
   */
  typedef typename TOutputImage::Pointer ImageOutputPointer;

  /**
   *  Iterator type for first input image
   */
  typedef itk::ImageRegionSimpleIterator< TInputImage1 > Image1Iterator;

  /**
   *  Iterator type for second input image
   */
  typedef itk::ImageRegionSimpleIterator< TInputImage2 > Image2Iterator;

  /**
   *  Iterator type for third input image
   */
  typedef itk::ImageRegionSimpleIterator< TInputImage3 > Image3Iterator;


  /**
   *  Iterator type for output image
   */
  typedef itk::ImageRegionSimpleIterator< TOutputImage > ImageOutputIterator;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Method for execute the algorithm
   */
   void GenerateData(void);
   
  /**
   *  Region
   */
  typedef typename TOutputImage::RegionType RegionType;

  /**
   * Connect one of the operands for pixel-wise addition
   */
   void SetInput1( Image1Pointer image1);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput2( Image2Pointer image2);

  /**
   * Connect one of the operands for pixel-wise addition
   */
  void SetInput3( Image3Pointer image3);


protected:

  Image1Pointer m_Image1;
  Image2Pointer m_Image2;
  Image3Pointer m_Image3;

  ImageOutputPointer m_OutputImage;

  FilterImageTernary();
  virtual ~FilterImageTernary() {};
  FilterImageTernary(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageTernary.txx"
#endif

#endif
