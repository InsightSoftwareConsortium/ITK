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

#include "itkImageSource.h"
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
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TernaryImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>   Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

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
  typedef SimpleImageRegionIterator< TInputImage1 > Image1Iterator;

  /**
   *  Iterator type for second input image
   */
  typedef SimpleImageRegionIterator< TInputImage2 > Image2Iterator;

  /**
   *  Iterator type for third input image
   */
  typedef SimpleImageRegionIterator< TInputImage3 > Image3Iterator;


  /**
   *  Iterator type for output image
   */
  typedef SimpleImageRegionIterator< TOutputImage > ImageOutputIterator;


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
