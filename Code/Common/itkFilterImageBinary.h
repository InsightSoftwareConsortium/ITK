/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageBinary.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageBinary_h
#define __itkFilterImageBinary_h

#include "itkImageSource.h"
#include "itkImageRegionSimpleIterator.h"

namespace itk
{
  
/** \class FilterBinary
 * \brief Implements pixel-wise generic operation of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * It is parametrized too by the operation to be applied. 
 * A Functor style is used.
 */

template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction    >
class ITK_EXPORT FilterImageBinary :
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageBinary  Self;

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
  typedef typename TOutputImage::Pointer ImageOutputPointer;

  /**
   *  Region
   */
  typedef typename TOutputImage::RegionType RegionType;

  /**
   *  Iterator type for first input image
   */
  typedef ImageRegionSimpleIterator< TInputImage1 > Image1Iterator;

  /**
   *  Iterator type for second input image
   */
  typedef ImageRegionSimpleIterator< TInputImage2 > Image2Iterator;

  /**
   *  Iterator type for output image
   */
  typedef ImageRegionSimpleIterator< TOutputImage > ImageOutputIterator;


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

  Image1Pointer m_Image1;
  Image2Pointer m_Image2;

  ImageOutputPointer m_OutputImage;

  FilterImageBinary();
  virtual ~FilterImageBinary() {};
  FilterImageBinary(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageBinary.txx"
#endif

#endif
