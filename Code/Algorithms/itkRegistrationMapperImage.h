/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationMapperImage_h
#define __itkRegistrationMapperImage_h

#include "itkImage.h"
#include "itkPoint.h"
#include "itkRegistrationMapper.h"

namespace itk
{
  
/** \class RegistrationMapperImage
 * \brief Maps values from the image using a transformation
 *
 * This Class is templated over the image type and the 
 * transformation type. Using a point in space, it transforms
 * the point and gets the value of the image at that new point 
 * using some interpolation strategy.
 *
 */


template <class TImage, class TTransformation>
class ITK_EXPORT RegistrationMapperImage : 
  public RegistrationMapper< TImage, TTransformation > 

{

public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMapper<TImage,TTransformation>  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef RegistrationMapper< TImage, TTransformation >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Typedef of the Point used to represent coordinates
   */
   typedef typename TTransformation::PointType   PointType;


  /**
   * Typedef of the Pixel type
   */
   typedef  typename   TImage::PixelType   PixelType;


  /**
   * Typedef of the image index
   */
   typedef  typename   TImage::IndexType   IndexType;


  /**
   * Typedef of the image region
   */
   typedef  typename   TImage::RegionType   RegionType;


  /**
   * Typedef of the image size
   */
   typedef  typename   TImage::SizeType   SizeType;



public: 

  /**
   * Evaluate the pixel at the current position
   */
   PixelType Evaluate( void ) const; 


  /**
   * Test whether the point is inside the image or not
   */
   bool IsInside( const PointType & point ) const;


protected:
  RegistrationMapperImage();
  ~RegistrationMapperImage(){};
  RegistrationMapperImage(const Self&) {}
  void operator=(const Self&) {}

  mutable IndexType         m_CurrentIndex;


};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include <itkRegistrationMapperImage.txx>
#endif

#endif
