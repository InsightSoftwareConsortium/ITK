/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$



  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkImageMapper_h
#define __itkImageMapper_h

#include "itkImage.h"
#include "itkPoint.h"
#include "itkRegistrationMapper.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{

/** \class ImageMapper
 * \brief Maps values from the image using a transformation
 *
 * This Class is templated over the image type and the 
 * transformation type. Using a point in space, it transforms
 * the point and gets the value of the image at that new point 
 * using some interpolation strategy.
 *
 */



template <class TImage, class TTransformation>
class ITK_EXPORT ImageMapper : 
  public RegistrationMapper< TImage, TTransformation > 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageMapper<TImage,TTransformation>  Self;

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
  
  /**
   * Type of the interpolation function
   */
   typedef typename LinearInterpolateImageFunction<TImage>  InterpolatorType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageMapper, RegistrationMapper);

  /**
   * Connect the Domain
   */
  void SetDomain(DomainPointer &);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);



  enum { SpaceDimension = TTransformation::ParametersDimension};

  

public: 

  /**
   * Specify the point for which the image value is wanted
   * this method will throw a MapperException is the mapped
   * point lies outside the image domain
   */
   double Evaluate( PointType & point ); // throw MapperException



protected:

  const double *            m_Spacing;
  IndexType                 m_Start;
  SizeType                  m_Size;
  InterpolatorType::Pointer m_Interpolator;

  ImageMapper();
  ~ImageMapper(){};
  ImageMapper(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include <itkImageMapper.txx>
#endif

#endif







