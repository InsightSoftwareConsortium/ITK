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
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Typedef of the Point used to represent coordinates
   */
   typedef typename TTransformation::PointType   PointType;


   /**
   * Typedef of transformation parameters
   */
   typedef typename TTransformation::ParametersType   ParametersType;

  /**
   *  Pointer type for the Reference 
   */
  typedef typename Superclass::DomainPointer DomainPointer;

  /**
   *  type for the Reference 
   */
  typedef typename Superclass::DomainType     DomainType;



  /**
   * Typedef of the Image type
   */
   typedef  TImage   ImageType;


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
   typedef LinearInterpolateImageFunction<ImageType>  InterpolatorType;


  /**
   * Type of the interpolation function
   */
   typedef typename  InterpolatorType::Pointer  InterpolatorPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageMapper, RegistrationMapper);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  /**
   * Set Domain
   */
  void SetDomain( DomainType * domain );



  enum { SpaceDimension = TTransformation::ParametersDimension };

  

public: 

  /**
   * Evaluate the pixel value for the point previously 
   * specified in the IsInside() method
   *
   * \sa IsInside()
   *
   */
   PixelType Evaluate( void ) const; 

   /**
    * Test whether the specified point is inside of
    * the Image Domain. Point coordinates are stored
    * in internal memory to be used by the Evaluate() method.
    *
    * \sa Evaluate();
    *
    */
   bool IsInside( const PointType & point );

protected:

  const double *            m_Spacing;
  IndexType                 m_Start;
  SizeType                  m_Size;
  InterpolatorPointer       m_Interpolator;
  mutable IndexType         m_CurrentIndex;

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







