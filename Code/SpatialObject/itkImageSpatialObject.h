/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkImageSpatialObject_h
#define __itkImageSpatialObject_h

#include "itkRGBPixel.h"
#include "itkImage.h"
#include "itkExceptionObject.h"
#include "itkSpatialObject.h"

namespace itk
{
  
 /** \class ImageSpatialObject
 * \brief Implementation of an image as spatial object.
 *
 * This class combines functionnalities from a spatial object,
 * and an image.
 *
 * \also SpatialObject CompositeSpatialObject
 */

template < unsigned int NDimensions = 3,
           class TransformType = AffineTransform< double,
                                                  NDimensions>,
           class PixelType = RGBPixel< char >
         >
class ImageSpatialObject 
: public SpatialObject< NDimensions, TransformType, PixelType >
{

public:
 
  typedef double ScalarType; 
  typedef ImageSpatialObject< NDimensions, TransformType, PixelType > Self;
  typedef SpatialObject< NDimensions, TransformType, PixelType > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef Image< PixelType, NDimensions > ImageType;
  typedef ImageType::Pointer ImagePointer;
  typedef ImageType::IndexType IndexType;
  typedef ImageType::RegionType RegionType;

  typedef Superclass::PointType PointType;
  typedef Superclass::BoundingBoxType BoundingBoxType;

  typedef VectorContainer< unsigned long, PointType> PointContainerType;
  typedef PointContainerType::Pointer PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Self, Superclass );

  /** Set the image. */
  void SetImage( ImagePointer image );

  /** Get a pointer to the image currently attached to the object. */
  ImagePointer GetImage( void );

  /** Return true if the object is evaluable at the requested point, and else otherwise. */
  bool IsEvaluableAt( const PointType & point );

  /** Returns the value of the image at the requested point. If the point is not inside
  * the object, then an exception is thrown.
  * \also ExceptionObject
  */
  void ValueAt( const PointType & point, PixelType & value );
  
  //void DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value );

  /** Returns true if the point is inside, false otherwise. */
  bool IsInside( const PointType & point );
 
  /** Compute the boundaries of the iamge spatial object. */
  void ComputeBounds( void );

  /** Returns the latest modified time of the object, and all of its component. */
  unsigned long GetMTime( void ) const;

protected:

  ImagePointer m_Image;

  ImageSpatialObject();
  virtual ~ImageSpatialObject();

  void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkImageSpatialObject.txx"
#endif

#endif //__itkImageSpatialObject_h
