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
           class PixelType = RGBPixel< char > ,
           unsigned int SpaceDimension = 3
         >
class ImageSpatialObject 
: public SpatialObject< NDimensions , SpaceDimension >
{

public:
 
  typedef double ScalarType; 
  typedef ImageSpatialObject< NDimensions, PixelType , SpaceDimension > Self;
  typedef SpatialObject< NDimensions > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef Image< PixelType, NDimensions > ImageType;
  typedef typename ImageType::Pointer ImagePointer;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::RegionType RegionType;

  typedef typename Superclass::PointType PointType;
  typedef typename Superclass::BoundingBoxType BoundingBoxType;

  typedef VectorContainer< unsigned long, PointType> PointContainerType;
  typedef typename PointContainerType::Pointer PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Self, Superclass );

  /** Set the image. */
  void SetImage( ImageType * image );

  /** Get a pointer to the image currently attached to the object. */
  ImageType * GetImage( void );

  /** Return true if the object is evaluable at the requested point, 
   *  and else otherwise. */
  bool IsEvaluableAt( const PointType & point, 
                      unsigned int depth=0, char *name=NULL) const;

  /** Returns the value of the image at the requested point. 
   *  If the point is not inside the object, then an exception is thrown.
   * \also ExceptionObject */
  bool ValueAt( const PointType & point, double & value, 
                unsigned int depth=0, char *name=NULL) const;
  
  /** Returns true if the point is inside, false otherwise. */
  bool IsInside( const PointType & point,
                  unsigned int depth=0, char *name=NULL) const;
 
  /** Compute the boundaries of the iamge spatial object. */
  bool ComputeBoundingBox( unsigned int depth=0, char *name=NULL);

  /** Returns the latest modified time of the object and its component. */
  unsigned long GetMTime( void ) const;

  /** Set the slice position */
  void SetSlicePosition(unsigned int dimension, int position);

  /** Get the slice position */
  int GetSlicePosition(unsigned int dimension) 
      {return m_SlicePosition[dimension];}

protected:

  ImagePointer m_Image;

  ImageSpatialObject();
  virtual ~ImageSpatialObject();

  void PrintSelf( std::ostream& os, Indent indent ) const;

  int* m_SlicePosition;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkImageSpatialObject.txx"
#endif

#endif //__itkImageSpatialObject_h
