/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBlobSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBlobSpatialObject_h
#define __itkBlobSpatialObject_h

#include <list>

#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class BlobSpatialObject
* \brief Representation of a Blob based on the spatial object classes.
*
* The Blob is basically defined by a set of points which are inside this blob
*
* \also SpatialObjectPoint
*/

template < unsigned int TDimension = 3 , unsigned int PipelineDimension = 3 >
class BlobSpatialObject 
:public SpatialObject<  TDimension, 
                        AffineTransform<double, TDimension>, 
                        PipelineDimension
                     >
{

public:

  typedef BlobSpatialObject                          Self;
  typedef SpatialObject< TDimension, 
                         AffineTransform< double, TDimension>,
                         PipelineDimension
                       >                              Superclass;
  typedef SmartPointer < Self >                       Pointer;
  typedef SmartPointer < const Self >                 ConstPointer;
  typedef double                                      ScalarType;
  typedef SpatialObjectPoint< TDimension >            BlobPointType;
  typedef typename BlobPointType::Pointer             BlobPointPointer; 
  typedef std::list < BlobPointPointer >              PointListType;
  typedef typename Superclass::PointType              PointType;
  typedef VectorContainer<unsigned long,PointType>    PointContainerType;
  typedef SmartPointer<PointContainerType>            PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( Self, Superclass );
  
  /** Returns a reference to the list of the Blob points. */
  PointListType & GetPoints( void );

 /** Returns a reference to the list of the Blob points. */
  const PointListType & GetPoints( void ) const;

  /** Set the list of Blob points.*/
  void SetPoints( PointListType & newPoints );

  /** Returns true if the Blob is evaluable at the requested point, 
   *  false otherwise. */
  bool IsEvaluableAt( const PointType & point );

  /** Returns the value of the Blob at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy Blobs. */
  void ValueAt( const PointType & point, double & value );

  /** Returns true if the point is inside the Blob, false otherwise. */
  bool IsInside( const PointType & point );

  /** Compute the boundaries of the Blob. */
  void ComputeBounds( void );

  /** Return the last modified time of the object, and all of its components */
  unsigned long GetMTime( void ) const;

protected:

  PointListType   m_Points;
  TimeStamp       m_BoundsMTime; 

  BlobSpatialObject();
  virtual ~BlobSpatialObject();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkBlobSpatialObject.txx" 
#endif 

#endif // __itkBlobSpatialObject_h
