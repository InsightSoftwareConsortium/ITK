/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLineSpatialObjectPoint_h
#define __itkLineSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "itkFixedArray.h"

namespace itk 
{

/** \class LineSpatialObjectPoint
* \brief Point used for a line definition
*
* This class contains all the functions necessary to define a point
* that can be used to build lines.
* This Class derives from SpatialObjectPoint.
* A LineSpatialObjectPoint has NDimension-1 normals. 
*/ 
template < unsigned int TPointDimension = 3 >
class LineSpatialObjectPoint 
: public SpatialObjectPoint<TPointDimension>
{

public:

  typedef LineSpatialObjectPoint                      Self;
  typedef SpatialObjectPoint<TPointDimension>         Superclass;
  typedef Point< double, TPointDimension >            PointType;
  typedef CovariantVector<double,TPointDimension>     VectorType;
  typedef FixedArray<VectorType,TPointDimension-1> NormalArrayType;

  /** Constructor. This one defines the # of dimensions in the LineSpatialObjectPoint */
  LineSpatialObjectPoint( void );

  /** Default destructor. */
  ~LineSpatialObjectPoint( void );

  /** Get Normal */
  const VectorType & GetNormal( unsigned int index ) const;

  /** Set Normal */
  void SetNormal(VectorType & normal, unsigned int index);

  /** Copy one LineSpatialObjectPoint to another */
  Self & operator=(const LineSpatialObjectPoint & rhs);

protected:

  NormalArrayType m_NormalArray;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkLineSpatialObjectPoint.txx"
#endif

#endif // __itkLineSpatialObjectPoint_h
