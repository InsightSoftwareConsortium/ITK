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
class ITK_EXPORT LineSpatialObjectPoint 
: public SpatialObjectPoint<TPointDimension>
{

public:

  typedef LineSpatialObjectPoint                      Self;
  typedef SpatialObjectPoint<TPointDimension>         Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef const SmartPointer< const Self >            ConstPointer;
  typedef Point< double, TPointDimension >            PointType;
  typedef CovariantVector<double,TPointDimension>     VectorType;
  typedef VectorType*                                 VectorPointer;
  typedef FixedArray<VectorPointer,TPointDimension-1> NormalArrayType;

  itkNewMacro( LineSpatialObjectPoint );

  itkTypeMacro( LineSpatialObjectPoint, SpatialObjectPoint );

  /** Get Normal */
  VectorPointer GetNormal( unsigned int index );

  /** Set Normal */
  void SetNormal(VectorType & normal, unsigned int index);

  /** Copy one LineSpatialObjectPoint to another */
  Self & operator=(const LineSpatialObjectPoint & rhs);

protected:

  /** Constructor. This one defines the # of dimensions in the LineSpatialObjectPoint */
  LineSpatialObjectPoint( void );

  /** Default destructor. */
  ~LineSpatialObjectPoint( void );

  NormalArrayType m_NormalArray;

  /**  Initialization of the common variables */
  void CommonConstruction( void );

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkLineSpatialObjectPoint.txx"
#endif

#endif // __itkLineSpatialObjectPoint_h
