/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTransformation_h
#define __itkTransformation_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"


namespace itk
{
  
/** \class Transformation
 * \brief Generic concept of transformation methods
 *
 * This Abstract Class define the generic interface for a transformation. 
 * It contains a Transform method.
 *
 */

template <class TScalarType,unsigned int NDimensions>
class ITK_EXPORT  Transformation 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Transformation  Self;


  /**
   * Dimension of the domain space
   */
  enum { SpaceDimension     = NDimensions };


  /**
   * Standard vector type for this class
   */
  typedef Vector<TScalarType, SpaceDimension> VectorType;

  
  /**
   * Standard covariant vector type for this class
   */
  typedef CovariantVector<TScalarType, SpaceDimension> CovariantVectorType;

  
  /**
   * Standard vnl_vector type for this class
   */
  typedef vnl_vector_fixed<TScalarType, SpaceDimension> VnlVectorType;

  
  /**
   * Standard coordinate point type for this class
   */
  typedef Point<TScalarType, SpaceDimension> PointType;


  /**
   *  Method to transform a Point
   */
  virtual PointType     Transform(const PointType  &point ) const
                                                { return point; }

  /**
   *  Method to transform a vector
   */
  virtual VectorType    Transform(const VectorType &vector) const
                                                { return vector; }

  /**
   *  Method to transform a vnl_vector
   */
  virtual VnlVectorType Transform(const VnlVectorType &vector) const
                                                { return vector; }

  /**
   *  Method to transform a CovariantVector
   */
  virtual CovariantVectorType Transform(
                           const CovariantVectorType &vector) const
                                                { return vector; }

  
  Transformation();
  virtual ~Transformation() {};
  Transformation(const Self&);
  const Self & operator=(const Self&);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformation.txx"
#endif

#endif



