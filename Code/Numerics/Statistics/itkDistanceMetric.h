/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceMetric_h
#define __itkDistanceMetric_h

#include "itkMembershipFunctionBase.h"
#include "itkArray.h"

namespace itk{ 
namespace Statistics{

/** \class DistanceMetric
 * \brief this class declares common interfaces 
 * for distance functions.
 *
 * As a function derived from MembershipFunctionBase, 
 * users use Evaluate method to get result.
 * 
 * To use this function in the context of MembershipFunction,
 * users should first set the origin by calling SetOrigin() function,
 * then call Evaluate() method with a point to get the distance between
 * the origin point and the evaluation point.
 * 
 * If users want to the distance between two points without setting
 * the origin point. Use two argument version of Evaluate() function.
 * 
 * The class can be templated over any container that holds data elements. The 
 * containter is expected to provide access to its elements with the [] operator.
 * It must also implement a Size() that returns the length of the container.
 * It must also contain a typedef "ValueType" that defines the data-type held
 * by the container.
 * (In other words it will support itk::Vector, FixedArray, Array ).
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function 
 * GetMeasurementVectorSize() instead. \c OriginType typedef has been changed 
 * from Vector to Array.
 */

template< class TVector >
class ITK_EXPORT DistanceMetric : public MembershipFunctionBase< TVector >
{
public:
  /** Standard typedefs */
  typedef DistanceMetric Self;
  typedef MembershipFunctionBase< TVector > Superclass;

  /** Typedef for the length of each measurement vector */
  typedef unsigned int  MeasurementVectorSizeType;

  /** Set/Get Macro to set the length of each measurement vector. */
  virtual void SetMeasurementVectorSize( MeasurementVectorSizeType );
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType ); 
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DistanceMetric, MembershipFunctionBase);

  typedef Array< double > OriginType ;

  /** Sets the origin point that will be used for the single point 
   * version Evaluate() function. This function is necessary part of
   * implementing MembershipFunctionBase's Evaluate() interface */ 
  void SetOrigin(const OriginType& x) ;
  itkGetConstReferenceMacro(Origin, OriginType) ;

  /** Gets the distance between the origin point and x. This function
   * work with SetOrigin() function*/
  virtual double Evaluate(const TVector &x) const = 0 ;
  
  /** Gets the distance between x1 and x2 points */
  virtual double Evaluate(const TVector &x1, const TVector &x2) const = 0 ;
  
protected:
  DistanceMetric() { m_MeasurementVectorSize = 0; }
  virtual ~DistanceMetric() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  OriginType m_Origin ;

private:
  MeasurementVectorSizeType m_MeasurementVectorSize;
  
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceMetric.txx"
#endif

#endif







