/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceMetric_h
#define __itkDistanceMetric_h

#include "itkMembershipFunctionBase.h"
#include "itkVector.h"

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
 */

template< class TVector >
class ITK_EXPORT DistanceMetric : public MembershipFunctionBase< TVector >
{
public:
  /** Standard typedefs */
  typedef DistanceMetric Self;
  typedef MembershipFunctionBase< TVector > Superclass;

  /** Length constant */
  itkStaticConstMacro(VectorLength, unsigned int, TVector::Length);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DistanceMetric, MembershipFunctionBase);

  typedef Vector< double, itkGetStaticConstMacro(VectorLength) > OriginType ;

  /** Sets the origin point that will be used for the single point 
   * version Evaluate() function. This function is necessary part of
   * implementing MembershipFunctionBase's Evaluate() interface */ 
  void SetOrigin(const OriginType& x) ;

  /** Gets the distance between the origin point and x. This function
   * work with SetOrigin() function*/
  virtual double Evaluate(const TVector &x) const = 0 ;
  
  /** Gets the distance between x1 and x2 points */
  virtual double Evaluate(const TVector &x1, const TVector &x2) const = 0 ;
  
protected:
  OriginType m_Origin ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceMetric.txx"
#endif

#endif







