/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMembershipFunctionBase_h
#define __itkMembershipFunctionBase_h

#include "itkFunctionBase.h"

namespace itk{ 
namespace Statistics{

/** \class MembershipFunctionBase
 * \brief MembershipFunctionBase class declares common interfaces 
 * for membership functions.
 *
 * As a function derived from FunctionBase, users use Evaluate method
 * get result. However, the return value type of the method is fixed
 * as double. Any function derived from this class returns quantitative
 * measure for how well the vector x belong to the class ( or group)
 * represented by the function.
 */

template< class TVector >
class ITK_EXPORT MembershipFunctionBase :
    public FunctionBase< TVector, double >
{
public:
  /** Standard class typedefs */
  typedef MembershipFunctionBase Self;
  typedef FunctionBase< TVector, double > Superclass ;
  typedef SmartPointer< Self > Pointer ;

  /** Strandard macros */
  itkTypeMacro(MembershipFunctionBase, FunctionBase);

  /** Method to get membership score (discriminant score) of an entity. */
  virtual double Evaluate(const TVector &x) const = 0 ;

protected:
  MembershipFunctionBase(void) {}
  virtual ~MembershipFunctionBase(void) {}

  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent) ; }
} ; // end of class

} // end of namespace Statistics
} // end namespace itk

#endif







