/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDensityFunction_h
#define __itkDensityFunction_h

#include "itkMembershipFunctionBase.h"

namespace itk{ 
  namespace Statistics{

/** \class DensityFunction
 * \brief DensityFunction class defines common interfaces for density functions.
 *
 * The Evaluate method returns density value for the input measurement vector
 */

template< class TMeasurementVector >
class ITK_EXPORT DensityFunction :
      public MembershipFunctionBase< TMeasurementVector >
{
public:
  /** Standard class typedefs */
  typedef DensityFunction Self;
  typedef MembershipFunctionBase< TMeasurementVector > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(DensityFunction, MembershipFunctionBase);

  /** Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  virtual double Evaluate(const TMeasurementVector &measurement) const = 0 ;
  
protected:
  DensityFunction(void) {}
  virtual ~DensityFunction(void) {}

  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent) ; }
} ; // end of class

  } // end of namespace Statistics
} // end namespace itk

#endif





