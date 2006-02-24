/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDensityFunction_h
#define __itkDensityFunction_h

#include "itkMembershipFunctionBase.h"
#include "itkMeasurementVectorTraits.h"

namespace itk{ 
namespace Statistics{

/** \class DensityFunction
 * \brief DensityFunction class defines common interfaces for 
 * density functions.
 *
 * The Evaluate method returns density value for the input measurement vector.
 * The length of the measurement vector must be set using 
 * \c SetMeasurementVectorSize() before using the class.
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Strandard macros */
  itkTypeMacro(DensityFunction, MembershipFunctionBase);

  /** Length of each measurement vector */
  typedef unsigned int MeasurementVectorSizeType;
  
  /** Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  virtual double Evaluate(const TMeasurementVector &measurement) const = 0 ;
  
protected:
  DensityFunction() {}
  virtual ~DensityFunction() {}

  void PrintSelf(std::ostream& os, Indent indent) const
    { 
    Superclass::PrintSelf(os,indent); 
    }

private:
} ; // end of class

} // end of namespace Statistics
} // end namespace itk

#endif





