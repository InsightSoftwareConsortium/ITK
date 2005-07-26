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
  
  /** Set/Get macros for the measurement vector length. NOTE: Users must set
   * this before using the pertinent density function. Often this may be 
   * set automatically. For instance when supplying mean or an input subsample,
   * the density function will query the length of measurement vectors from the
   * sample or mean */
  itkSetMacro( MeasurementVectorSize, MeasurementVectorSizeType );
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType );
  
protected:
  DensityFunction(void): m_MeasurementVectorSize( 0 ) {}
  virtual ~DensityFunction(void) {}

  void PrintSelf(std::ostream& os, Indent indent) const
    { 
    Superclass::PrintSelf(os,indent); 
    
    os << indent << "MeasurementVectorSize: " 
      << m_MeasurementVectorSize << std::endl;
    }

private:
  MeasurementVectorSizeType m_MeasurementVectorSize;
} ; // end of class

} // end of namespace Statistics
} // end namespace itk

#endif





