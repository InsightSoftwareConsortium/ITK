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

#include "itkObject.h"

namespace itk{ 
  namespace Statistics{

/** \class DensityFunction
 * \brief DensityFunction class represents  Density Function.
 *
 * This class keeps parameter to define  Density Function  and has
 * method to return the probability density 
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement. 
 */

template< class TMeasurementVector >
class ITK_EXPORT DensityFunction :
      public Object
{
public:
  /** Standard class typedefs */
  typedef DensityFunction Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(DensityFunction, Object);
  itkNewMacro(Self);

  /** Typedef alias for the measurement vectors */
  typedef TMeasurementVector MeasurementVectorType ;

 /** Method to get probability of an instance. The return value is the
  * value of the density function, not probability. */
  virtual double Evaluate(MeasurementVectorType &measurement) = 0 ;
  

protected:
  DensityFunction(void) {}
  virtual ~DensityFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent) ; }
} ; // end of class

  } // end of namespace Statistics
} // end namespace itk

#endif
