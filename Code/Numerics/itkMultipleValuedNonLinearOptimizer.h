/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultipleValuedNonLinearOptimizer_h
#define __itkMultipleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/vnl_least_squares_function.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class MultipleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT MultipleValuedNonLinearOptimizer : 
        public NonLinearOptimizer < TCostFunction >
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedNonLinearOptimizer  Self;
  typedef   NonLinearOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( MultipleValuedNonLinearOptimizer, NonLinearOptimizer );

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType   MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType DerivativeType;

protected:
  MultipleValuedNonLinearOptimizer() {};
  virtual ~MultipleValuedNonLinearOptimizer() {};

private:
  MultipleValuedNonLinearOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#endif



