/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSingleValuedNonLinearOptimizer_h
#define __itkSingleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "itkSingleValuedCostFunction.h"

namespace itk
{
  
/** \class SingleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 * \ingroup Numerics Optimizers
 *
 */
class ITK_EXPORT SingleValuedNonLinearOptimizer : 
    public NonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef SingleValuedNonLinearOptimizer  Self;
  typedef NonLinearOptimizer              Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedNonLinearOptimizer, 
                NonLinearOptimizer );

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /** Type of the Cost Function   */
  typedef  SingleValuedCostFunction         CostFunctionType;
  typedef  CostFunctionType::Pointer        CostFunctionPointer;

  /**  Measure type.
   *  It defines a type used to return the cost function value.  */
  typedef CostFunctionType::MeasureType   MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative. */
  typedef CostFunctionType::DerivativeType DerivativeType;

  /** Set the cost function. */
  virtual void SetCostFunction( CostFunctionType * costFunction );

protected:
  SingleValuedNonLinearOptimizer();
  virtual ~SingleValuedNonLinearOptimizer() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  CostFunctionPointer           m_CostFunction;

private: 
  SingleValuedNonLinearOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk



#endif



