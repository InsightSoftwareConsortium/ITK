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

  /**  Measure type.
   *  It defines a type used to return the cost function value.  */
  typedef double                    MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative. */
  typedef Array<double>             DerivativeType;

protected:
  SingleValuedNonLinearOptimizer();
  virtual ~SingleValuedNonLinearOptimizer() {}

private: 
  SingleValuedNonLinearOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSingleValuedNonLinearOptimizer.txx"
#endif

#endif



