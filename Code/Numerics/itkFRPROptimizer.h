/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFRPROptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFRPROptimizer_h
#define __itkFRPROptimizer_h

#include <itkVector.h>
#include <itkMatrix.h>
#include <itkPowellOptimizer.h>

namespace itk
{

/** \class FRPROptimizer
 * \brief Implements Fletch-Reeves & Polak-Ribiere optimization using dBrent
 * line search - adapted from Numerical Recipes in C (first edition).
 *
 * This optimizer needs a cost function.
 * This optimizer needs to be able to compute partial derivatives of the 
 *    cost function with respect to each parameter.
 *
 * The SetStepLength determines the initial distance to step in a line direction
 * when bounding the minimum (using bracketing triple spaced using a 
 * derivative-based search strategy).
 *
 * The StepTolerance terminates optimization when the parameter values are
 * known to be within this (scaled) distance of the local extreme.
 *
 * The ValueTolerance terminates optimization when the cost function values at
 * the current parameters and at the local extreme are likely (within a second
 * order approximation) to be within this is tolerance.
 *
 * \ingroup Numerics Optimizers
 *
 */

class ITK_EXPORT FRPROptimizer: 
    public PowellOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef FRPROptimizer                       Self ;
  typedef PowellOptimizer                     Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  typedef SingleValuedNonLinearOptimizer::ParametersType
                                              ParametersType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(FRPROptimizer, PowellOptimizer );
  
  /** Type of the Cost Function   */
  typedef  SingleValuedCostFunction         CostFunctionType;
  typedef  CostFunctionType::Pointer        CostFunctionPointer;

  /** Start optimization. */
  void StartOptimization();

  /** Set it to the Fletch-Reeves optimizer */
  void SetToFletchReeves();
  
  /** Set it to the Fletch-Reeves optimizer */
  void SetToPolakRibiere();
  
protected:
  FRPROptimizer() ;
  virtual ~FRPROptimizer(); 

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Get the value of the n-dimensional cost function at this scalar step
   * distance along the current line direction from the current line origin.
   * Line origin and distances are set via SetLine */
  virtual void GetValueAndDerivative(ParametersType p, double * val,
                                     ParametersType * xi);

  virtual void   LineOptimize(ParametersType * p, ParametersType xi,
                              double * val );


private:
  FRPROptimizer(const FRPROptimizer&) ; // not implemented

  typedef enum 
    {
    FletchReeves,
    PolakRibiere
    }               OptimizationType;

  OptimizationType  m_OptimizationType;

} ; // end of class

} // end of namespace itk

#endif
