/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLBFGSOptimizer_h
#define __itkLBFGSOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_lbfgs.h"

namespace itk
{
  
/** \class LBFGSOptimizer
 * \brief Wrap of the vnl_lbfgs algorithm
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT LBFGSOptimizer : 
    public SingleValuedNonLinearVnlOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSOptimizer                     Self;
  typedef SingleValuedNonLinearVnlOptimizer   Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LBFGSOptimizer, SingleValuedNonLinearVnlOptimizer );

  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_lbfgs             InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  vnl_lbfgs * GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction( SingleValuedCostFunction * costFunction );

  /** Set/Get the optimizer trace flag. If set to true, the optimizer
   * prints out information every iteration.
   */
  virtual void SetTrace( bool flag );
  itkGetMacro( Trace, bool );
  itkBooleanMacro( Trace );

  /** Set/Get the maximum number of function evaluations allowed. */
  virtual void SetMaximumNumberOfFunctionEvaluations( unsigned int n );
  itkGetMacro( MaximumNumberOfFunctionEvaluations, unsigned int );

  /** Set/Get the gradient convergence tolerance. This is a positive 
   * real number that determines the accuracy with which the solution is to
   * be found. The optimization terminates when: 
   * ||G|| < gtol max(1,||X||) where ||.|| denotes the Euclidean norm.
   */
  virtual void SetGradientConvergenceTolerance( double gtol );
  itkGetMacro( GradientConvergenceTolerance, double );

  /** Set/Get the line search accuracy. This is a positive real number
   * with a default value of 0.9, which controls the accuracy of the line
   * search. If the function and gradient evalutions are inexpensive with 
   * respect to the cost of the iterations it may be advantageous to set
   * the value to a small value (say 0.1).
   */
  virtual void SetLineSearchAccuracy( double tol );
  itkGetMacro( LineSearchAccuracy, double );

  /** Set/Get the default step size. This is a positive real number
   * with a default value of 1.0 which determines the stpe size in the line
   * search.
   */
  virtual void SetDefaultStepLength( double stp );
  itkGetMacro( DefaultStepLength, double );

protected:
  LBFGSOptimizer();
  virtual ~LBFGSOptimizer();
  void PrintSelf(std::ostream& os, Indent indent) const;

  typedef Superclass::CostFunctionAdaptorType   CostFunctionAdaptorType;

private:
  LBFGSOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  bool                          m_OptimizerInitialized;
  InternalOptimizerType       * m_VnlOptimizer;

  bool                          m_Trace;
  unsigned int                  m_MaximumNumberOfFunctionEvaluations;
  double                        m_GradientConvergenceTolerance;
  double                        m_LineSearchAccuracy;
  double                        m_DefaultStepLength;

};

} // end namespace itk



#endif



