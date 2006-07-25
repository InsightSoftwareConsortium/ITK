/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSBOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLBFGSBOptimizer_h
#define __itkLBFGSBOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include <string>

namespace itk
{
  
/** \class LBFGSBOptimizer
 * \brief Limited memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 *
 * This class is a wrapper for converted fortan code for performing limited
 * memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 * The algorithm miminizes a nonlinear function f(x) of n variables subject to
 * simple bound constraints of l <= x <= u.
 *
 * See also the documentation in Numerics/lbfgsb.c
 *
 * References:
 *
 * [1] R. H. Byrd, P. Lu and J. Nocedal. 
 * A Limited Memory Algorithm for Bound Constrained Optimization, (1995), 
 * SIAM Journal on Scientific and Statistical Computing , 
 * 16, 5, pp. 1190-1208. 
 *
 * [2] C. Zhu, R. H. Byrd and J. Nocedal. 
 * L-BFGS-B: Algorithm 778: L-BFGS-B, FORTRAN routines for large scale 
 * bound constrained optimization (1997), 
 * ACM Transactions on Mathematical Software, 
 * Vol 23, Num. 4, pp. 550 - 560. 
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT LBFGSBOptimizer : 
    public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSBOptimizer                     Self;
  typedef SingleValuedNonLinearOptimizer      Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LBFGSBOptimizer, SingleValuedNonLinearOptimizer );

  /**  BoundValue type.
   *  Use for defining the lower and upper bounds on the variables. 
   */
  typedef Array<double>             BoundValueType;

  /** BoundSelection type
   * Use for defining the boundary condition for each variables. 
   */
  typedef Array<long>                BoundSelectionType;

  /** Start optimization with an initial value. */
  void StartOptimization( void );

  /** Type of the Cost Function   */
  typedef  SingleValuedCostFunction         CostFunctionType;
  typedef  CostFunctionType::Pointer        CostFunctionPointer;
  typedef  CostFunctionType::MeasureType    MeasureType;

  /** Set the lower bound value for each variable. */
  virtual void SetLowerBound( const BoundValueType & value );
  virtual const BoundValueType & GetLowerBound();

  /** Set the upper bound value for each variable. */
  virtual void SetUpperBound( const BoundValueType & value );
  virtual const BoundValueType & GetUpperBound();

  /** Set the boundary condition for each variable, where
   * select[i] = 0 if x[i] is unbounded,
   *           = 1 if x[i] has only a lower bound,
   *           = 2 if x[i] has both lower and upper bounds, and
   *           = 3 if x[1] has only an upper bound
   */
  virtual void SetBoundSelection( const BoundSelectionType & select );
  virtual const BoundSelectionType & GetBoundSelection();

  /** Set/Get the CostFunctionConvergenceFactor. Algorithm terminates
   * when the reduction in cost function is less than factor * epsmcj
   * where epsmch is the machine precision.
   * Typical values for factor: 1e+12 for low accuracy; 
   * 1e+7 for moderate accuracy and 1e+1 for extremely high accuracy.
   */
  itkSetMacro( CostFunctionConvergenceFactor, double );
  itkGetMacro( CostFunctionConvergenceFactor, double );

  /** Set/Get the ProjectedGradientTolerance. Algorithm terminates
   * when the project gradient is below the tolerance. Default value
   * is 1e-5.
   */
  itkSetMacro( ProjectedGradientTolerance, double );
  itkGetMacro( ProjectedGradientTolerance, double );

  /** Set/Get the MaximumNumberOfIterations. Default is 500 */
  itkSetMacro( MaximumNumberOfIterations, unsigned int );
  itkGetMacro( MaximumNumberOfIterations, unsigned int );

  /** Set/Get the MaximumNumberOfEvaluations. Default is 500 */
  itkSetMacro( MaximumNumberOfEvaluations, unsigned int );
  itkGetMacro( MaximumNumberOfEvaluations, unsigned int );

  /** Set/Get the MaximumNumberOfCorrections. Default is 5 */
  itkSetMacro( MaximumNumberOfCorrections, unsigned int );
  itkGetMacro( MaximumNumberOfCorrections, unsigned int );

  /** This optimizer does not support scaling of the derivatives. */
  void SetScales( const ScalesType & )
    {
    itkExceptionMacro( << "This optimizer does not support scales." );
    }

  /** Get the current iteration number. */
  itkGetConstReferenceMacro( CurrentIteration, unsigned int );

  /** Get the current cost function value. */
  itkGetConstReferenceMacro( Value, MeasureType );

  /** Get the current infinity norm of the project gradient of the cost
   * function. */
  itkGetConstReferenceMacro( InfinityNormOfProjectedGradient, double );

protected:
  LBFGSBOptimizer();
  virtual ~LBFGSBOptimizer();
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  LBFGSBOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  BoundValueType        m_LowerBound;
  BoundValueType        m_UpperBound;
  BoundSelectionType    m_BoundSelection;

  double                m_CostFunctionConvergenceFactor;
  double                m_ProjectedGradientTolerance;
  unsigned int          m_MaximumNumberOfIterations;
  unsigned int          m_MaximumNumberOfEvaluations;
  unsigned int          m_MaximumNumberOfCorrections;

  unsigned int          m_CurrentIteration;
  MeasureType           m_Value;
  double                m_InfinityNormOfProjectedGradient;

};

} // end namespace itk



#endif



