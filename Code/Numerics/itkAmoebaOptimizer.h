/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAmoebaOptimizer_h
#define __itkAmoebaOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_amoeba.h"

namespace itk
{
  
/** \class AmoebaOptimizer
 * \brief Wrap of the vnl_amoeba algorithm
 *
 * AmoebaOptimizer is a wrapper around the vnl_amoeba algorithm which
 * is an implementation of the Nelder-Meade downhill simplex
 * problem. For most problems, it is a few times slower than a
 * Levenberg-Marquardt algorithm but does not require derivatives of
 * its cost function. It works by creating a simplex (n+1 points in
 * ND space). The cost function is evaluated at each corner of the
 * simplex.  The simplex is then modified (by reflecting a corner
 * about the opposite edge, by shrinking the entire simplex, by
 * contracting one edge of the simplex, or by expanding the simplex)
 * in searching for the minimum of the cost function.
 *
 * The methods AutomaticInitialSimplex() and SetInitialSimplexDelta()
 * control whether the optimizer defines the initial simplex
 * automatically (by constructing a very small simplex around the
 * initial position) or uses a user supplied simplex size.
 *
 * AmoebaOptimizer can only minimize a function.
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT AmoebaOptimizer : 
    public SingleValuedNonLinearVnlOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef AmoebaOptimizer                     Self;
  typedef SingleValuedNonLinearVnlOptimizer   Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( AmoebaOptimizer, SingleValuedNonLinearVnlOptimizer );

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_amoeba             InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  vnl_amoeba * GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction( SingleValuedCostFunction * costFunction );

  /** Set/Get the maximum number of iterations. The optimization algorithm will
   * terminate after the maximum number of iterations has been reached. 
   * The default value is 500. */
  virtual void SetMaximumNumberOfIterations( unsigned int n );
  itkGetMacro( MaximumNumberOfIterations, unsigned int );

  /** Set/Get the mode which determines how the amoeba algorithm
   * defines the initial simplex.  Default is
   * AutomaticInitialSimplexOn. If AutomaticInitialSimplex is on, the
   * initial simplex is created with a default size. If
   * AutomaticInitialSimplex is off, then InitialSimplexDelta will be
   * used to define the initial simplex, setting the ith corner of the
   * simplex as [x0[0], x0[1], ..., x0[i]+InitialSimplexDelta[i], ...,
   * x0[d-1]]. */
  itkSetMacro(AutomaticInitialSimplex, bool);
  itkBooleanMacro(AutomaticInitialSimplex);
  itkGetMacro(AutomaticInitialSimplex, bool);

  /** Set/Get the deltas that are used to define the initial simplex
   * when AutomaticInitialSimplex is off. */
  itkSetMacro(InitialSimplexDelta, ParametersType);
  itkGetMacro(InitialSimplexDelta, ParametersType);

  /** The optimization algorithm will terminate when the simplex
   * diameter and the difference in cost function at the corners of
   * the simplex falls below user specified thresholds.  The simplex
   * diameter threshold is set via method
   * SetParametersConvergenceTolerance() with the default value being
   * 1e-8.  The cost function convergence threshold is set via method
   * SetFunctionConvergenceTolerance() with the default value being
   * 1e-4. */
  virtual void SetParametersConvergenceTolerance( double tol );
  itkGetMacro( ParametersConvergenceTolerance, double );
  virtual void SetFunctionConvergenceTolerance( double tol );
  itkGetMacro( FunctionConvergenceTolerance, double );

  /** Return Current Value */
  MeasureType GetValue() const;

protected:
  AmoebaOptimizer();
  virtual ~AmoebaOptimizer();
  void PrintSelf(std::ostream& os, Indent indent) const;

  typedef Superclass::CostFunctionAdaptorType   CostFunctionAdaptorType;

private:
  AmoebaOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  bool                          m_OptimizerInitialized;
  InternalOptimizerType       * m_VnlOptimizer;
  unsigned int                  m_MaximumNumberOfIterations;
  double                        m_ParametersConvergenceTolerance;
  double                        m_FunctionConvergenceTolerance;

  bool                          m_AutomaticInitialSimplex;
  ParametersType                m_InitialSimplexDelta;
};

} // end namespace itk



#endif



