/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

  /** The optimization algorithm will terminate when the simplex diameter 
   * and the difference in cost function within the simplex falls below user specified 
   * thresholds. 
   * The simplex diameter threshold is set via method 
   * SetParametersConvergenceTolerance() with the default value being 1e-8.
   * The cost function convergence threshold is set via method 
   * SetFunctionConvergenceTolerance() with the default value being 1e-4. */
  virtual void SetParametersConvergenceTolerance( double tol );
  itkGetMacro( ParametersConvergenceTolerance, double );
  virtual void SetFunctionConvergenceTolerance( double tol );
  itkGetMacro( FunctionConvergenceTolerance, double );

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
};

} // end namespace itk



#endif



