/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkGradientDescentOptimizer_h
#define __itkGradientDescentOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
  
/** \class GradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * GradientDescentOptimizer implements a simple gradient descent optimizer.
 * At each iteration the current position is updated according to
 *
 * p(n+1) = p(n) + learningRate * d f(p(n)) / d p(n)
 *
 * The learning rate is a fixed scalar defined via SetLearningRate().
 * The optimizer steps through a user defined number of iterations;
 * no convergence checking is done.
 *
 * Additionally, user can scale each component of the df / dp
 * but setting a scaling vector using method SetScale().
 *
 * \sa RegularStepGradientDescentOptimizer
 */  
template <class TCostFunction>
class ITK_EXPORT GradientDescentOptimizer : 
        public SingleValuedNonLinearOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GradientDescentOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef SingleValuedNonLinearOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Cost Function  typedef.
   */
  typedef          TCostFunction                CostFunctionType;
  typedef typename CostFunctionType::Pointer    CostFunctionPointer;

  /**
   * Dimension of the Search Space
   */
  enum { SpaceDimension = TCostFunction::SpaceDimension };
 

  /**
   *  Parameters type.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType ParametersType;


  /**
   *  Measure type.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType MeasureType;


  /**
   *  Derivative type.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType DerivativeType;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( GradientDescentOptimizer, 
      SingleValuedNonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Codes of stopping conditions
   */
  typedef enum {
    MaximumNumberOfIterations
  } StopConditionType;

  
  /**
   * Select to Minimize the cost function
   */
  void    SetMinimize(void) 
              { m_Maximize = false; }

  /**
   * Select to Maximize the cost function
   */
  void    SetMaximize(void)
              { m_Maximize = true; }

  /**
   * Advance one step following the gradient direction
   */
  void    AdvanceOneStep( void );

  /**
   * Start Optimization
   */
  void    StartOptimization( void );

  /**
   * Resume previously stopped optimization with current parameters
   * \sa StopOptimization
   */
  void    ResumeOptimization( void );

  /**
   * Stop optimization
   * \sa ResumeOptimization
   */
  void    StopOptimization( void );

  itkSetMacro( LearningRate, double );
  itkSetMacro( NumberOfIterations, unsigned long );

  itkGetConstMacro( LearningRate, double);
  itkGetConstMacro( NumberOfIterations, unsigned long );
  itkGetConstMacro( CurrentNumberOfIterations, unsigned int );

  void SetScale( const ParametersType & scale )
          { m_Scale = scale; this->Modified(); }

  itkSetObjectMacro( CostFunction, CostFunctionType );

protected:

  GradientDescentOptimizer();
  virtual ~GradientDescentOptimizer() {};
  GradientDescentOptimizer(const Self&) {}
  void operator=(const Self&) {}

private:

  DerivativeType                m_Gradient; 
  ParametersType                m_Scale;

  bool                          m_Stop;
  bool                          m_Maximize;
  double                        m_Value;
  StopConditionType             m_StopCondition;
  double                        m_LearningRate;
  unsigned long                 m_NumberOfIterations;
  unsigned long                 m_CurrentNumberOfIterations;

  CostFunctionPointer           m_CostFunction;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDescentOptimizer.txx"
#endif

#endif



