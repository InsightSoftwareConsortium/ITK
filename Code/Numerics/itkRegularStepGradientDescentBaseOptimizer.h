/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentBaseOptimizer.h
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
#ifndef __itkRegularStepGradientDescentBaseOptimizer_h
#define __itkRegularStepGradientDescentBaseOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
  
/** \class RegularStepGradientDescentBaseOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT RegularStepGradientDescentBaseOptimizer : 
        public SingleValuedNonLinearOptimizer< TCostFunction >
{
public:
  /** Standard "Self" typedef. */
  typedef RegularStepGradientDescentBaseOptimizer  Self;
  typedef SingleValuedNonLinearOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( RegularStepGradientDescentBaseOptimizer, 
      SingleValuedNonLinearOptimizer );

  /** Cost function typedefs. */
  typedef          TCostFunction                CostFunctionType;
  typedef typename CostFunctionType::Pointer    CostFunctionPointer;
  
  /** Dimension of the search space. */
  enum { SpaceDimension = TCostFunction::SpaceDimension };

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType ParametersType;

  /**  Measure type.
   * It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType MeasureType;

  /**  Derivative type.
   * It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType DerivativeType;

  /** Codes of stopping conditions. */
  typedef enum {
    GradientMagnitudeTolerance=1,
    StepTooSmall,
    ImageNotAvailable,
    SamplesNotAvailable,
    MaximumNumberOfIterations
  } StopConditionType;

  /** Specify whether to minimize or maximize the cost function. */
  itkSetMacro( Maximize, bool );
  itkGetMacro( Maximize, bool );
  itkBooleanMacro( Maximize );
  bool GetMinimize( ) const
    { return !m_Maximize; }
  void SetMinimize(bool v)
    { this->SetMaximize(!v); }
  void    MinimizeOn(void) 
    { SetMaximize( false ); }
  void    MinimizeOff(void) 
    { SetMaximize( true ); }
  
  /** Test a precondition. */
  bool    Precondition( void );

  /** Start optimization. */
  void    StartOptimization( void );

  /** Resume previously stopped optimization with current parameters.
   * \sa StopOptimization */
  void    ResumeOptimization( void );

  /** Stop optimization.
   * \sa ResumeOptimization */
  void    StopOptimization( void );

  /** Set/Get parameters to control the optimization process. */
  itkSetMacro( MaximumStepLength, double );
  itkSetMacro( MinimumStepLength, double );
  itkSetMacro( NumberOfIterations, unsigned long );
  itkSetMacro( GradientMagnitudeTolerance, double );
  itkGetConstMacro( CurrentStepLength, double);
  itkGetConstMacro( MaximumStepLength, double );
  itkGetConstMacro( MinimumStepLength, double );
  itkGetConstMacro( NumberOfIterations, unsigned long );
  itkGetConstMacro( GradientMagnitudeTolerance, double );
  itkGetConstMacro( CurrentIteration, unsigned int );
  itkGetConstMacro( StopCondition, StopConditionType );
  itkGetConstMacro( Value, MeasureType );
  
  /** Set/Get the cost function to optimize over. */
  itkSetObjectMacro( CostFunction, CostFunctionType );
  itkGetObjectMacro( CostFunction, CostFunctionType );
  
protected:
  RegularStepGradientDescentBaseOptimizer();
  virtual ~RegularStepGradientDescentBaseOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Advance one step following the gradient direction
   * This method verifies if a change in direction is required
   * and if a reduction in steplength is required. */
  virtual void AdvanceOneStep( void );

  /** Advance one step along the corrected gradient taking into
   * account the steplength represented by factor.
   * This method is invoked by AdvanceOneStep. It is expected
   * to be overrided by optimization methods in non-vector spaces
   * \sa AdvanceOneStep */
  virtual void StepAlongGradient( 
                  double factor, 
                  const DerivativeType & transformedGradient ) = 0;


private:  
  RegularStepGradientDescentBaseOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&);//purposely not implemented

protected:
  DerivativeType                m_Gradient; 
  DerivativeType                m_PreviousGradient; 

  bool                          m_Stop;
  bool                          m_Maximize;
  MeasureType                   m_Value;
  double                        m_GradientMagnitudeTolerance;
  double                        m_MaximumStepLength;
  double                        m_MinimumStepLength;
  double                        m_CurrentStepLength;
  StopConditionType             m_StopCondition;
  unsigned long                 m_NumberOfIterations;
  unsigned long                 m_CurrentIteration;

  CostFunctionPointer           m_CostFunction;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularStepGradientDescentBaseOptimizer.txx"
#endif

#endif



