/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.h
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
#ifndef __itkRegularStepGradientDescentOptimizer_h
#define __itkRegularStepGradientDescentOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
  
/** \class RegularStepGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT RegularStepGradientDescentOptimizer : 
        public SingleValuedNonLinearOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegularStepGradientDescentOptimizer  Self;

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
  itkTypeMacro( RegularStepGradientDescentOptimizer, 
      SingleValuedNonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  


  /**
   * Codes of stopping conditions
   */
  typedef enum {
    GradientMagnitudeTolerance,
    StepTooSmall,
    ImageNotAvailable,
    SamplesNotAvailable,
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
   * Test a precondition
   */
  bool    Precondition( void );

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

  itkSetMacro( MaximumStepLength, double );
  itkSetMacro( MinimumStepLength, double );
  itkSetMacro( MaximumNumberOfIterations, unsigned long );
  itkSetMacro( GradientMagnitudeTolerance, double );

  itkGetConstMacro( CurrentStepLength, double);
  itkGetConstMacro( MaximumStepLength, double );
  itkGetConstMacro( MinimumStepLength, double );
  itkGetConstMacro( MaximumNumberOfIterations, unsigned long );
  itkGetConstMacro( GradientMagnitudeTolerance, double );
  itkGetConstMacro( CurrentNumberOfIterations, unsigned int );
  itkGetConstMacro( StopCondition, StopConditionType );

  void SetScale( const ParametersType & scale )
          { m_Scale = scale; this->Modified(); }

  itkSetObjectMacro( CostFunction, CostFunctionType );

protected:

  RegularStepGradientDescentOptimizer();
  virtual ~RegularStepGradientDescentOptimizer() {};
  RegularStepGradientDescentOptimizer(const Self&) {}
  void operator=(const Self&) {}

private:

  ParametersType                m_Gradient; 
  ParametersType                m_PreviousGradient; 
  ParametersType                m_Scale;

  bool                          m_Stop;
  bool                          m_Maximize;
  double                        m_Value;
  double                        m_GradientMagnitudeTolerance;
  double                        m_MaximumStepLength;
  double                        m_MinimumStepLength;
  double                        m_CurrentStepLength;
  StopConditionType             m_StopCondition;
  unsigned long                 m_MaximumNumberOfIterations;
  unsigned long                 m_CurrentNumberOfIterations;

  CostFunctionPointer           m_CostFunction;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularStepGradientDescentOptimizer.txx"
#endif

#endif



