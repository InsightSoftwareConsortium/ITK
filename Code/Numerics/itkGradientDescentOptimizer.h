/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkGradientDescentOptimizer_h
#define __itkGradientDescentOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
  
/** \class GradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
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
   * ParametersType typedef.
   */
  typedef typename TCostFunction::ParametersType    ParametersType;
  typedef typename ParametersType::Pointer          ParametersPointer;



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
   * Start optimization with an initial value
   */
  void StartOptimization( ParametersPointer &);


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
  void    SetMinimize(void);

  /**
   * Select to Maximize the cost function
   */
  void    SetMaximize(void);

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

  /**
   * Define the minimum step size 
   * this is used as a stopping condition
   */
  void    SetMinimumStepSize( double step_size );

  /**
   * Define the maximum step size 
   * this is a superior bound to the step in the gradient direction
   * it is also the initial default value for the step
   */
  void    SetMaximumStepSize( double step_size );

  itkSetMacro( MaximumStepLength, double );
  itkSetMacro( MinimumStepLength, double );
  itkSetMacro( MaximumNumberOfIterations, unsigned long );

  itkGetConstMacro( CurrentStepLength, double);
  itkGetConstMacro( MaximumStepLength, double );
  itkGetConstMacro( MinimumStepLength, double );
  itkGetConstMacro( MaximumNumberOfIterations, unsigned long );

  itkSetObjectMacro( CostFunction, CostFunctionType );

protected:

  GradientDescentOptimizer();
  virtual ~GradientDescentOptimizer() {};
  GradientDescentOptimizer(const Self&) {}
  void operator=(const Self&) {}

private:

  ParametersPointer             m_Gradient; 
  ParametersPointer             m_PreviousGradient; 
  ParametersPointer             m_StepSize;

  bool                          m_Stop;
  bool                          m_Maximize;
  double                        m_Value;
  double                        m_GradientMagnitudeTolerance;
  double                        m_MaximumStepLength;
  double                        m_MinimumStepLength;
  double                        m_CurrentStepLength;
  StopConditionType             m_StopCondition;
  unsigned long                 m_MaximumNumberOfIterations;
  unsigned long                 m_CurrentNumberIterations;

  CostFunctionPointer           m_CostFunction;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDescentOptimizer.txx"
#endif

#endif



