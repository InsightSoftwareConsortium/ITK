/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
    RegularStepGradientMagnitudeTolerance,
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
  ParametersType                m_PreviousRegularStepGradient; 
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



