/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \f[ 
 *        p_{n+1} = p_n 
 *                + \mbox{learningRate} 
                  \, \frac{\partial f(p_n) }{\partial p_n} 
 * \f]
 *
 * The learning rate is a fixed scalar defined via SetLearningRate().
 * The optimizer steps through a user defined number of iterations;
 * no convergence checking is done.
 *
 * Additionally, user can scale each component of the df / dp
 * but setting a scaling vector using method SetScale().
 *
 * \sa RegularStepGradientDescentOptimizer
 * 
 * \ingroup Numerics Optimizers
 */  
class ITK_EXPORT GradientDescentOptimizer : 
    public SingleValuedNonLinearOptimizer
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizer          Self;
  typedef SingleValuedNonLinearOptimizer    Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GradientDescentOptimizer, SingleValuedNonLinearOptimizer );


  /** Codes of stopping conditions */
  typedef enum {
    MaximumNumberOfIterations,
    MetricError
  } StopConditionType;

  /** Methods to configure the cost function. */
  itkGetMacro( Maximize, bool );
  itkSetMacro( Maximize, bool );
  itkBooleanMacro( Maximize );
  bool GetMinimize( ) const
  { return !m_Maximize; }
  void SetMinimize(bool v)
  { this->SetMaximize(!v); }
  void MinimizeOn()
  { this->MaximizeOff(); }
  void MinimizeOff()
  { this->MaximizeOn(); }
  
  /** Advance one step following the gradient direction. */
  virtual void AdvanceOneStep( void );

  /** Start optimization. */
  void    StartOptimization( void );

  /** Resume previously stopped optimization with current parameters
   * \sa StopOptimization. */
  void    ResumeOptimization( void );

  /** Stop optimization.
   * \sa ResumeOptimization */
  void    StopOptimization( void );

  /** Set the learning rate. */
  itkSetMacro( LearningRate, double );

  /** Get the learning rate. */
  itkGetConstMacro( LearningRate, double);

  /** Set the number of iterations. */
  itkSetMacro( NumberOfIterations, unsigned long );

  /** Get the number of iterations. */
  itkGetConstMacro( NumberOfIterations, unsigned long );

  /** Get the current iteration number. */
  itkGetConstMacro( CurrentIteration, unsigned int );

  /** Get the current value. */
  itkGetConstMacro( Value, double );

  /** Get Stop condition. */
  itkGetConstMacro( StopCondition, StopConditionType );


protected:
  GradientDescentOptimizer();
  virtual ~GradientDescentOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  // made protected so subclass can access
  DerivativeType                m_Gradient; 
  bool                          m_Maximize;
  double                        m_LearningRate;

private:
  GradientDescentOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  bool                          m_Stop;
  double                        m_Value;
  StopConditionType             m_StopCondition;
  unsigned long                 m_NumberOfIterations;
  unsigned long                 m_CurrentIteration;



};

} // end namespace itk


#endif



