/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExhaustiveOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExhaustiveOptimizer_h
#define __itkExhaustiveOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
  
/** \class ExhaustiveOptimizer
 * \brief Optimizer that fully samples a grid on the parametric space.
 *
 * This optimizer is equivalent to an exahaustive search in a discrete grid
 * defined over the parametric space. The grid parameters are define through an
 * array of minimum and maximum values. The subdivisions of the grid along each
 * one of the dimensions of the parametric space is defined by an array of
 * number of steps.
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT ExhaustiveOptimizer : 
                    public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef ExhaustiveOptimizer      Self;
  typedef SingleValuedNonLinearOptimizer               Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;
  
  typedef Array< unsigned long > StepsType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( ExhaustiveOptimizer,           SingleValuedNonLinearOptimizer   );
 
  virtual void    StartOptimization( void );

  void StartWalking( void );
  void ResumeWalking( void );
  void StopWalking(void);
  
  itkSetMacro( StepLength, double );
  itkSetMacro( NumberOfSteps, StepsType );
  itkGetConstReferenceMacro( StepLength, double );
  itkGetConstReferenceMacro( NumberOfSteps, StepsType );
  itkGetConstReferenceMacro( CurrentValue, MeasureType );
  itkGetConstReferenceMacro( MaximumMetricValue, MeasureType );
  itkGetConstReferenceMacro( MinimumMetricValue, MeasureType );
  itkGetConstReferenceMacro( MinimumMetricValuePosition, ParametersType );
  itkGetConstReferenceMacro( MaximumMetricValuePosition, ParametersType );
  itkGetConstReferenceMacro( CurrentIndex, ParametersType );
  itkGetConstReferenceMacro( MaximumNumberOfIterations, unsigned long );


protected:
  ExhaustiveOptimizer();
  virtual ~ExhaustiveOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Advance one step following the gradient direction
   * This method verifies if a change in direction is required
   * and if a reduction in steplength is required. */
  void AdvanceOneStep( void );
  void IncrementIndex( ParametersType & param );


  
private:  
  ExhaustiveOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&);//purposely not implemented

protected:
  MeasureType          m_CurrentValue;
  StepsType            m_NumberOfSteps;
  unsigned long        m_CurrentIteration;
  bool                 m_Stop;
  unsigned int         m_CurrentParameter;
  double               m_StepLength; 
  ParametersType       m_CurrentIndex;
  unsigned long        m_MaximumNumberOfIterations;
  MeasureType          m_MaximumMetricValue;
  MeasureType          m_MinimumMetricValue;
  ParametersType       m_MinimumMetricValuePosition;
  ParametersType       m_MaximumMetricValuePosition;
  
};

} // end namespace itk



#endif
    
 
