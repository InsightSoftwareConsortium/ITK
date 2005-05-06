/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpectationMaximizationMixtureModelEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExpectationMaximizationMixtureModelEstimator_h
#define __itkExpectationMaximizationMixtureModelEstimator_h

#include "itkMembershipFunctionBase.h"
#include "itkMixtureModelComponentBase.h"

namespace itk{ 
namespace Statistics{

/** \class ExpectationMaximizationMixtureModelEstimator 
 *  \brief This class generates the parameter estimates for a mixture
 *  model using expectation maximization strategy.
 *
 * The first template argument is the type of the target sample
 * data. This estimator expects one or more mixture model component
 * objects of the classes derived from the
 * MixtureModelComponentBase. The actual component (or module)
 * parameters are updated by each component. Users can think this
 * class as a strategy or a integration point for the EM
 * procedure. The initial proportion (SetInitialProportions), the
 * input sample (SetSample), the mixture model components
 * (AddComponent), and the maximum iteration (SetMaximumIteration) are
 * required. The EM procedure terminates when the current iteration
 * reaches the maximum iteration or the model parameters converge.
 *
 * \sa MixtureModelComponentBase, GaussianMixtureModelComponent
 */

template< class TSample >
class ITK_EXPORT ExpectationMaximizationMixtureModelEstimator : public Object
{
public:
  /** Standard class typedef*/
  typedef ExpectationMaximizationMixtureModelEstimator Self;
  typedef Object Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard macros */
  itkTypeMacro(ExpectationMaximizationMixtureModelEstimator,
               Object);
  itkNewMacro(Self) ;

  /** Length constant */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** TSample template argument related typedefs */
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  /** Type of the mixture model component base class*/
  typedef MixtureModelComponentBase< TSample > ComponentType ;

  /** Type of the component pointer storage */ 
  typedef std::vector< ComponentType* > ComponentVectorType ;

  /** Type of the membership function base class */
  typedef MembershipFunctionBase< MeasurementVectorType > 
  ComponentMembershipFunctionType ;

  /** Type of the array of the proportion values */
  typedef Array< double > ProportionVectorType ;

  /** Sets the target data that will be classified by this */
  void SetSample(const TSample* sample) ;

  /** Returns the target data */
  const TSample* GetSample() const;

  /** Set/Gets the initial proportion values. The size of proportion
   * vector should be same as the number of component (or classes) */
  void SetInitialProportions(ProportionVectorType &propotion) ;
  ProportionVectorType* GetInitialProportions() ;

  /** Gets the result proportion values */
  ProportionVectorType* GetProportions() ;

  /** Set/Gets the maximum number of iterations. When the optimization
   * process reaches the maximum number of interations, even if the
   * class parameters aren't converged, the optimization process
   * stops. */
  void SetMaximumIteration(int numberOfIterations) ;
  int GetMaximumIteration() ;

  /** Gets the current iteration. */
  int GetCurrentIteration() 
  { return m_CurrentIteration ; }

  /** Adds a new component (or class). */
  int AddComponent(ComponentType* component) ;

  /** Gets the total number of classes currently plugged in. */
  unsigned int GetNumberOfComponents() ;

  /** Runs the optimization process. */
  void Update() ;

  /** Termination status after running optimization */
  enum TERMINATION_CODE { CONVERGED = 0, NOT_CONVERGED = 1 } ;

  /** Gets the termination status */
  TERMINATION_CODE GetTerminationCode() ;

  /** Gets the membership function specified by componentIndex
  argument. */
  ComponentMembershipFunctionType* GetComponentMembershipFunction(int componentIndex) ;

protected:
  ExpectationMaximizationMixtureModelEstimator() ;
  virtual ~ExpectationMaximizationMixtureModelEstimator() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;

  bool CalculateDensities() ;
  double CalculateExpectation() ;
  bool UpdateComponentParameters() ;
  bool UpdateProportions() ;

  /** Starts the estimation process */
  void GenerateData() ;

private:
  /** Target data sample pointer*/
  const TSample* m_Sample ;

  int m_MaxIteration ;
  int m_CurrentIteration ;
  TERMINATION_CODE m_TerminationCode ;
  ComponentVectorType m_ComponentVector ;
  ProportionVectorType m_InitialProportions ;
  ProportionVectorType m_Proportions ;
} ; // end of class


} // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExpectationMaximizationMixtureModelEstimator.txx"
#endif

#endif







