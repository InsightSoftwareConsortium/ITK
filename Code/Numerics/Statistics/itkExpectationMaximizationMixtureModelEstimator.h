/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpectationMaximizationMixtureModelEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
 *  \brief Integration point for MembershipCalculator, DecisionRule, and 
 * target sample data.
 *
 * The first template argument is the type of the target sample data 
 * that this classifier will assign a class label for each measurement 
 * vector. The second one is the type of a membership value calculator
 * for each. A membership calculator represents a specific knowledge about
 * a class. In other words, it should tell us how "likely" is that a
 * measurement vector (pattern) belong to the class. The third argument
 * is the type of decision rule. The main role of a decision rule is 
 * comparing the return values of the membership calculators. However,
 * decision rule can include some prior knowledge that can improve the
 * result. 
 *
 * Before you call the GenerateData method to start the classification process, 
 * you should plug in all necessary parts ( one or more membership 
 * calculators, a decision rule, and a target sample data). To plug in 
 * the decision rule, you use SetDecisionRule method, for the target sample
 * data, SetSample method, and for the membership calculators, use 
 * AddMembershipCalculator method.
 *
 * As the method name indicates, you can have more than one membership 
 * calculator. One for each classes. The order you put the membership 
 * calculator becomes the class label for the class that is represented
 * by the membership calculator.
 *
 * The classification result is stored in a vector of Subsample object.
 * Each class has its own class sample (Subsample object) that has 
 * InstanceIdentifiers for all measurement vectors belong to the class. 
 * The InstanceIdentifiers come from the target sample data. Therefore,
 * the Subsample objects act as separate class masks. 
 */

template< class TSample >
class ITK_EXPORT ExpectationMaximizationMixtureModelEstimator : public Object
{
public:
  /** Standard class typedef*/
  typedef ExpectationMaximizationMixtureModelEstimator Self;
  typedef Object Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Standard macros */
  itkTypeMacro(ExpectationMaximizationMixtureModelEstimator,
               Object);
  itkNewMacro(Self) ;

  /** Length constant */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** TSample template argument related typedefs */
  typedef TSample SampleType ;
  typedef typename TSample::Pointer SamplePointer ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  typedef MixtureModelComponentBase< TSample > ComponentType ;
  typedef typename ComponentType::Pointer ComponentPointer ;
  typedef std::vector< ComponentPointer > ComponentVectorType ;
  typedef MembershipFunctionBase< MeasurementVectorType > ComponentMembershipFunctionType ;
  typedef typename ComponentMembershipFunctionType::Pointer ComponentMembershipFunctionPointer ;

  /** Sets the target data that will be classified by this */
  void SetSample(SamplePointer sample) ;

  /** Returns the target data */
  SamplePointer GetSample() ;

  typedef Array< double > ProportionVectorType ;

  void SetInitialProportions(ProportionVectorType &propotion) ;

  ProportionVectorType* GetInitialProportions() ;

  ProportionVectorType* GetProportions() ;

  void SetMaximumIteration(int numberOfIterations) ;
  
  int GetMaximumIteration() ;

  int GetCurrentIteration() 
  { return m_CurrentIteration ; }

  int AddComponent(ComponentPointer component) ;

  int GetNumberOfComponents() ;

  void Update() ;

  enum TERMINATION_CODE { CONVERGED = 0, NOT_CONVERGED = 1 } ;

  TERMINATION_CODE GetTerminationCode() ;
  ComponentMembershipFunctionPointer GetComponentMembershipFunction(int componentIndex) ;

protected:
  ExpectationMaximizationMixtureModelEstimator() ;
  virtual ~ExpectationMaximizationMixtureModelEstimator() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;

  bool CalculateDensities() ;
  double CalculateExpectation() ;
  bool UpdateComponentParameters() ;
  bool UpdateProportions() ;

  /** Starts the classification process */
  void GenerateData() ;

private:
  /** Target data sample pointer*/
  SamplePointer m_Sample ;

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







