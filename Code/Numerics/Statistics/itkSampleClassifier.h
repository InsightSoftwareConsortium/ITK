/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleClassifier_h
#define __itkSampleClassifier_h

#include <vector>

#include "itkObject.h"
#include "itkExceptionObject.h"
#include "itkSubsample.h"
#include "itkMembershipSample.h"
#include "itkClassifierBase.h"

namespace itk{ 
  namespace Statistics{

/** \class SampleClassifier 
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
class ITK_EXPORT SampleClassifier : 
      public ClassifierBase< TSample >
{
public:
  /** Standard class typedef*/
  typedef SampleClassifier Self;
  typedef ClassifierBase< TSample > Superclass;
  typedef SmartPointer< Self > Pointer;

 /** Standard macros */
  itkTypeMacro(SampleClassifier, Object);
  itkNewMacro(Self) ;

  /** TSample template argument related ypedefs */
  typedef TSample SampleType ;
  typedef typename TSample::Pointer SamplePointer ;

  /** Output type for GetClassSample method */
  typedef MembershipSample< TSample > OutputType ;
  typedef typename OutputType::Pointer OutputPointer ;

  /** typedefs from TSample object */
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** typedefs from Superclass */
  typedef typename Superclass::MembershipFunctionPointerVector 
    MembershipFunctionPointerVector ;

  /** Sets the target data that will be classified by this */
  void SetSample(SamplePointer sample) ;

  /** Returns the target data */
  SamplePointer GetSample() ;

  /** Sets the user given class labels for membership functions.
   * If users do not provide class labels for membership functions by calling
   * this function, then the index of the membership function vector for a
   * membership function will be used as class label of measurement vectors
   * belong to the membership function */ 
  void SetMembershipFunctionClassLabels(std::vector< unsigned int > labels) ;

  /** Gets the user given class labels */
  std::vector< unsigned int > GetMembershipFunctionClassLabels() 
  { return m_ClassLabels ; }

  /** Returns the classification result */
  OutputPointer GetOutput() ;

protected:
  SampleClassifier() ;
  virtual ~SampleClassifier() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Starts the classification process */
  void GenerateData() ;
private:
  /** Target data sample pointer*/
  SamplePointer m_Sample ;

  /** Output pointer (MembershipSample) */
  OutputPointer m_Output ;

  /** User given class labels for membership functions */
  std::vector< unsigned int > m_ClassLabels ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleClassifier.txx"
#endif

#endif







