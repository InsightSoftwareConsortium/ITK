/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGenericClassifier.h
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
#ifndef __itkGenericClassifier_h
#define __itkGenericClassifier_h

#include <vector>

#include "itkObject.h"
#include "itkExceptionObject.h"
#include "itkSubsample.h"

namespace itk{ 
  namespace Statistics{

/** \class GenericClassifier 
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

template< class TSample, class TMembershipCalculator, class TDecisionRule >
class ITK_EXPORT GenericClassifier : 
      public Object
{
public:
  /** Standard class typedef*/
  typedef GenericClassifier Self;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;

 /** Standard macros */
  itkTypeMacro(GenericClassifier, Object);
  itkNewMacro(Self) ;

  /** TSample template argument related typedefs */
  typedef TSample SampleType ;
  typedef typename TSample::Pointer SamplePointer ;

  /** TMembershipCalculator template argument related typedefs */
  typedef TMembershipCalculator MembershipCalculatorType ;
  typedef typename TMembershipCalculator::Pointer MembershipCalculatorPointer ;

  /** TDecisionRule template argument related typedefs */
  typedef TDecisionRule DecisionRuleType ;
  typedef typename TDecisionRule::Pointer DecisionRulePointer ;

  /** Output type for GetClassSample method */
  typedef Subsample< TSample > ClassSampleType ;
  typedef ClassSampleType::Pointer ClassSamplePointer ;

  /** Container of pointers of the subsamples for all classes */ 
  typedef std::vector< ClassSamplePointer > ClassSampleVectorType ;

  /** Container of the pointers of MembershipCalculator objects */
  typedef std::vector< MembershipCalculatorPointer > 
    MembershipCalculatorVectorType ;

  /** Sets the target data that will be classified by this */
  void SetSample(SamplePointer sample) ;

  /** Returns the target data */
  SamplePointer GetSample() ;

  /** Returns a subsample that has instance identifiers of all patterns 
   * that belong to the class for the classLabel */ 
  ClassSamplePointer GetClassSample(unsigned int classLabel) 
    throw (ExceptionObject) ;

  /** Stores a MembershipCalculator of a class in its internal vector */
  unsigned int AddMembershipCalculator(MembershipCalculatorPointer function) ;

  /** Stores the decision rule that makes the real decision using 
   * informations from MembershipCalculators and other prior knowledge */
  void SetDecisionRule(DecisionRulePointer decisionRule) ;

  /** Returns the DecisionRule object pointer */
  DecisionRulePointer GetDecisionRule() ;

  /** Starts the classification process */
  void GenerateData() ;

  /** Returns the classification result */
  ClassSampleVectorType GetOutput() ;

protected:
  GenericClassifier() {}
  virtual ~GenericClassifier() {}
  /** Creates subclasses that holds the classification result for each class
   * and stores their pointer as many as the "numberOfClasses" */ 
  void PrepareClassSampleVector(unsigned int numberOfClasses) ;

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  /** Target data sample pointer*/
  SamplePointer m_Sample ;

  /** DecisionRule poitner */
  DecisionRulePointer m_DecisionRule ;

  /** Container for MembershipCalculators' pointers */
  MembershipCalculatorVectorType m_MembershipCalculators ;
  
  /** Classification results storage */
  ClassSampleVectorType m_ClassSamples ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGenericClassifier.txx"
#endif

#endif







