/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTableLookupClassifier.h
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
#ifndef __itkTableLookupClassifier_h
#define __itkTableLookupClassifier_h

#include <vector>

#include "itkGenericClassifier.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk{ 
namespace Statistics{

/** \class TableLookupClassifier 
 *  \brief Integration point for MembershipCalculator, DecisionRule, and 
 * target sample data with a pre-calculated look up table.
 *
 * This classifier is identical to the GenericClassifier except 
 * that it creates a look-up table which stores a mapping between
 * a measurement vector and its corresponding class labels. After
 * this step, classification of target data becomes a look up operation
 * in this table.
 *
 * For creation of the look-up table, users should provdes the lower-bound
 * upper-bound of the measurement vectors using SetLookupTableLowerBound
 * and SetLookupTableUpperBound methods.
 * 
 * This classifier is meant to be used with measurement-vectors with low
 * dimensionality (maybe 1 or 2). If users have good knowledge of the 
 * possible range of measurement-vectors, by setting the bound fit the range,
 * they might enhance its performenace further.
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
 * The classification result is a MembershipSample.
 */

template< class TSample, class TMembershipCalculator, class TDecisionRule >
class ITK_EXPORT TableLookupClassifier : 
    public GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
{
public:
  /** Standard class typedef*/
  typedef TableLookupClassifier Self;
  typedef GenericClassifier< TSample, TMembershipCalculator, TDecisionRule > Superclass;
  typedef SmartPointer<Self> Pointer;

  /** Standard macros */
  itkTypeMacro(TableLookupClassifier, GenericClassifier);
  itkNewMacro(Self) ;

  /** Common typedefs for classifiers which are Inherited from GenericClassifier class */
  typedef typename Superclass::SampleType SampleType ;
  typedef typename Superclass::SamplePointer SamplePointer ;
  typedef typename Superclass::MembershipCalculatorType MembershipCalculatorType ;
  typedef typename Superclass::MembershipCalculatorPointer MembershipCalculatorPointer ;
  typedef typename Superclass::DecisionRuleType DecisionRuleType ;
  typedef typename Superclass::DecisionRulePointer DecisionRulePointer ;
  typedef typename Superclass::OutputType OutputType ;
  typedef typename Superclass::OutputPointer OutputPointer ;
  typedef typename Superclass::MembershipCalculatorVectorType MembershipCalculatorVectorType ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::MeasurementVectorType measurementVectorType ;

  enum { MeasurementVectorSize = Superclass::MeasurementVectorSize } ;

  /** Lookup table related typedefs */
  typedef Index< MeasurementVectorSize > CachedMeasurementVectorType ;
  typedef Image< MeasurementType, MeasurementVectorSize > LookupTableType ;
  typedef LookupTableType::Pointer LookupTablePointer ;
  typedef ImageRegionIteratorWithIndex< LookupTableType > LookupTableIteratorType ;
  typedef LookupTableType::RegionType RegionType ;
  typedef LookupTableType::SizeType SizeType ;

  /** sets the upper boundary for the lookup table construction */
  void SetLookupTableLowerBound(CachedMeasurementVectorType lower) ;

  /** sets the upper boundary for the lookup table construction */
  void SetLookupTableUpperBound(CachedMeasurementVectorType upper) ;

  /** Starts the classification process */
  void GenerateData() ;

protected:
  TableLookupClassifier() ;
  virtual ~TableLookupClassifier() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void PrepareLookupTable() ;

private:
  LookupTablePointer m_LookupTable ;
  CachedMeasurementVectorType m_UpperBound ;
  CachedMeasurementVectorType m_LowerBound ;
} ; // end of class


} // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTableLookupClassifier.txx"
#endif

#endif







