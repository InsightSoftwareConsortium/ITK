/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleClassifierWithMask.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleClassifierWithMask_h
#define __itkSampleClassifierWithMask_h

#include <vector>

#include "itkObject.h"
#include "itkExceptionObject.h"
#include "itkSubsample.h"
#include "itkMembershipSample.h"
#include "itkSampleClassifier.h"

namespace itk{ 
  namespace Statistics{

/** \class SampleClassifierWithMask 
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

template< class TSample, class TMaskSample >
class ITK_EXPORT SampleClassifierWithMask : 
      public SampleClassifier< TSample >
{
public:
  /** Standard class typedef*/
  typedef SampleClassifierWithMask Self;
  typedef SampleClassifier< TSample > Superclass;
  typedef SmartPointer< Self > Pointer;

 /** Standard macros */
  itkTypeMacro(SampleClassifierWithMask, SampleClassifier);
  itkNewMacro(Self) ;

  /** Superclass typedefs */
  typedef typename Superclass::OutputType OutputType ;
  typedef typename Superclass::ClassLabelType ClassLabelType ;
  typedef typename Superclass::ClassLabelVectorType ClassLabelVectorType ;

  /** typedefs from TSample object */
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** typedefs from Superclass */
  typedef typename Superclass::MembershipFunctionPointerVector 
    MembershipFunctionPointerVector ;

  void SetMask( TMaskSample* mask ) ;

  TMaskSample* GetMask()
  { return m_Mask.GetPointer() ; }

  void SetSelectedClassLabels( ClassLabelVectorType& labels)
  { m_SelectedClassLabels = labels ; }

  void SetOtherClassLabel( ClassLabelType label) 
  { m_OtherClassLabel = label ; }
 
protected:
  SampleClassifierWithMask() ;
  virtual ~SampleClassifierWithMask() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Starts the classification process */
  void GenerateData() ;

private:
  /** Mask sample pointer*/
  typename TMaskSample::Pointer m_Mask ;
  ClassLabelVectorType m_SelectedClassLabels ;
  ClassLabelType m_OtherClassLabel ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleClassifierWithMask.txx"
#endif

#endif







