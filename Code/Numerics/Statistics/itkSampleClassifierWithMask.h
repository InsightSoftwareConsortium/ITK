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
 * target sample data. This class is functionally identical to the 
 * SampleClassifier, except that users can perform only part of the
 * input sample that belongs to the subset of classes. 
 * 
 * To this purpose, this class needs a class mask sample that has
 * class labels as measurement vectors. Using SetMask method, users can
 * provide the class mask sample.
 *
 * To specify which classes should be included for classification, users
 * must call SetSelectedClassLabels method with class labels that will be
 * included. All measurement vectors that belong to the non-selected
 * classes will be classified to the class label that has been given 
 * by the SetOtherClassLabel method.
 *
 * Except for the modifications mentioned above, the basic behavior and
 * methods are identical to those of SampleClassifier.
 * 
 * \sa SampleClassifier
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







