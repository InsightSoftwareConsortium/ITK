/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTableLookupSampleClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTableLookupSampleClassifier_h
#define __itkTableLookupSampleClassifier_h

#include <vector>

#include "itkSampleClassifier.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk{ 
namespace Statistics{

/** \class TableLookupSampleClassifier 
 *  \brief Integration point for MembershipCalculator, DecisionRule, and 
 * target sample data with a pre-calculated look up table.
 *
 * This classifier is identical to the SampleClassifier except 
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

template< class TSample >
class ITK_EXPORT TableLookupSampleClassifier : 
    public SampleClassifier< TSample >
{
public:
  /** Standard class typedef*/
  typedef TableLookupSampleClassifier Self;
  typedef SampleClassifier< TSample > Superclass;
  typedef SmartPointer<Self> Pointer;

  /** Standard macros */
  itkTypeMacro(TableLookupSampleClassifier, SampleClassifier);
  itkNewMacro(Self) ;

  /** Common typedefs for classifiers which are Inherited from SampleClassifier class */
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      Superclass::MeasurementVectorSize);

  /** Lookup table related typedefs */
  typedef Index< itkGetStaticConstMacro(MeasurementVectorSize) > CachedMeasurementVectorType ;
  typedef Image< MeasurementType, itkGetStaticConstMacro(MeasurementVectorSize) > LookupTableType ;
  typedef typename LookupTableType::Pointer LookupTablePointer ;
  typedef ImageRegionIteratorWithIndex< LookupTableType > LookupTableIteratorType ;
  typedef typename LookupTableType::RegionType RegionType ;
  typedef typename LookupTableType::SizeType SizeType ;

  /** sets the upper boundary for the lookup table construction */
  void SetLookupTableLowerBound(MeasurementVectorType lower) ;

  /** sets the upper boundary for the lookup table construction */
  void SetLookupTableUpperBound(MeasurementVectorType upper) ;

protected:
  TableLookupSampleClassifier() ;
  virtual ~TableLookupSampleClassifier() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void PrepareLookupTable() ;

  /** Starts the classification process */
  void GenerateData() ;

private:
  LookupTablePointer m_LookupTable ;
  CachedMeasurementVectorType m_UpperBound ;
  CachedMeasurementVectorType m_LowerBound ;
} ; // end of class


} // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTableLookupSampleClassifier.txx"
#endif

#endif







