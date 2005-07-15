/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumRatioDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MaximumRatioDecisionRule_h
#define __MaximumRatioDecisionRule_h

#include "itkWin32Header.h"

#include <vector>
#include "vnl/vnl_matrix.h"

#include "itkNumericTraits.h"
#include "itkDecisionRuleBase.h"

namespace itk {

/** \class MaximumRatioDecisionRule
 *  \brief This rule returns  \f$i\f$ if
 *   \f$\frac{f_{i}(\overrightarrow{x})}{f_{j}(\overrightarrow{x})} >
 *   \frac{K_{j}}{K_{i}}\f$ for all \f$j \not= i\f$,
 * where the \f$i\f$ is the index of a class which has 
 * membership function \f$f_{i}\f$ and its prior value 
 * (usually, the a priori probability or the size of a class) is
 * \f$K_{i}\f$
 * 
 * Users should set the a priori values before calling the Evaluate method.
 * 
 * \sa MaximumDecisionRule, MinimumDecisionRule 
 */
 
class ITKCommon_EXPORT MaximumRatioDecisionRule : 
    public DecisionRuleBase
{
public:
  /** Standard class typedefs */ 
  typedef MaximumRatioDecisionRule Self ;
  typedef DecisionRuleBase Superclass;
  typedef SmartPointer<Self> Pointer;
  
  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumRatioDecisionRule, DecisionRuleBase);
  
  /** Standard New() method support */
  itkNewMacro(Self) ;
  
  typedef float APrioriValueType ;
  typedef std::vector< APrioriValueType > APrioriVectorType ;


  /** Types for the arguments that are acceptable in the Evaluate() method */
  typedef Superclass::VectorType  VectorType;
  typedef Superclass::ArrayType   ArrayType;
 

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate( const VectorType &discriminantScores) const;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate( const ArrayType &discriminantScores) const;


  /** Sets the a priori probabilities */
  void SetAPriori(APrioriVectorType& values) ;

protected:
  MaximumRatioDecisionRule() ;
  virtual ~MaximumRatioDecisionRule() {}
  
private:
  /** Number of classes */
  unsigned int m_NumberOfClasses ;

  /** a priori probability ratio matrix: internal use */
  vnl_matrix< double > m_APrioriRatioMatrix ;
} ; // end of class

} // end of namespace
#endif







