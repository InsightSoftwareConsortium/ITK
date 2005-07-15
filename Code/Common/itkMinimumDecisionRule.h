/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MinimumDecisionRule_h
#define __MinimumDecisionRule_h

#include "itkWin32Header.h"

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkDecisionRuleBase.h"

namespace itk
{

/** \class MinimumDecisionRule
 *  \brief A Decision rule that choose the class that has minimum value
 */
 
class ITKCommon_EXPORT MinimumDecisionRule : 
  public DecisionRuleBase
{
 public:
  /** Standard class typedefs */ 
  typedef MinimumDecisionRule Self ;
  typedef DecisionRuleBase Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  
  /** Run-time type information (and related methods) */
  itkTypeMacro(MinimumDecisionRule, DecisionRuleBase);
  
  /** Standard New() method support */
  itkNewMacro(Self) ;
  

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


 protected:
  MinimumDecisionRule() ;
  virtual ~MinimumDecisionRule() {}
  
 private:


} ; // end of class

} // namespace itk

#endif







