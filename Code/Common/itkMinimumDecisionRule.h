/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
 
class ITK_EXPORT MinimumDecisionRule : 
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
  
  unsigned int Evaluate(std::vector< double > &discriminantScores) ;

 protected:
  MinimumDecisionRule() ;
  virtual ~MinimumDecisionRule() {}
  
 private:


} ; // end of class

} // namespace itk

#endif







