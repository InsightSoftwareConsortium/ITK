/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaximumDecisionRule_h
#define __itkMaximumDecisionRule_h

#include "itkWin32Header.h"

#include <vector>
#include "itkDecisionRuleBase.h"

namespace itk{ 

/** \class MaximumDecisionRule
 *  \brief A Decision rule that choose the class that has maximum value
 */
 
class ITKCommon_EXPORT MaximumDecisionRule : 
      public DecisionRuleBase
{
public:
 /** Standard class typedefs */ 
  typedef MaximumDecisionRule Self ;
  typedef DecisionRuleBase Superclass;
  typedef SmartPointer<Self> Pointer;

 /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumDecisionRule, DecisionRuleBase);

  /** Standard New() method support */
  itkNewMacro(Self) ;

  unsigned int Evaluate(std::vector< double > &discriminantScores) ;

protected:
  MaximumDecisionRule() {}
  virtual ~MaximumDecisionRule() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;
} ; // end of class

inline unsigned int MaximumDecisionRule::Evaluate(std::vector< double > 
                                                  &discriminantScores)
{
  double max = discriminantScores[0] ;
  unsigned int maxIndex = 0 ;
  unsigned int i ;
  for (i = 1 ; i < discriminantScores.size() ; i++)
    {
      if (discriminantScores[i] > max) 
        {
          max = discriminantScores[i] ;
          maxIndex = i ;
        }
    }
  return maxIndex ;
}

} // end of namespace itk

#endif







