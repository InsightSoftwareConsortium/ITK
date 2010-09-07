/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumDecisionRule2.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaximumDecisionRule2_h
#define __itkMaximumDecisionRule2_h

#include "itkDecisionRule.h"

namespace itk
{
namespace Statistics
{
/** \class MaximumDecisionRule2
 *  \brief A Decision rule that choose the class of which discriminant
 *  score is the largest. This class will replace the MaximumDecisionRule
 *  in Code/Common
 */

class ITK_EXPORT MaximumDecisionRule2:public DecisionRule
{
public:
  /** Standard class typedefs */
  typedef MaximumDecisionRule2       Self;
  typedef DecisionRule               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumDecisionRule2, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const MembershipVectorType & discriminantScores) const;

protected:
  MaximumDecisionRule2() {}
  virtual ~MaximumDecisionRule2() {}
  void PrintSelf(std::ostream & os, Indent indent) const;
};  // end of class
} // end of namespace itk
} // end of namespace Statistics

#endif
