/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaxDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaxDecisionRule_h
#define __itkMaxDecisionRule_h

#include <vector>
#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk{ 
  namespace Statistics{

/** \class MaxDecisionRule
 *  \brief A Decision rule that choose the class that has maximum value
 */
 
class ITK_EXPORT MaxDecisionRule : 
      public Object
{
public:
 /** Standard class typedefs */ 
  typedef MaxDecisionRule Self ;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;

 /** Run-time type information (and related methods) */
  itkTypeMacro(MaxDecisionRule, Object);

  /** Standard New() method support */
  itkNewMacro(Self) ;

  unsigned int Evaluate(std::vector< double > discriminantScores) ;

protected:
  MaxDecisionRule() {}
  virtual ~MaxDecisionRule() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk

#endif







