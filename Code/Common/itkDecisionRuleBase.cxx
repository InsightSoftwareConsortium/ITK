/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecisionRuleBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDecisionRuleBase.h"

namespace itk
{

DecisionRuleBase
::DecisionRuleBase() 
{
}

DecisionRuleBase
::~DecisionRuleBase()
{
}

void
DecisionRuleBase
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
}// end PrintSelf

} // namespace itk







