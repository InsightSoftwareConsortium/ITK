/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDecisionRule.h"

namespace itk
{
namespace Statistics
{
DecisionRule
::DecisionRule()
{}

DecisionRule
::~DecisionRule()
{}

void
DecisionRule
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
} // end PrintSelf
} // end of namespace Statistics
} // end of namespace itk
