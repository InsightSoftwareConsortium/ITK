/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumDecisionRule2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMaximumDecisionRule2.h"

namespace itk
{
namespace Statistics
{
void
MaximumDecisionRule2::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

unsigned int
MaximumDecisionRule2::Evaluate(const MembershipVectorType & discriminantScores) const
{
  double       max = discriminantScores[0];
  unsigned int maxIndex = 0;
  unsigned int i;

  for ( i = 1; i < discriminantScores.size(); i++ )
    {
    if ( discriminantScores[i] > max )
      {
      max = discriminantScores[i];
      maxIndex = i;
      }
    }
  return maxIndex;
}
} // end of namespace itk
} // end of namespace Statistics
