/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaxDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMaxDecisionRule.h"

namespace itk{ 
  namespace Statistics{


unsigned int MaxDecisionRule::Evaluate(std::vector< double > discriminantScores)
{
  double max = discriminantScores[0] ;
  unsigned int maxIndex = 0 ;
  unsigned int i ;
  for (i = 0 ; i < discriminantScores.size() ; i++)
    {
      if (discriminantScores[i] > max) 
        {
          max = discriminantScores[i] ;
          maxIndex = i ;
        }
    }
  return maxIndex ;
}

void MaxDecisionRule::PrintSelf(std::ostream& os, Indent indent) const 
{ 
  Superclass::PrintSelf(os, indent) ; 
}
 
  } // end of namespace Statistics 
} // end of namespace itk







