/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMinimumDecisionRule.h"

namespace itk
{

MinimumDecisionRule
::MinimumDecisionRule() 
{

}

unsigned int 
MinimumDecisionRule
::Evaluate(std::vector< double > &discriminantScores)
{

  double       minimumDistance      = discriminantScores[0];
  unsigned int classifiedPixelIndex = 0;
  unsigned int numberOfClasses      = static_cast<unsigned int>( discriminantScores.size() );

  //Loop through the probabilities to get the best index
  for(unsigned int classIndex = 1; classIndex < numberOfClasses; classIndex++ )
    {  
    if( discriminantScores[classIndex] < minimumDistance ) 
      {
      minimumDistance      = discriminantScores[classIndex];
      classifiedPixelIndex = classIndex;
      }
    }// end for

  return classifiedPixelIndex;
}


} // namespace itk








