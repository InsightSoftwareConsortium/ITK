/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumDecisionRuleTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include <vector>

#include "itkMinimumDecisionRule.h"


int itkMinimumDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MinimumDecionRule " << std::endl << std::endl;

  typedef itk::MinimumDecisionRule  DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::vector< double > discriminantScores ;
  discriminantScores.resize( 3 ) ;
  
  discriminantScores[0] = 0.0 ;
  discriminantScores[1] = 1.0 ;
  discriminantScores[2] = 2.0 ;

  if ( decisionRule->Evaluate( discriminantScores ) != 0 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }

  std::cout << "[SUCCEEDED]" << std::endl ;
  return EXIT_SUCCESS;
}
