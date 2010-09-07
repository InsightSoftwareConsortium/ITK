/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecisionRuleTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkDecisionRule.h"
#include "itkObjectFactory.h"

namespace itk {
namespace Statistics {
namespace DecisionRuleTest {

class MyDecisionRule : public DecisionRule
{
public:
  /** Standard class typedef. */
  typedef MyDecisionRule                   Self;
  typedef DecisionRule                     Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Standard macros */
  itkTypeMacro(MyDecisionRule, DecisionRule);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  unsigned int Evaluate(const MembershipVectorType &scoreVector) const
    {
    double max = scoreVector[0];

    unsigned int maxIndex = 0;
    unsigned int i;
    for (i = 1; i < scoreVector.size(); i++)
      {
      if (scoreVector[i] > max)
        {
        max = scoreVector[i];
        maxIndex = i;
        }
      }
    return maxIndex;
    }
};

}
}
}
int itkDecisionRuleTest(int, char* [] )
{
  typedef itk::Statistics::DecisionRuleTest::MyDecisionRule DecisionRuleType;

  typedef DecisionRuleType::MembershipVectorType MembershipVectorType;

  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::cout << decisionRule->GetNameOfClass() << std::endl;
  std::cout << decisionRule->DecisionRuleType::Superclass::GetNameOfClass() << std::endl;

  decisionRule->Print(std::cout);

  MembershipVectorType membershipScoreVector;

  double membershipScore1;
  membershipScore1 = 0.1;
  membershipScoreVector.push_back( membershipScore1 );

  double membershipScore2;
  membershipScore2 = 0.5;
  membershipScoreVector.push_back( membershipScore2 );

  double membershipScore3;
  membershipScore3 = 1.9;
  membershipScoreVector.push_back( membershipScore3 );

  // the maximum score is the third component. The decision rule should
  // return index ( 2)
  if( decisionRule->Evaluate( membershipScoreVector ) != 2 )
    {
    std::cerr << "Decision rule computation is incorrect!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
