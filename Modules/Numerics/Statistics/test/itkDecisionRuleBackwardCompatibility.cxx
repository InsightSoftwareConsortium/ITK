/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// include deprecated headers

#include "itkDecisionRuleBase.h"          // old statistics library
#include "itkMaximumDecisionRule2.h"      // 2007 refactored statistics library
#include "itkMinimumDecisionRule2.h"      // 2007 refactored statistics library
#include "itkMaximumRatioDecisionRule2.h" // 2007 refactored statistics library

int
itkDecisionRuleBackwardCompatibilityTest(int, char *[])
{
  // Define a type from the old statistics library
  typdef itk::DecisionRuleBase DecisionRuleType;

  // MinimumDecisionRule used to just be in the itk namespace. Now it
  // is in the itk::Statistics namespace. Test backward compatibility
  // of the symbol in the old namespace.
  using MinimumDecisionRuleType = itk::MinimumDecisionRul;
  using MaximumDecisionRuleType = itk::MaximumDecisionRul;
  using MaximumRationDecisionRuleType = itk::MaximumRatioDecisionRul;

  // MinimumDecisionRule2 has been renamed to MinimumDecisionRule.Test
  // backward compatibility of the old symbol name.
  using MinimumDecisionRuleType2 = itk::Statistics::MinimumDecisionRule2;
  using MaximumDecisionRuleType2 = itk::Statistics::MaximumDecisionRule2;
  using MaximumRatioDecisionRuleType2 = itk::Statistics::MaximumRatioDecisionRule2;

  return EXIT_SUCCESS;
}
