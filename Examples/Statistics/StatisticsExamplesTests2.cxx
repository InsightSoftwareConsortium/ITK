// this file defines the Statistics examples tested for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST( MinimumDecisionRuleTest );
REGISTER_TEST( MaximumDecisionRuleTest );
REGISTER_TEST( MaximumRatioDecisionRuleTest );
}

#undef main
#define main MinimumDecisionRuleTest
#include "MinimumDecisionRule.cxx"

#undef main
#define main MaximumDecisionRuleTest
#include "MaximumDecisionRule.cxx"

#undef main
#define main MaximumRatioDecisionRuleTest
#include "MaximumRatioDecisionRule.cxx"

