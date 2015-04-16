/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFFTCommon.h"
#include "itkTestingMacros.h"

int itkFFTCommonTest(int, char *[])
{
  TEST_EXPECT_TRUE(itk::IsPrime(0) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(1) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(2));
  TEST_EXPECT_TRUE(itk::IsPrime(3));
  TEST_EXPECT_TRUE(itk::IsPrime(4) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(5));
  TEST_EXPECT_TRUE(itk::IsPrime(6) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(7));
  TEST_EXPECT_TRUE(itk::IsPrime(8) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(9) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(10) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(11));
  TEST_EXPECT_TRUE(itk::IsPrime(12) == false);
  TEST_EXPECT_TRUE(itk::IsPrime(13));

  TEST_EXPECT_EQUAL(itk::GreatestPrimeFactor(12), 3);
  TEST_EXPECT_EQUAL(itk::GreatestPrimeFactor(75), 5);
  TEST_EXPECT_EQUAL(itk::GreatestPrimeFactor(1024), 2);

  return EXIT_SUCCESS;
}
