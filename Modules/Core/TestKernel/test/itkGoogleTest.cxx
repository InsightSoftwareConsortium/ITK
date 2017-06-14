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

#include <gtest/gtest.h>
#include <itkMath.h>

// This file contains a couple minimal Google Tests to verify that
// using the Google Test framework in ITK works.

TEST(GoogleTest,t1) {
  void *ptr = NULL;
  ASSERT_TRUE((ptr == ITK_NULLPTR));
  EXPECT_TRUE((false == ITK_NULLPTR));
  // ASSERT_EQ(0, ITK_NULLPTR); fails with C++11
}
