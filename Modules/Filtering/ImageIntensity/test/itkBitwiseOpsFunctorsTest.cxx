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

#include "itkBitwiseOpsFunctors.h"

TEST(BitwiseOpsTest,DivFloor)
{
  typedef itk::Functor::BitwiseNot<unsigned char,unsigned char> OpType;

  OpType op1;
  OpType op2;

  EXPECT_EQ(op1, op1);
  EXPECT_EQ(op1, op2);

  op1=op2;


  EXPECT_EQ(0xFE, op1(0x01));
  EXPECT_EQ(0x02, op1(0xFD));
  EXPECT_EQ(0xF0, op1(0x0F));
}
