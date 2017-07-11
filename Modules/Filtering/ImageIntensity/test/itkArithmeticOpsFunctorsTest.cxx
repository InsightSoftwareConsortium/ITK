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

#include "itkArithmeticOpsFunctors.h"

TEST(ArithmeticOpsTest,DivFloor_float)
{
typedef itk::Functor::DivFloor<float,float,float> OpType;

OpType op1;
OpType op2;

EXPECT_EQ(op1, op1);
EXPECT_EQ(op1, op2);

op1=op2;

EXPECT_EQ(2.0, op1(5.0f,2.0f));
EXPECT_EQ(-3.0, op1(-5.0f,2.0f));
EXPECT_NO_THROW(op1(5.0f,0.0f));
}

TEST(ArithmeticOpsTest,DivFloor_short)
{
typedef itk::Functor::DivFloor<short,short,short> OpType;

OpType op1;
OpType op2;

EXPECT_EQ(op1, op1);
EXPECT_EQ(op1, op2);

op1=op2;

EXPECT_EQ(2, op1(5,2));
EXPECT_EQ(-3, op1(-5,2));
EXPECT_NO_THROW(op1(5,0));
}

TEST(ArithmeticOpsTest,DivReal) {

typedef itk::Functor::DivReal<float,float,float> OpType;

OpType op1;
OpType op2;

EXPECT_EQ(op1, op1);
EXPECT_EQ(op1, op2);

op1=op2;

EXPECT_EQ(2.5f, op1(5.0f,2.0f));
EXPECT_EQ(-2.5f, op1(-5.0f,2.0f));
EXPECT_NO_THROW(op1(5.0f,0.0f));

}


TEST(ArithmeticOpsTest, UnaryMinus) {

  typedef itk::Functor::UnaryMinus<short, short> OpType;

  OpType op1;
  OpType op2;

  EXPECT_EQ(op1, op1);
  EXPECT_EQ(op1, op2);

  op1=op2;

  EXPECT_EQ(-1, op1(1));
  EXPECT_EQ(2, op1(-2));

}
