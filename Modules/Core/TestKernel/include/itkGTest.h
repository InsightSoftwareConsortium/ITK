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

#ifndef itkGTest_h
#define itkGTest_h

#include "gtest/gtest.h"
#include "itkGTestPredicate.h"
#include "itkGTestTypedefsAndConstructors.h"


/** \namespace itk::GTest
 * \brief The GTest namespace contains GTest extensions for ITK, and
 * convenience type alias, and functions to aid in analytic testing of
 * results and values.
 *
 */


/** A lightweight alternative for `ITK_EXERCISE_BASIC_OBJECT_METHODS`, using GoogleTest macro's. */
#define ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(object, ClassName, SuperclassName) \
  [object] {                                                                       \
    std::ostringstream outputStreamForPrintExercise;                               \
    object->Print(outputStreamForPrintExercise);                                   \
    EXPECT_FALSE(outputStreamForPrintExercise.str().empty());                      \
    EXPECT_STREQ(object->Self::GetNameOfClass(), #ClassName);                      \
    EXPECT_STREQ(object->Superclass::GetNameOfClass(), #SuperclassName);           \
  }()


// end namespace itk

#endif // itkGTest_h
