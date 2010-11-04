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
#include "itkAmoebaOptimizer.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkLBFGSOptimizer.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkOptimizer.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkVersorTransformOptimizer.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKOptimizers);
  namespace wrappers
  {
    ITK_WRAP_OBJECT(AmoebaOptimizer);
    ITK_WRAP_OBJECT(ConjugateGradientOptimizer);
    ITK_WRAP_OBJECT(GradientDescentOptimizer);
    ITK_WRAP_OBJECT(LBFGSOptimizer);
    ITK_WRAP_OBJECT(LevenbergMarquardtOptimizer);
    ITK_WRAP_OBJECT(MultipleValuedNonLinearOptimizer);
    ITK_WRAP_OBJECT(MultipleValuedNonLinearVnlOptimizer);
    ITK_WRAP_OBJECT(NonLinearOptimizer);
    ITK_WRAP_OBJECT(OnePlusOneEvolutionaryOptimizer);
    ITK_WRAP_OBJECT(Optimizer);
    ITK_WRAP_OBJECT(QuaternionRigidTransformGradientDescentOptimizer);
    ITK_WRAP_OBJECT(RegularStepGradientDescentBaseOptimizer);
    ITK_WRAP_OBJECT(RegularStepGradientDescentOptimizer);
    ITK_WRAP_OBJECT(SingleValuedNonLinearOptimizer);
    ITK_WRAP_OBJECT(SingleValuedNonLinearVnlOptimizer);
    ITK_WRAP_OBJECT(VersorTransformOptimizer);
  }
}

#endif
