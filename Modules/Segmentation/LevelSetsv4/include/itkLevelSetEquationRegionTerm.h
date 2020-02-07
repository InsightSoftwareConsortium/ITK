/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLevelSetEquationRegionTerm_h
#define itkLevelSetEquationRegionTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
template <typename TInput, typename TLevelSetContainer>
class LevelSetEquationRegionTerm : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationRegionTerm);

  using Self = LevelSetEquationRegionTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  using InputType = TInput;
  using InputPointer = typename InputType::Pointer;

  using LevelSetContainerType = TLevelSetContainer;
  using LevelSetContainerPointer = typename LevelSetContainerType::Pointer;
  using LevelSetOutputType = typename LevelSetContainerType::OutputType;
  using LevelSetInputType = typename LevelSetContainerType::InputType;
  using GradientType = typename LevelSetContainerType::GradientType;
  using HessianType = typename LevelSetContainerType::HessianType;

protected:
  LevelSetEquationRegionTerm()
    : Superclass()
  {}

  virtual ~LevelSetEquationRegionTerm() = default;
};
} // namespace itk
#endif // itkLevelSetEquationRegionTerm_h
