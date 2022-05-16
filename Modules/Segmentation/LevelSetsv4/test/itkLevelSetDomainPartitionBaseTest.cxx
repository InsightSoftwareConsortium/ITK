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

#include "itkLevelSetDomainPartitionBase.h"
#include "itkImage.h"

namespace itk
{

template <typename TDomain>
class LevelSetDomainPartitionBaseHelper : public LevelSetDomainPartitionBase<TDomain>
{
public:
  /** Standard class type aliases. */
  using Self = LevelSetDomainPartitionBaseHelper;
  using Superclass = LevelSetDomainPartitionBase<TDomain>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(LevelSetDomainPartitionBaseHelper, LevelSetDomainPartitionBase);

  itkNewMacro(Self);

protected:
  void
  AllocateListDomain() override
  {}
  void
  PopulateListDomain() override
  {}
};

} // namespace itk


int
itkLevelSetDomainPartitionBaseTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using ImageType = itk::Image<double, Dimension>;

  using DomainPartitionBaseHelperType = itk::LevelSetDomainPartitionBaseHelper<ImageType>;

  itk::IdentifierType count = 2;

  auto function = DomainPartitionBaseHelperType::New();
  function->SetNumberOfLevelSetFunctions(count);

  if (function->GetNumberOfLevelSetFunctions() != count)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
