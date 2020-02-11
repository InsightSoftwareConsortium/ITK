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

#include "itkFastMarchingStoppingCriterionBase.h"

namespace itk
{
template <typename TInput, typename TOutput>
class FastMarchingStoppingCriterionBaseHelperTest : public FastMarchingStoppingCriterionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingStoppingCriterionBaseHelperTest);

  using Self = FastMarchingStoppingCriterionBaseHelperTest;
  using Superclass = FastMarchingStoppingCriterionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using NodeType = typename Superclass::NodeType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingStoppingCriterionBaseHelperTest, FastMarchingStoppingCriterionBase);

  bool
  IsSatisfied() const override
  {
    return true;
  }
  std::string
  GetDescription() const override
  {
    return "Description";
  }

protected:
  FastMarchingStoppingCriterionBaseHelperTest()
    : Superclass()
  {}
  ~FastMarchingStoppingCriterionBaseHelperTest() override = default;

  void
  SetCurrentNode(const NodeType &) override
  {}

  void
  Reset() override
  {}
};
} // namespace itk

int
itkFastMarchingStoppingCriterionBaseTest(int, char *[])
{
  using ImageType = itk::Image<float, 2>;

  using ImageStoppingCriterionType = itk::FastMarchingStoppingCriterionBaseHelperTest<ImageType, ImageType>;

  ImageStoppingCriterionType::Pointer image_criterion = ImageStoppingCriterionType::New();
  if (image_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using MeshStoppingCriterionType = itk::FastMarchingStoppingCriterionBaseHelperTest<MeshType, MeshType>;

  MeshStoppingCriterionType::Pointer mesh_criterion = MeshStoppingCriterionType::New();
  if (mesh_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
