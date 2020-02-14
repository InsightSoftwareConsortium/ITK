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

#include "itkFastMarchingBase.h"

namespace itk
{
template <typename TInput, typename TOutput>
class FastMarchingBaseTestHelper : public FastMarchingBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingBaseTestHelper);

  using Self = FastMarchingBaseTestHelper;
  using Superclass = FastMarchingBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingBaseTestHelper, FastMarchingBase);

  using Traits = typename Superclass::Traits;
  using OutputDomainType = typename Superclass::OutputDomainType;

  //  using NodeContainerType = typename Superclass::NodeContainerType;
  using NodeType = typename Superclass::NodeType;

  using OutputPixelType = typename Superclass::OutputPixelType;
  using LabelType = typename Superclass::LabelType;

protected:
  FastMarchingBaseTestHelper() = default;
  ~FastMarchingBaseTestHelper() override = default;

  IdentifierType
  GetTotalNumberOfNodes() const override
  {
    return 1;
  }

  void
  SetOutputValue(OutputDomainType *, const NodeType &, const OutputPixelType &) override
  {}

  const OutputPixelType
  GetOutputValue(OutputDomainType *, const NodeType &) const override
  {
    return NumericTraits<OutputPixelType>::ZeroValue();
  }

  unsigned char
  GetLabelValueForGivenNode(const NodeType &) const override
  {
    return Traits::Far;
  }

  void
  SetLabelValueForGivenNode(const NodeType &, const LabelType &) override
  {}

  void
  UpdateNeighbors(OutputDomainType *, const NodeType &) override
  {}

  void
  UpdateValue(OutputDomainType *, const NodeType &) override
  {}

  bool
  CheckTopology(OutputDomainType *, const NodeType &) override
  {
    return true;
  }

  void
  InitializeOutput(OutputDomainType *) override
  {}
};
} // namespace itk

// -----------------------------------------------------------------------------

int
itkFastMarchingBaseTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    return EXIT_FAILURE;
  }

  using PixelType = float;

  bool exception_caught = false;

  if (std::stoi(argv[1]) == 0)
  {
    constexpr unsigned Dimension = 3;
    using ImageType = itk::Image<PixelType, Dimension>;

    ImageType::Pointer input = ImageType::New();

    using ImageFastMarching = itk::FastMarchingBaseTestHelper<ImageType, ImageType>;
    ImageFastMarching::Pointer fmm = ImageFastMarching::New();
    fmm->SetInput(input);

    try
    {
      fmm->Update();
    }
    catch (const itk::ExceptionObject & excep)
    {
      std::cerr << "Exception caught !" << std::endl;
      std::cerr << excep << std::endl;
      exception_caught = true;
    }

    using OutputImageType = ImageFastMarching::OutputDomainType;
    OutputImageType::Pointer output = fmm->GetOutput();

    (void)output;
  }
  else
  {
    if (std::stoi(argv[1]) == 1)
    {
      using MeshType = itk::QuadEdgeMesh<PixelType, 3, itk::QuadEdgeMeshTraits<PixelType, 3, bool, bool>>;

      MeshType::Pointer input = MeshType::New();

      using MeshFastMarching = itk::FastMarchingBaseTestHelper<MeshType, MeshType>;
      MeshFastMarching::Pointer fmm = MeshFastMarching::New();
      fmm->SetInput(input);

      try
      {
        fmm->Update();
      }
      catch (const itk::ExceptionObject & excep)
      {
        std::cerr << "Exception caught !" << std::endl;
        std::cerr << excep << std::endl;
        exception_caught = true;
      }

      using OutputMeshType = MeshFastMarching::OutputDomainType;
      OutputMeshType::Pointer output = fmm->GetOutput();

      (void)output;
    }
  }

  // Test streaming enumeration for FastMarchingTraitsEnums::TopologyCheck elements
  const std::set<itk::FastMarchingTraitsEnums::TopologyCheck> allTopologyCheck{
    itk::FastMarchingTraitsEnums::TopologyCheck::Nothing,
    itk::FastMarchingTraitsEnums::TopologyCheck::NoHandles,
    itk::FastMarchingTraitsEnums::TopologyCheck::Strict
  };
  for (const auto & ee : allTopologyCheck)
  {
    std::cout << "STREAMED ENUM VALUE FastMarchingTraitsEnums::TopologyCheck: " << ee << std::endl;
  }

  if (exception_caught)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
