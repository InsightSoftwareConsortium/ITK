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

#include "itkFastMarchingBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkTestingMacros.h"

namespace itk
{
template <typename TInput, typename TOutput>
class FastMarchingBaseTestHelper : public FastMarchingBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingBaseTestHelper);

  using Self = FastMarchingBaseTestHelper;
  using Superclass = FastMarchingBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingBaseTestHelper, FastMarchingBase);

  using typename Superclass::Traits;
  using typename Superclass::OutputDomainType;

  //  using typename Superclass::NodeContainerType;
  using typename Superclass::NodeType;

  using typename Superclass::OutputPixelType;
  using typename Superclass::LabelType;

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
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " useMeshVsImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using PixelType = float;

  auto useMeshVsImage = std::stoul(argv[1]);

  if (useMeshVsImage == 0)
  {
    using ImageType = itk::Image<PixelType, Dimension>;

    auto input = ImageType::New();

    using ImageFastMarching = itk::FastMarchingBaseTestHelper<ImageType, ImageType>;
    auto fmm = ImageFastMarching::New();

    // Check default values
    auto topologyCheck = ImageFastMarching::TopologyCheckEnum::Nothing;
    ITK_TEST_SET_GET_VALUE(topologyCheck, fmm->GetTopologyCheck());

    ITK_TEST_SET_GET_NULL_VALUE(fmm->GetTrialPoints());

    ITK_TEST_SET_GET_NULL_VALUE(fmm->GetAlivePoints());

    ITK_TEST_SET_GET_NULL_VALUE(fmm->GetProcessedPoints());

    ITK_TEST_SET_GET_NULL_VALUE(fmm->GetForbiddenPoints());

    ITK_TEST_SET_GET_NULL_VALUE(fmm->GetStoppingCriterion());

    double speedConstant = 1.0;
    ITK_TEST_SET_GET_VALUE(speedConstant, fmm->GetSpeedConstant());

    double normalizationFactor = 1.0;
    ITK_TEST_SET_GET_VALUE(normalizationFactor, fmm->GetNormalizationFactor());

    auto targetReachedValue = itk::NumericTraits<typename ImageFastMarching::OutputPixelType>::ZeroValue();
    ITK_TEST_EXPECT_EQUAL(targetReachedValue, fmm->GetTargetReachedValue());

    bool collectPoints = false;
    ITK_TEST_EXPECT_EQUAL(collectPoints, fmm->GetCollectPoints());

    // Check other values
    topologyCheck = ImageFastMarching::TopologyCheckEnum::Strict;
    fmm->SetTopologyCheck(topologyCheck);
    ITK_TEST_SET_GET_VALUE(topologyCheck, fmm->GetTopologyCheck());

    auto                                     processedPoints = ImageFastMarching::NodePairContainerType::New();
    typename ImageFastMarching::NodePairType node_pair;
    ImageType::OffsetType                    offset = { { 28, 35 } };

    itk::Index<Dimension> index;
    index.Fill(0);

    node_pair.SetValue(0.0);
    node_pair.SetNode(index + offset);
    processedPoints->push_back(node_pair);

    fmm->SetProcessedPoints(processedPoints);
    ITK_TEST_SET_GET_VALUE(processedPoints, fmm->GetProcessedPoints());

    auto stoppingCriterion = itk::FastMarchingThresholdStoppingCriterion<ImageType, ImageType>::New();
    fmm->SetStoppingCriterion(stoppingCriterion);
    ITK_TEST_SET_GET_VALUE(stoppingCriterion, fmm->GetStoppingCriterion());

    speedConstant = 2.0;
    fmm->SetSpeedConstant(speedConstant);
    ITK_TEST_SET_GET_VALUE(speedConstant, fmm->GetSpeedConstant());

    normalizationFactor = 2.0;
    fmm->SetNormalizationFactor(normalizationFactor);
    ITK_TEST_SET_GET_VALUE(normalizationFactor, fmm->GetNormalizationFactor());

    collectPoints = true;
    ITK_TEST_SET_GET_BOOLEAN(fmm, CollectPoints, collectPoints);

    fmm->SetInput(input);

    ITK_TRY_EXPECT_EXCEPTION(fmm->Update());


    using OutputImageType = ImageFastMarching::OutputDomainType;
    OutputImageType::Pointer output = fmm->GetOutput();

    (void)output;
  }
  else if (useMeshVsImage == 1)
  {
    using MeshType = itk::QuadEdgeMesh<PixelType, Dimension, itk::QuadEdgeMeshTraits<PixelType, Dimension, bool, bool>>;

    auto input = MeshType::New();

    using MeshFastMarching = itk::FastMarchingBaseTestHelper<MeshType, MeshType>;
    auto fmm = MeshFastMarching::New();
    fmm->SetInput(input);

    ITK_TRY_EXPECT_EXCEPTION(fmm->Update());


    using OutputMeshType = MeshFastMarching::OutputDomainType;
    OutputMeshType::Pointer output = fmm->GetOutput();

    (void)output;
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


  return EXIT_SUCCESS;
}
