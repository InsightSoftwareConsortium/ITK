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

#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk
{

template <typename TInput,   // LevelSetImageType
          typename TFeature, // FeatureImageType
          typename TSharedData>
class ScalarChanAndVeseLevelSetFunctionTest2Helper
  : public ScalarChanAndVeseLevelSetFunction<TInput, TFeature, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarChanAndVeseLevelSetFunctionTest2Helper);

  /** Standard class type aliases. */
  using Self = ScalarChanAndVeseLevelSetFunctionTest2Helper;
  using Superclass = ScalarChanAndVeseLevelSetFunction<TInput, TFeature, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarChanAndVeseLevelSetFunctionTest2Helper, ScalarChanAndVeseLevelSetFunction);

  using typename Superclass::ScalarValueType;
  using typename Superclass::FeaturePixelType;
  using typename Superclass::FeatureIndexType;


  virtual ScalarValueType
  computeInternalTerm(const FeaturePixelType &, const FeatureIndexType &, const unsigned int &)
  {
    return ScalarValueType(0);
  }

  virtual ScalarValueType
  computeExternalTerm(const FeaturePixelType &, const FeatureIndexType &, const unsigned int &)
  {
    return ScalarValueType(0);
  }

  virtual void
  computeOverlapParameters(const FeatureIndexType, unsigned int &, unsigned int &)
  {}

  void
  ComputeParameters() override
  {}

protected:
  ScalarChanAndVeseLevelSetFunctionTest2Helper() = default;
  ~ScalarChanAndVeseLevelSetFunctionTest2Helper() override = default;
};

} // namespace itk

int
itkScalarChanAndVeseLevelSetFunctionTest2(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;

  using DataHelperType = itk::ScalarChanAndVeseLevelSetFunctionData<ImageType, FeatureImageType>;

  using SharedDataHelperType =
    itk::ConstrainedRegionBasedLevelSetFunctionSharedData<ImageType, FeatureImageType, DataHelperType>;


  using ChanAndVeseLevelSetFunctionType =
    itk::ScalarChanAndVeseLevelSetFunctionTest2Helper<ImageType, FeatureImageType, SharedDataHelperType>;

  auto function = ChanAndVeseLevelSetFunctionType::New();

  std::cout << "GetNameOfClass() = " << function->GetNameOfClass() << std::endl;
  function->Print(std::cout);

  return EXIT_SUCCESS;
}
