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

#ifndef itkLevelSetEquationOverlapPenaltyTerm_h
#define itkLevelSetEquationOverlapPenaltyTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkCompensatedSummation.h"

namespace itk
{
/**
 *  \class LevelSetEquationOverlapPenaltyTerm
 *  \brief Class to represent the overlap penalty among many level-sets
 *
 *  \f[
 *    \sum_{i \neq j } \left(H_{\epsilon}\left(\phi_j \left( p \right) \right)\right)
 *  \f]
 *
 *  \li \f$ i  \f$ is the current level-set id,
 *  \li \f$ j  \f$ is any level-set id,
 *  \li \f$ p  \f$ is the given location,
 *  \li \f$ H_{\epsilon}  \f$ is regularized Heaviside function.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEquationOverlapPenaltyTerm
  : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEquationOverlapPenaltyTerm);

  using Self = LevelSetEquationOverlapPenaltyTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationOverlapPenaltyTerm, LevelSetEquationTermBase);

  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputPixelRealType = typename Superclass::InputPixelRealType;

  using LevelSetContainerType = typename Superclass::LevelSetContainerType;
  using LevelSetContainerPointer = typename Superclass::LevelSetContainerPointer;
  using LevelSetType = typename Superclass::LevelSetType;
  using LevelSetPointer = typename Superclass::LevelSetPointer;
  using LevelSetOutputPixelType = typename Superclass::LevelSetOutputPixelType;
  using LevelSetOutputRealType = typename Superclass::LevelSetOutputRealType;
  using LevelSetInputIndexType = typename Superclass::LevelSetInputIndexType;
  using LevelSetGradientType = typename Superclass::LevelSetGradientType;
  using LevelSetHessianType = typename Superclass::LevelSetHessianType;
  using LevelSetIdentifierType = typename Superclass::LevelSetIdentifierType;

  using HeavisideType = typename Superclass::HeavisideType;
  using HeavisideConstPointer = typename Superclass::HeavisideConstPointer;

  using LevelSetDataType = typename Superclass::LevelSetDataType;

  using DomainMapImageFilterType = typename Superclass::DomainMapImageFilterType;
  using CacheImageType = typename Superclass::CacheImageType;

  using IdListType = typename LevelSetContainerType::IdListType;
  using IdListIterator = typename LevelSetContainerType::IdListIterator;
  using IdListConstIterator = typename LevelSetContainerType::IdListConstIterator;

  using CompensatedSummationType = CompensatedSummation<LevelSetOutputRealType>;


  /** Update the term parameter values at end of iteration */
  void
  Update() override;

  /** Initialize parameters in the terms prior to an iteration */
  void
  InitializeParameters() override;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  void
  Initialize(const LevelSetInputIndexType & index) override;

  /** Compute the sum of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  virtual void
  ComputeSumTerm(const LevelSetInputIndexType & index, LevelSetOutputRealType & sum);

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & index,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;


protected:
  LevelSetEquationOverlapPenaltyTerm();

  ~LevelSetEquationOverlapPenaltyTerm() override = default;

  /** Returns the term contribution for a given location index */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & index) override;

  /** Returns the term contribution for a given location index */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & index, const LevelSetDataType & data) override;

private:
  DomainMapImageFilterType * m_DomainMapImageFilter;
  CacheImageType *           m_CacheImage;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationOverlapPenaltyTerm.hxx"
#endif

#endif
