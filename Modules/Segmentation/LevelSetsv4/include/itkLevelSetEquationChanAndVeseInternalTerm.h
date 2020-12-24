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

#ifndef itkLevelSetEquationChanAndVeseInternalTerm_h
#define itkLevelSetEquationChanAndVeseInternalTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
/**
 *  \class LevelSetEquationChanAndVeseInternalTerm
 *  \brief Class to represent the internal energy Chan And Vese term
 *
 *  \f[
 *    \delta_{\epsilon}\left( \phi_{k} \left( p \right) \right) \cdot
      \left\| I\left( p \right) - \mu_{in} \right\|^2
 *  \cdot
 *  \f]
 *
 *  \li \f$ \delta_{epsilon}  \f$ is a regularized dirac function,
 *  \li \f$ k \f$ is the current level-set id,
 *  \li \f$ I\left( p \right) \f$ is the pixel value at the given location \f$ p \f$,
 *  \li \f$ \mu_{in}  \f$ is the internal mean intensity.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEquationChanAndVeseInternalTerm
  : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEquationChanAndVeseInternalTerm);

  using Self = LevelSetEquationChanAndVeseInternalTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationChanAndVeseInternalTerm, LevelSetEquationTermBase);

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

  itkSetMacro(Mean, InputPixelRealType);
  itkGetMacro(Mean, InputPixelRealType);

  /** Update the term parameter values at end of iteration */
  void
  Update() override;

  /** Initialize parameters in the terms prior to an iteration */
  void
  InitializeParameters() override;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  void
  Initialize(const LevelSetInputIndexType & inputIndex) override;

  /** Compute the product of Heaviside functions in the multi-levelset cases */
  virtual void
  ComputeProduct(const LevelSetInputIndexType & inputIndex, LevelSetOutputRealType & prod);

  /** Compute the product of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  virtual void
  ComputeProductTerm(const LevelSetInputIndexType &, LevelSetOutputRealType &)
  {}

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & inputIndex,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;

protected:
  LevelSetEquationChanAndVeseInternalTerm();

  ~LevelSetEquationChanAndVeseInternalTerm() override = default;

  /** Returns the term contribution for a given location inputPixel, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & inputIndex) override;

  /** Returns the term contribution for a given location inputPixel, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & inputIndex, const LevelSetDataType & data) override;

  /** Accumulate contribution to term parameters from a given pixel */
  void
  Accumulate(const InputPixelType & inputPixel, const LevelSetOutputRealType & heavisideValue);

  InputPixelRealType     m_Mean;
  InputPixelRealType     m_TotalValue;
  LevelSetOutputRealType m_TotalH;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationChanAndVeseInternalTerm.hxx"
#endif

#endif
