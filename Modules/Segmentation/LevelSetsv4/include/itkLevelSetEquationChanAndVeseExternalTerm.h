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

#ifndef itkLevelSetEquationChanAndVeseExternalTerm_h
#define itkLevelSetEquationChanAndVeseExternalTerm_h

#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 *  \class LevelSetEquationChanAndVeseExternalTerm
 *  \brief Class to represent the external energy Chan And Vese term
 *
 *  \f[
 *    \delta_{\epsilon}\left( \phi_{k}\left( p \right) \right) \cdot
 *    \prod_{i = 1, i \neq k}^{N} \left( 1 - H_{\epsilon} \left( \phi_i\left( p \right) \right) \right) \cdot
      \left\| I(p) - \mu_{out} \right\|^2
 *  \f]
 *
 *  \li \f$ \delta_{\epsilon} \f$ is a regularized dirac function,
 *  \li \f$ k \f$ is the current level-set id,
 *  \li \f$  I\left( p \right) \f$ is the pixel value at the given location \f$ p \f$,
 *  \li \f$ H_{\epsilon}  \f$ is a regularized Heaviside function,
 *  \li \f$ \mu_{out} \f$ is the external mean intensity
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEquationChanAndVeseExternalTerm
  : public LevelSetEquationChanAndVeseInternalTerm<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationChanAndVeseExternalTerm);

  using Self = LevelSetEquationChanAndVeseExternalTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationChanAndVeseInternalTerm<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationChanAndVeseExternalTerm, LevelSetEquationChanAndVeseInternalTerm);

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

  using DomainMapImageFilterType = typename Superclass::DomainMapImageFilterType;
  using CacheImageType = typename Superclass::CacheImageType;

  using DomainIteratorType = typename DomainMapImageFilterType::DomainMapType::const_iterator;

  using IdListType = typename LevelSetContainerType::IdListType;
  using IdListIterator = typename LevelSetContainerType::IdListIterator;
  using IdListConstIterator = typename LevelSetContainerType::IdListConstIterator;

  using HeavisideType = typename Superclass::HeavisideType;
  using HeavisideConstPointer = typename Superclass::HeavisideConstPointer;

  /** Compute the product of Heaviside functions in the multi-levelset cases */
  void
  ComputeProduct(const LevelSetInputIndexType & iP, LevelSetOutputRealType & prod) override;

  /** Compute the product of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  void
  ComputeProductTerm(const LevelSetInputIndexType & iP, LevelSetOutputRealType & prod) override;

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & iP,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;

protected:
  LevelSetEquationChanAndVeseExternalTerm();
  ~LevelSetEquationChanAndVeseExternalTerm() override = default;

private:
  DomainMapImageFilterType * m_DomainMapImageFilter;
  CacheImageType *           m_CacheImage;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationChanAndVeseExternalTerm.hxx"
#endif

#endif
