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

#ifndef itkLevelSetEquationBinaryMaskTerm_h
#define itkLevelSetEquationBinaryMaskTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
/**
 *  \class LevelSetEquationBinaryMaskTerm
 *  \brief Class to represent the mask boundary as a negative energy
 *
 *  \f[
 *    M\left( p \right)
 *  \cdot
 *  \f]
 *
 *  \li \f$ M \f$ is a mask with 0 representing a boundary,
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEquationBinaryMaskTerm : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationBinaryMaskTerm);

  using Self = LevelSetEquationBinaryMaskTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationBinaryMaskTerm, LevelSetEquationTermBase);

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

  itkSetObjectMacro(Mask, InputImageType);

  /** Update the term parameter values at end of iteration */
  void
  Update() override;

  /** Initialize parameters in the terms prior to an iteration */
  void
  InitializeParameters() override;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  void
  Initialize(const LevelSetInputIndexType & iP) override;

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & iP,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;

protected:
  LevelSetEquationBinaryMaskTerm();

  ~LevelSetEquationBinaryMaskTerm() override = default;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP) override;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP, const LevelSetDataType & iData) override;


  InputImagePointer m_Mask;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationBinaryMaskTerm.hxx"
#endif

#endif
