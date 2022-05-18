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
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEquationBinaryMaskTerm);

  using Self = LevelSetEquationBinaryMaskTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationBinaryMaskTerm, LevelSetEquationTermBase);

  using typename Superclass::InputImageType;
  using typename Superclass::InputImagePointer;
  using typename Superclass::InputPixelType;
  using typename Superclass::InputPixelRealType;

  using typename Superclass::LevelSetContainerType;
  using typename Superclass::LevelSetContainerPointer;
  using typename Superclass::LevelSetType;
  using typename Superclass::LevelSetPointer;
  using typename Superclass::LevelSetOutputPixelType;
  using typename Superclass::LevelSetOutputRealType;
  using typename Superclass::LevelSetInputIndexType;
  using typename Superclass::LevelSetGradientType;
  using typename Superclass::LevelSetHessianType;
  using typename Superclass::LevelSetIdentifierType;

  using typename Superclass::HeavisideType;
  using typename Superclass::HeavisideConstPointer;

  using typename Superclass::LevelSetDataType;

  using typename Superclass::DomainMapImageFilterType;
  using typename Superclass::CacheImageType;

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
  Value(const LevelSetInputIndexType & index) override;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & index, const LevelSetDataType & iData) override;


  InputImagePointer m_Mask;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationBinaryMaskTerm.hxx"
#endif

#endif
