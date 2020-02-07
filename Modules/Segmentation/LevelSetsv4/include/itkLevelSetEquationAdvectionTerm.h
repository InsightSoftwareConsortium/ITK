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

#ifndef itkLevelSetEquationAdvectionTerm_h
#define itkLevelSetEquationAdvectionTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/**
 *  \class LevelSetEquationAdvectionTerm
 *  \brief Derived class to represents an advection term in the level-set evolution PDE
 *
 *  \f[
 *  AdvectionImage\left( p \right) \bullet \nabla \phi\left( p \right)
 *  \f]
 *
 *  \li \f$ AdvectionImage \left( p \right) \f$ is the advection image provided by the user.
 *  \li \f$ \cdot \bullet \cdot \f$ denotes the usual dot product
 *  \li \f$ \nabla \phi \f$ denotes the gradient of the level set function \f$ \phi \f$.
 *
 * The advection image can be directly provided by the user; or by
 * default, it is computed as the gradient of the input image. In this last
 * case, it can be smoothed by the means of DerivativeSigma.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEquationAdvectionTerm : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationAdvectionTerm);

  using Self = LevelSetEquationAdvectionTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationAdvectionTerm, LevelSetEquationTermBase);

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
  using LevelSetDataType = typename Superclass::LevelSetDataType;

  using HeavisideType = typename Superclass::HeavisideType;
  using HeavisideConstPointer = typename Superclass::HeavisideConstPointer;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using VectorType = LevelSetGradientType;

  using AdvectionImageType = Image<VectorType, Self::ImageDimension>;
  using AdvectionImagePointer = typename AdvectionImageType::Pointer;


  void
  SetAdvectionImage(AdvectionImageType * iImage);
  itkGetModifiableObjectMacro(AdvectionImage, AdvectionImageType);

  itkSetMacro(DerivativeSigma, LevelSetOutputRealType);
  itkGetMacro(DerivativeSigma, LevelSetOutputRealType);

  /** Neighborhood radius type */
  using DefaultBoundaryConditionType = ZeroFluxNeumannBoundaryCondition<InputImageType>;
  using RadiusType = typename ConstNeighborhoodIterator<InputImageType>::RadiusType;
  using NeighborhoodType = ConstNeighborhoodIterator<InputImageType, DefaultBoundaryConditionType>;

  using NeighborhoodScalesType = Vector<LevelSetOutputRealType, Self::ImageDimension>;

  /** \todo to be documented. */
  void
  Update() override;

  /** Initialize the parameters in the terms prior to an iteration */
  void
  InitializeParameters() override;

  /** \todo to be documented. */
  void
  Initialize(const LevelSetInputIndexType &) override;

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & iP,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;

protected:
  LevelSetEquationAdvectionTerm();

  ~LevelSetEquationAdvectionTerm() override = default;

  AdvectionImagePointer m_AdvectionImage;

  /** Return the spatial speed dependence a given pixel location
   * Usually, it is constant across the image domain */
  VectorType
  AdvectionSpeed(const LevelSetInputIndexType & iP) const;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP) override;
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP, const LevelSetDataType & iData) override;

  LevelSetOutputRealType m_NeighborhoodScales[ImageDimension];

private:
  LevelSetOutputRealType m_DerivativeSigma;

  bool m_AutoGenerateAdvectionImage;

  void
  GenerateAdvectionImage();
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationAdvectionTerm.hxx"
#endif

#endif
