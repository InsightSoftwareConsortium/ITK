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

#ifndef itkLevelSetEquationPropagationTerm_h
#define itkLevelSetEquationPropagationTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/**
 *  \class LevelSetEquationPropagationTerm
 *  \brief Derived class to represents a propagation term in the level-set evolution PDE
 *
 *  \f[
 *  PropagationImage( p ) \cdot \left\| \nabla \phi\left( p \right) \right\|
 *  \f]
 *
 *  \li PropagationImage is the propagation image set by the user.
 *  \li \f$ \| \cdot  \| \f$ denotes the usual \f$L_2\f$-norm
 *  \li \f$ \nabla \phi \f$ denotes the gradient of the level-set function \f$ \phi \f$.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer,
          typename TPropagationImage = TInput>
class ITK_TEMPLATE_EXPORT LevelSetEquationPropagationTerm : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEquationPropagationTerm);

  using Self = LevelSetEquationPropagationTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationPropagationTerm, LevelSetEquationTermBase);

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

  using PropagationImageType = TPropagationImage;
  using PropagationImagePointer = typename PropagationImageType::Pointer;

  /** Neighborhood radius type */
  using DefaultBoundaryConditionType = ZeroFluxNeumannBoundaryCondition<InputImageType>;
  using RadiusType = typename ConstNeighborhoodIterator<InputImageType>::RadiusType;
  using NeighborhoodType = ConstNeighborhoodIterator<InputImageType, DefaultBoundaryConditionType>;

  using NeighborhoodScalesType = Vector<LevelSetOutputRealType, Self::ImageDimension>;

  /** Set/Get the propagation image. By default, if no PropagationImage has
  been set, it casts the input image and uses it in the term contribution
  calculation. */
  itkSetObjectMacro(PropagationImage, PropagationImageType);
  itkGetModifiableObjectMacro(PropagationImage, PropagationImageType);

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
  LevelSetEquationPropagationTerm();

  ~LevelSetEquationPropagationTerm() override = default;

  PropagationImagePointer m_PropagationImage;

  /** Return the spatial speed dependence a given pixel location
   * Usually, it is constant across the image domain */
  LevelSetOutputRealType
  PropagationSpeed(const LevelSetInputIndexType & iP) const;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP) override;
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP, const LevelSetDataType & iData) override;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationPropagationTerm.hxx"
#endif

#endif
