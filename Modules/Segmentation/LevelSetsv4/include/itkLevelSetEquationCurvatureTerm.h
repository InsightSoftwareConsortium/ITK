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

#ifndef itkLevelSetEquationCurvatureTerm_h
#define itkLevelSetEquationCurvatureTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/**
 *  \class LevelSetEquationCurvatureTerm
 *  \brief Derived class to represents a curvature term in the level-set evolution PDE
 *
 *  \f[
 *  CurvatureImage( p ) \cdot \kappa( p )
 *  \f]
 *
 *  \li CurvatureImage denotes the curvature image set by the user
 *  \li \f$ \kappa( p ) \f$ denotes the mean curvature of the level set function,
 *  i.e. \f$ \kappa( p ) = \text{div} \left( \frac{ \nabla \phi( p ) }{ \left\| \nabla \phi(p) \right\| } \right) \f$
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, // Input image or mesh
          typename TLevelSetContainer,
          typename TCurvatureImage = TInput>
class ITK_TEMPLATE_EXPORT LevelSetEquationCurvatureTerm : public LevelSetEquationTermBase<TInput, TLevelSetContainer>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEquationCurvatureTerm);

  using Self = LevelSetEquationCurvatureTerm;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetEquationTermBase<TInput, TLevelSetContainer>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetEquationCurvatureTerm, LevelSetEquationTermBase);

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

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using CurvatureImageType = TCurvatureImage;
  using CurvatureImagePointer = typename CurvatureImageType::Pointer;

  /** Set/Get the propagation image. By default, if no PropagationImage has
  been set, it casts the input image and uses it in the term contribution
  calculation. */
  void
  SetCurvatureImage(CurvatureImageType * iImage);
  itkGetModifiableObjectMacro(CurvatureImage, CurvatureImageType);

  itkSetMacro(UseCurvatureImage, bool);
  itkGetMacro(UseCurvatureImage, bool);
  itkBooleanMacro(UseCurvatureImage);

  /** Neighborhood radius type */
  using DefaultBoundaryConditionType = ZeroFluxNeumannBoundaryCondition<InputImageType>;
  using RadiusType = typename ConstNeighborhoodIterator<InputImageType>::RadiusType;
  using NeighborhoodType = ConstNeighborhoodIterator<InputImageType, DefaultBoundaryConditionType>;

  using NeighborhoodScalesType = Vector<LevelSetOutputRealType, Self::ImageDimension>;

  /** Update the term parameter values at end of iteration */
  void
  Update() override;

  /** Initialize the parameters in the terms prior to an iteration */
  void
  InitializeParameters() override;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  void
  Initialize(const LevelSetInputIndexType &) override;

  /** Supply updates at pixels to keep the term parameters always updated */
  void
  UpdatePixel(const LevelSetInputIndexType & iP,
              const LevelSetOutputRealType & oldValue,
              const LevelSetOutputRealType & newValue) override;

protected:
  LevelSetEquationCurvatureTerm();

  ~LevelSetEquationCurvatureTerm() override = default;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP) override;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  LevelSetOutputRealType
  Value(const LevelSetInputIndexType & iP, const LevelSetDataType & iData) override;

  LevelSetOutputRealType m_NeighborhoodScales[ImageDimension];

  CurvatureImagePointer m_CurvatureImage;

  bool m_UseCurvatureImage;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEquationCurvatureTerm.hxx"
#endif

#endif
