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
#ifndef itkVariationalRegistrationNCCFunction_h
#define itkVariationalRegistrationNCCFunction_h

#include "itkVariationalRegistrationFunction.h"
#include "itkCovariantVector.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

/** \class VariationalRegistrationNCCFunction
 *
 *  \brief This class computes NCC forces in the variational registration framework.
 *
 *  This class implements NCC forces as given in <em> Hermosillo, Chefd'Hotel, and Faugeras.
 *  "Variational methods for multimodal image matching." IJCV 50(3), 2002: 329-343</em>
 *  and <em>Avants et al. "Symmetric diffeomorphic image registration with cross-correlation:
 *  evaluating automated labeling of elderly and neurodegenerative brain." Medical image analysis
 *  12(1), 2008: 26-41</em> (except Jacobian term). We define the derivative of NCC between two
 *  images as:
 *  \f[
 *    f^{NCC}(x)=\tau\kappa\frac{2\sum_w (F-\bar{F})(M-\bar{M})}{\sum_w (F-\bar{F})^2\
 *    \sum_w (M-\bar{M})^2}\left((M-\bar{M}) -
 *    \frac{\sum_w (F-\bar{F})(M-\bar{M})}{\sum_w (F-\bar{F})^2}(F-\bar{F})\right)
 *    \nabla M(x+u(x))
 *  \f]
 *  \f$\tau\f$ is the step size and \f$\kappa\f$ is the mean squared spacing. Use SetRadius()
 *  (see FiniteDifferenceFunction) to set the size of the neighbourhood to compute local mean values
 *  \f$\bar{F}\f$ and \f$\bar{M}\f$ and the local sums.
 *  Alternative, the classical gradient \f$\nabla M(x+u(x))\f$ can be replaced by \f$\nabla F(x)\f$
 *  or \f$\frac{\nabla F(x) + \nabla M(x+u(x))}{2}\f$.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationFunction
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalRegistrationNCCFunction
  : public VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationNCCFunction);

  /** Standard class type alias. */
  using Self = VariationalRegistrationNCCFunction;
  using Superclass = VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationNCCFunction, VariationalRegistrationFunction);

  /** Get image dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;

  /** MaskImage image type. */
  using MaskImageType = typename Superclass::MaskImageType;
  using MaskImagePointer = typename Superclass::MaskImagePointer;

  using IndexType = typename FixedImageType::IndexType;
  using SizeType = typename FixedImageType::SizeType;
  using SpacingType = typename FixedImageType::SpacingType;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldTypePointer = typename Superclass::DisplacementFieldTypePointer;

  /** Inherit some types from the superclass. */
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  /** Gradient calculator type. */
  using GradientCalculatorType = CentralDifferenceImageFunction<FixedImageType>;
  using GradientCalculatorPointer = typename GradientCalculatorType::Pointer;

  /** Set the object's state before each iteration. */
  void
  InitializeIteration() override;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  /** Select that the fixed image gradient is used for computing the forces. */
  virtual void
  SetGradientTypeToFixedImage()
  {
    m_GradientType = GRADIENT_TYPE_FIXED;
  }

  /** Select that the warped image gradient is used for computing the forces. */
  virtual void
  SetGradientTypeToWarpedMovingImage()
  {
    m_GradientType = GRADIENT_TYPE_WARPED;
  }

  /** Select that fixed and warped image gradients are used for computing the
   *  forces. */
  virtual void
  SetGradientTypeToSymmetric()
  {
    m_GradientType = GRADIENT_TYPE_SYMMETRIC;
  }

  /** Computes the time step for an update.
   * Returns the constant time step scaled with the mean squared spacing.
   * \sa SetTimeStep() */
  typename Superclass::TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const override
  {
    return this->GetTimeStep() * m_Normalizer;
  }

protected:
  VariationalRegistrationNCCFunction();
  ~VariationalRegistrationNCCFunction() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** FixedImage image neighborhood iterator type. */
  using FixedImageNeighborhoodIteratorType = ConstNeighborhoodIterator<FixedImageType>;

  using GlobalDataStruct = typename Superclass::GlobalDataStruct;

  /** Type of available image forces */
  enum GradientType
  {
    GRADIENT_TYPE_WARPED = 0,
    GRADIENT_TYPE_FIXED = 1,
    GRADIENT_TYPE_SYMMETRIC = 2
  };

  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the warped image. */
  GradientCalculatorPointer m_WarpedImageGradientCalculator;

  /** Set if warped or fixed image gradient is used for force computation. */
  GradientType m_GradientType;

  /** Precalculated normalizer for spacing consideration. */
  double m_Normalizer;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationNCCFunction.hxx"
#endif

#endif
