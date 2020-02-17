/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkVariationalRegistrationDemonsFunction_h
#define itkVariationalRegistrationDemonsFunction_h

#include "itkVariationalRegistrationFunction.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

/** \class itk::VariationalRegistrationDemonsFunction
 *
 * \brief This class computes different Demon forces in the variational registration framework.
 *
 * This class implements \em active, \em passive and \em symmetric Demons forces given by
 * \f[
 *   f^{active}(x)=\tau\frac{(F(x)-M(x+u(x)))}{\|\nabla M(x+u(x))\|^2 + \kappa|F(x)-M(x+u(x))|^2}\nabla M(x+u(x))
 * \f]
 * \f[
 *   f^{passive}(x)=\tau\frac{(F(x)-M(x+u(x)))}{\|\nabla F(x)\|^2 + \kappa|F(x)-M(x+u(x))|^2}\nabla F(x)
 * \f]
 * \f[
 *   f^{symmetric}(x)=\tau\frac{(F(x)-M(x+u(x)))}{\|\frac{\nabla F(x) + \nabla M(x+u(x))}{2}\|^2 +
 * \kappa|F(x)-M(x+u(x))|^2}\frac{\nabla F(x) + \nabla M(x+u(x))}{2} \f] with \f$\tau\f$ as the step size and
 * \f$\kappa\f$ as the mean squared spacing.
 *
 * \sa VariationalRegistrationFilter
 * \sa VariationalRegistrationFunction
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalRegistrationDemonsFunction
  : public VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationDemonsFunction);

  /** Standard class type alias. */
  using Self = VariationalRegistrationDemonsFunction;
  using Superclass = VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationDemonsFunction, VariationalRegistrationFunction);

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

  /** Image parameter types. */
  using IndexType = typename FixedImageType::IndexType;
  using SizeType = typename FixedImageType::SizeType;
  using SpacingType = typename FixedImageType::SpacingType;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer DisplacementFieldTypePointer;
  /** Various type definitions. */
  using PixelType = typename Superclass::PixelType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  /** Image gradient calculator type. */
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

  /** Set the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(const double threshold)
  {
    m_IntensityDifferenceThreshold = threshold;
  }

  /** Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. */
  virtual double
  GetIntensityDifferenceThreshold() const
  {
    return m_IntensityDifferenceThreshold;
  }

protected:
  VariationalRegistrationDemonsFunction();
  ~VariationalRegistrationDemonsFunction() override = default;

  using GlobalDataStruct = typename Superclass::GlobalDataStruct;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Type of available image forces */
  enum GradientType
  {
    GRADIENT_TYPE_WARPED = 0,
    GRADIENT_TYPE_FIXED = 1,
    GRADIENT_TYPE_SYMMETRIC = 2
  };

private:
  /** Function to compute derivatives of the fixed image. */
  GradientCalculatorPointer m_FixedImageGradientCalculator;

  /** Function to compute derivatives of the warped image. */
  GradientCalculatorPointer m_WarpedImageGradientCalculator;

  /** Set if warped or fixed image gradient is used for force computation. */
  GradientType m_GradientType;

  /** Threshold below which the denominator term is considered zero. */
  double m_DenominatorThreshold;

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold;

  /** Precalculated normalizer for spacing consideration. */
  double m_Normalizer;

  /** Zero update return value (zero vector). */
  PixelType m_ZeroUpdateReturn;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationDemonsFunction.hxx"
#endif

#endif
