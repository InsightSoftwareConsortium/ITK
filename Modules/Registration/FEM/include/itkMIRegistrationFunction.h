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
#ifndef itkMIRegistrationFunction_h
#define itkMIRegistrationFunction_h

#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPoint.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * \class MIRegistrationFunction
 *
 * This class encapsulates a mutual information force for deformable registration
 * algorithms. It is used by a RegistrationFilter to compute the
 * output displacement field which will map a moving image onto a
 * a fixed image.
 *
 * Non-integer moving image values are obtained by using
 * interpolation. The default interpolator is of type
 * LinearInterpolateImageFunction. The user may set other
 * interpolators via method SetMovingImageInterpolator. Note that the input
 * interpolator must derive from baseclass InterpolateImageFunction.
 *
 * This class is templated over the fixed image type, moving image type,
 * and the displacement field type.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and displacement field type all have the same number of dimensions.
 *
 * \sa MIRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKFEMRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT MIRegistrationFunction
  : public PDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MIRegistrationFunction);

  /** Standard class type aliases. */
  using Self = MIRegistrationFunction;
  using Superclass = PDEDeformableRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MIRegistrationFunction, PDEDeformableRegistrationFunction);

  /** MovingImage image type. */
  using typename Superclass::MovingImageType;
  using typename Superclass::MovingImagePointer;

  /** FixedImage image type. */
  using typename Superclass::FixedImageType;
  using typename Superclass::FixedImagePointer;
  using IndexType = typename FixedImageType::IndexType;
  using SizeType = typename FixedImageType::SizeType;
  using SpacingType = typename FixedImageType::SpacingType;

  /** Displacement field type. */
  using typename Superclass::DisplacementFieldType;
  using typename Superclass::DisplacementFieldTypePointer;

  /** Inherit some enums from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Inherit some enums from the superclass. */
  using typename Superclass::PixelType;
  using typename Superclass::RadiusType;
  using typename Superclass::NeighborhoodType;
  using typename Superclass::FloatOffsetType;
  using typename Superclass::TimeStepType;

  /** Interpolator type. */
  using CoordRepType = double;
  using InterpolatorType = InterpolateImageFunction<MovingImageType, CoordRepType>;
  using InterpolatorPointer = typename InterpolatorType::Pointer;
  using PointType = typename InterpolatorType::PointType;
  using DefaultInterpolatorType = LinearInterpolateImageFunction<MovingImageType, CoordRepType>;

  /** Covariant vector type. */
  using CovariantVectorType = CovariantVector<double, Self::ImageDimension>;

  /** Gradient calculator type. */
  using GradientCalculatorType = CentralDifferenceImageFunction<FixedImageType>;
  using GradientCalculatorPointer = typename GradientCalculatorType::Pointer;

  /** Set the moving image interpolator. */
  void
  SetMovingImageInterpolator(InterpolatorType * ptr)
  {
    m_MovingImageInterpolator = ptr;
  }

  /** Get the moving image interpolator. */
  InterpolatorType *
  GetMovingImageInterpolator()
  {
    return m_MovingImageInterpolator;
  }

  /** This class uses a constant timestep of 1. */
  TimeStepType
  ComputeGlobalTimeStep(void * itkNotUsed(GlobalData)) const override
  {
    return m_TimeStep;
  }

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation. */
  void *
  GetGlobalDataPointer() const override
  {
    auto * global = new GlobalDataStruct();

    return global;
  }

  /** Release memory for global data structure. */
  void
  ReleaseGlobalDataPointer(void * GlobalData) const override
  {
    delete (GlobalDataStruct *)GlobalData;
  }

  /** Set the object's state before each iteration. */
  void
  InitializeIteration() override;

  /** Compute update at a non boundary neighbourhood.
   * This method is called by a finite difference solver image filter at each
   * pixel that does not lie on a data set boundary. */
  PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  void
  SetMinNorm(float ts = 1.0)
  {
    m_Minnorm = ts;
  }

  void
  SetDoInverse(bool b = false)
  {
    m_DoInverse = b;
  }

protected:
  MIRegistrationFunction();
  ~MIRegistrationFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** FixedImage image neighborhood iterator type. */
  using FixedImageNeighborhoodIteratorType = ConstNeighborhoodIterator<FixedImageType>;

  /** A global data type for this class of equation. Used to store
   * iterators for the fixed image. */
  struct GlobalDataStruct
  {
    FixedImageNeighborhoodIteratorType m_FixedImageIterator;
  };

private:
  /** The global timestep. */
  TimeStepType m_TimeStep;

  SpacingType m_FixedImageSpacing;
  PointType   m_FixedImageOrigin;

  GradientCalculatorPointer m_FixedImageGradientCalculator;
  GradientCalculatorPointer m_MovingImageGradientCalculator;

  InterpolatorPointer m_MovingImageInterpolator;

  /** Threshold below which the denominator term is considered zero. */
  double m_DenominatorThreshold{ 1e-9 };

  /** Threshold below which two intensity value are assumed to match. */
  double m_IntensityDifferenceThreshold{ 0.001 };

  mutable double m_MetricTotal{ 0.0 };

  unsigned int m_NumberOfSamples{ 1 };
  unsigned int m_NumberOfBins{ 4 };
  float        m_Minnorm{ 1.0 };

  bool m_DoInverse{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMIRegistrationFunction.hxx"
#endif

#endif
