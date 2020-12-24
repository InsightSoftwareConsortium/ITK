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
#ifndef itkMutualInformationImageToImageMetric_h
#define itkMutualInformationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkPoint.h"

#include "itkIndex.h"
#include "itkKernelFunctionBase.h"

namespace itk
{
/** \class MutualInformationImageToImageMetric
 * \brief Computes the mutual information between two images to be registered
 *
 * MutualInformationImageToImageMetric computes the mutual information
 * between a fixed and moving image to be registered.
 *
 * This class is templated over the FixedImage type and the MovingImage type.
 *
 * The fixed and moving images are set via methods SetFixedImage() and
 * SetMovingImage(). This metric makes use of user specified Transform and
 * Interpolator. The Transform is used to map points from the fixed image to
 * the moving image domain. The Interpolator is used to evaluate the image
 * intensity at user specified geometric points in the moving image.
 * The Transform and Interpolator are set via methods SetTransform() and
 * SetInterpolator().
 *
 * \warning This metric assumes that the moving image has already been
 * connected to the interpolator outside of this class.
 *
 * The method GetValue() computes of the mutual information
 * while method GetValueAndDerivative() computes
 * both the mutual information and its derivatives with respect to the
 * transform parameters.
 *
 * The calculations are based on the method of Viola and Wells
 * where the probability density distributions are estimated using
 * Parzen windows.
 *
 * By default a Gaussian kernel is used in the density estimation.
 * Other option include Cauchy and spline-based. A user can specify
 * the kernel passing in a pointer a KernelFunctionBase using the
 * SetKernelFunction() method.
 *
 * Mutual information is estimated using two sample sets: one to calculate
 * the singular and joint pdf's and one to calculate the entropy
 * integral. By default 50 samples points are used in each set.
 * Other values can be set via the SetNumberOfSpatialSamples() method.
 *
 * Quality of the density estimate depends on the choice of the
 * kernel's standard deviation. Optimal choice will depend on the images.
 * It is can be shown that around the optimal variance, the mutual
 * information estimate is relatively insensitive to small changes
 * of the standard deviation. In our experiments, we have found that a
 * standard deviation of 0.4 works well for images normalized to have a mean
 * of zero and standard deviation of 1.0.
 * The variance can be set via methods SetFixedImageStandardDeviation()
 * and SetMovingImageStandardDeviation().
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997).
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * \sa KernelFunctionBase
 * \sa GaussianKernelFunction
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 *
 * \sphinx
 * \sphinxexample{Registration/Common/MutualInformation,Mutual Information}
 * \sphinxexample{Core/Transform/MutualInformationAffine,Mutual Information Affine}
 * \endsphinx
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT MutualInformationImageToImageMetric : public ImageToImageMetric<TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MutualInformationImageToImageMetric);

  /** Standard class type aliases. */
  using Self = MutualInformationImageToImageMetric;
  using Superclass = ImageToImageMetric<TFixedImage, TMovingImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MutualInformationImageToImageMetric, ImageToImageMetric);

  /** Types inherited from Superclass. */
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformJacobianType = typename Superclass::TransformJacobianType;
  using InterpolatorType = typename Superclass::InterpolatorType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using ParametersType = typename Superclass::ParametersType;
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;
  using FixedImageConstPointer = typename Superclass::FixedImageConstPointer;
  using MovingImageCosntPointer = typename Superclass::MovingImageConstPointer;

  /** Index and Point type alias support */
  using FixedImageIndexType = typename FixedImageType::IndexType;
  using FixedImageIndexValueType = typename FixedImageIndexType::IndexValueType;
  using MovingImageIndexType = typename MovingImageType::IndexType;
  using FixedImagePointType = typename TransformType::InputPointType;
  using MovingImagePointType = typename TransformType::OutputPointType;

  using KernelFunctionType = KernelFunctionBase<double>;

  /** Enum of the moving image dimension. */
  static constexpr unsigned int MovingImageDimension = MovingImageType::ImageDimension;

  /** Get the derivatives of the match measure. */
  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override;

  /**  Get the value. */
  MeasureType
  GetValue(const ParametersType & parameters) const override;

  /**  Get the value and derivatives for single valued optimizers. */
  void
  GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType &          value,
                        DerivativeType &       derivative) const override;

  /** Set the number of spatial samples. This is the number of image
   * samples used to calculate the joint probability distribution.
   * The number of spatial samples is clamped to be a minimum of 1.
   * Default value is 50. */
  void
  SetNumberOfSpatialSamples(unsigned int num);

  /** Get the number of spatial samples. */
  itkGetConstReferenceMacro(NumberOfSpatialSamples, unsigned int);

  /** Set/Get the moving image intensity standard deviation. This defines
   * the kernel bandwidth used in the joint probability distribution
   * calculation. Default value is 0.4 which works well for image intensities
   * normalized to a mean of 0 and standard deviation of 1.0.
   * Value is clamped to be always greater than zero. */
  itkSetClampMacro(MovingImageStandardDeviation,
                   double,
                   NumericTraits<double>::NonpositiveMin(),
                   NumericTraits<double>::max());
  itkGetConstReferenceMacro(MovingImageStandardDeviation, double);

  /** Set/Get the fixed image intensity standard deviation. This defines
   * the kernel bandwidth used in the joint probability distribution
   * calculation. Default value is 0.4 which works well for image intensities
   * normalized to a mean of 0 and standard deviation of 1.0.
   * Value is clamped to be always greater than zero. */
  itkSetClampMacro(FixedImageStandardDeviation,
                   double,
                   NumericTraits<double>::NonpositiveMin(),
                   NumericTraits<double>::max());
  itkGetConstMacro(FixedImageStandardDeviation, double);

  /** Set/Get the kernel function. This is used to calculate the joint
   * probability distribution. Default is the GaussianKernelFunction. */
  itkSetObjectMacro(KernelFunction, KernelFunctionType);
  itkGetModifiableObjectMacro(KernelFunction, KernelFunctionType);


protected:
  MutualInformationImageToImageMetric();
  ~MutualInformationImageToImageMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** \class SpatialSample
   * A spatial sample consists of the fixed domain point, the fixed
   * image value at that point, and the corresponding moving image value.
   * \ingroup ITKRegistrationCommon
   */
  class SpatialSample
  {
  public:
    SpatialSample() { FixedImagePointValue.Fill(0.0); }
    ~SpatialSample() = default;

    FixedImagePointType FixedImagePointValue;
    double              FixedImageValue{ 0.0 };
    double              MovingImageValue{ 0.0 };
  };

  /** SpatialSampleContainer type alias support */
  using SpatialSampleContainer = std::vector<SpatialSample>;

  /** Container to store sample set  A - used to approximate the probability
   * density function (pdf). */
  mutable SpatialSampleContainer m_SampleA;

  /** Container to store sample set  B - used to approximate the mutual
   * information value. */
  mutable SpatialSampleContainer m_SampleB;

  unsigned int m_NumberOfSpatialSamples;
  double       m_MovingImageStandardDeviation;
  double       m_FixedImageStandardDeviation;
  double       m_MinProbability;

  typename KernelFunctionType::Pointer m_KernelFunction;

  /** Uniformly select samples from the fixed image buffer.
   * \warning Note that this method has a different signature than the one in
   * the base OptImageToImageMetric and therefore they are not intended to
   * provide polymorphism. That is, this function is not overriding the one in
   * the base class. */
  virtual void
  SampleFixedImageDomain(SpatialSampleContainer & samples) const;

  /**
   * Calculate the intensity derivatives at a point
   */
  void
  CalculateDerivatives(const FixedImagePointType &, DerivativeType &, TransformJacobianType &) const;

  using CoordinateRepresentationType = typename Superclass::CoordinateRepresentationType;
  using DerivativeFunctionType = CentralDifferenceImageFunction<MovingImageType, CoordinateRepresentationType>;

  typename DerivativeFunctionType::Pointer m_DerivativeCalculator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMutualInformationImageToImageMetric.hxx"
#endif

#endif
