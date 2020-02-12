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
#ifndef itkDiffeomorphicDemonsRegistrationFilter_h
#define itkDiffeomorphicDemonsRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkESMDemonsRegistrationFunction.h"

#include "itkMultiplyImageFilter.h"
#include "itkExponentialDisplacementFieldImageFilter.h"

namespace itk
{
/** \class DiffeomorphicDemonsRegistrationFilter
 * \brief Deformably register two images using a diffeomorphic demons algorithm.
 *
 * This class was contributed by Tom Vercauteren, INRIA & Mauna Kea Technologies,
 * based on a variation of the DemonsRegistrationFilter. The basic modification
 * is to use diffeomorphism exponentials.
 *
 * See T. Vercauteren, X. Pennec, A. Perchant and N. Ayache,
 * "Non-parametric Diffeomorphic Image Registration with the Demons Algorithm",
 * Proc. of MICCAI 2007.
 *
 * DiffeomorphicDemonsRegistrationFilter implements the demons deformable algorithm that
 * register two images by computing the deformation field which will map a
 * moving image onto a fixed image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in DemonsRegistrationFunction.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \sa DemonsRegistrationFilter
 * \sa DemonsRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKPDEDeformableRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT DiffeomorphicDemonsRegistrationFilter
  : public PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DiffeomorphicDemonsRegistrationFilter);

  /** Standard class type aliases. */
  using Self = DiffeomorphicDemonsRegistrationFilter;
  using Superclass = PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DiffeomorphicDemonsRegistrationFilter, PDEDeformableRegistrationFilter);

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;

  /** FiniteDifferenceFunction type. */
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  /** Take timestep type from the FiniteDifferenceFunction. */
  using TimeStepType = typename FiniteDifferenceFunctionType::TimeStepType;

  /** DemonsRegistrationFilterFunction type. */
  using DemonsRegistrationFunctionType =
    ESMDemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;
  using GradientType = typename DemonsRegistrationFunctionType::GradientEnum;

  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This value is calculated for the current iteration */
  virtual double
  GetMetric() const;

  const double &
  GetRMSChange() const override;

  virtual void
  SetUseGradientType(GradientType gtype);

  virtual GradientType
  GetUseGradientType() const;

  /** Use a first-order approximation of the exponential.
   *  This amounts to using an update rule of the type
   *  s <- s o (Id + u) instead of s <- s o exp(u) */
  itkSetMacro(UseFirstOrderExp, bool);
  itkGetConstMacro(UseFirstOrderExp, bool);
  itkBooleanMacro(UseFirstOrderExp);

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

  /** Set/Get the maximum length in terms of pixels of
   *  the vectors in the update buffer. */
  virtual void
  SetMaximumUpdateStepLength(double);

  virtual double
  GetMaximumUpdateStepLength() const;

protected:
  DiffeomorphicDemonsRegistrationFilter();
  ~DiffeomorphicDemonsRegistrationFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration. */
  void
  InitializeIteration() override;

  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * FiniteDifferenceFilter::GenerateData(). */
  void
  AllocateUpdateBuffer() override;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

private:
  /** Downcast the DifferenceFunction using a dynamic_cast to ensure that it is of the correct type.
   * this method will throw an exception if the function is not of the expected type. */
  DemonsRegistrationFunctionType *
  DownCastDifferenceFunctionType();

  const DemonsRegistrationFunctionType *
  DownCastDifferenceFunctionType() const;

  /** Exp and composition type alias */
  using MultiplyByConstantType =
    MultiplyImageFilter<DisplacementFieldType, itk::Image<TimeStepType, ImageDimension>, DisplacementFieldType>;

  using FieldExponentiatorType = ExponentialDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>;

  using VectorWarperType = WarpVectorImageFilter<DisplacementFieldType, DisplacementFieldType, DisplacementFieldType>;

  using FieldInterpolatorType =
    VectorLinearInterpolateNearestNeighborExtrapolateImageFunction<DisplacementFieldType, double>;

  using AdderType = AddImageFilter<DisplacementFieldType, DisplacementFieldType, DisplacementFieldType>;

  using MultiplyByConstantPointer = typename MultiplyByConstantType::Pointer;
  using FieldExponentiatorPointer = typename FieldExponentiatorType::Pointer;
  using VectorWarperPointer = typename VectorWarperType::Pointer;
  using FieldInterpolatorPointer = typename FieldInterpolatorType::Pointer;
  using FieldInterpolatorOutputType = typename FieldInterpolatorType::OutputType;
  using AdderPointer = typename AdderType::Pointer;

  MultiplyByConstantPointer m_Multiplier;
  FieldExponentiatorPointer m_Exponentiator;
  VectorWarperPointer       m_Warper;
  AdderPointer              m_Adder;
  bool                      m_UseFirstOrderExp{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDiffeomorphicDemonsRegistrationFilter.hxx"
#endif

#endif
