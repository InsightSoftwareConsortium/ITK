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
#ifndef itkFastSymmetricForcesDemonsRegistrationFilter_h
#define itkFastSymmetricForcesDemonsRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkESMDemonsRegistrationFunction.h"

#include "itkMultiplyImageFilter.h"
#include "itkExponentialDisplacementFieldImageFilter.h"

namespace itk
{
/** \class FastSymmetricForcesDemonsRegistrationFilter
 * \brief Deformably register two images using a symmetric forces demons algorithm.
 *
 * This class was contributed by Tom Vercauteren, INRIA & Mauna Kea Technologies
 * based on a variation of the DemonsRegistrationFilter.
 *
 * FastSymmetricForcesDemonsRegistrationFilter implements the demons deformable algorithm that
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
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/154
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \sa DemonsRegistrationFilter
 * \sa DemonsRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKPDEDeformableRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT FastSymmetricForcesDemonsRegistrationFilter
  : public PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastSymmetricForcesDemonsRegistrationFilter);

  /** Standard class type aliases. */
  using Self = FastSymmetricForcesDemonsRegistrationFilter;
  using Superclass = PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastSymmetricForcesDemonsRegistrationFilter, PDEDeformableRegistrationFilter);

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;

  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This value is calculated for the current iteration */
  virtual double
  GetMetric() const;

  const double &
  GetRMSChange() const override;

  /** DemonsRegistrationFilterFunction type.
   *
   *  FIXME: Why is this the only permissible function ?
   *
   */
  using DemonsRegistrationFunctionType =
    ESMDemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  using GradientType = typename DemonsRegistrationFunctionType::GradientEnum;
  virtual void
  SetUseGradientType(GradientType gtype);

  virtual GradientType
  GetUseGradientType() const;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

  virtual void
  SetMaximumUpdateStepLength(double);

  virtual double
  GetMaximumUpdateStepLength() const;

protected:
  FastSymmetricForcesDemonsRegistrationFilter();
  ~FastSymmetricForcesDemonsRegistrationFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration. */
  void
  InitializeIteration() override;

  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * FiniteDifferenceFilter::GenerateData(). */
  void
  AllocateUpdateBuffer() override;

  /** FiniteDifferenceFunction type. */
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  /** Take timestep type from the FiniteDifferenceFunction. */
  using TimeStepType = typename FiniteDifferenceFunctionType::TimeStepType;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  /** other type alias */
  using MultiplyByConstantType =
    MultiplyImageFilter<DisplacementFieldType, itk::Image<TimeStepType, ImageDimension>, DisplacementFieldType>;

  using AdderType = AddImageFilter<DisplacementFieldType, DisplacementFieldType, DisplacementFieldType>;

  using MultiplyByConstantPointer = typename MultiplyByConstantType::Pointer;
  using AdderPointer = typename AdderType::Pointer;

private:
  /** Downcast the DifferenceFunction using a dynamic_cast to ensure that it is of the correct type.
   * this method will throw an exception if the function is not of the expected type. */
  DemonsRegistrationFunctionType *
  DownCastDifferenceFunctionType();

  const DemonsRegistrationFunctionType *
  DownCastDifferenceFunctionType() const;

  MultiplyByConstantPointer m_Multiplier;
  AdderPointer              m_Adder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastSymmetricForcesDemonsRegistrationFilter.hxx"
#endif

#endif
