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
#ifndef itkDemonsRegistrationFilter_h
#define itkDemonsRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkDemonsRegistrationFunction.h"

namespace itk
{
/** \class DemonsRegistrationFilter
 * \brief Deformably register two images using the demons algorithm.
 *
 * DemonsRegistrationFilter implements the demons deformable algorithm that
 * register two images by computing the displacement field which will map a
 * moving image onto a fixed image.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the displacement field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial displacement field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The algorithm has one parameters: the number of iteration to be performed.
 *
 * The output displacement field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in DemonsRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and displacement field type all have the same number of dimensions.
 *
 * \sa DemonsRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKPDEDeformableRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT DemonsRegistrationFilter
  : public PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DemonsRegistrationFilter);

  /** Standard class type aliases. */
  using Self = DemonsRegistrationFilter;
  using Superclass = PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DemonsRegistrationFilter, PDEDeformableRegistrationFilter);

  /** Inherit types from superclass. */
  using TimeStepType = typename Superclass::TimeStepType;

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** displacement field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;

  /** FiniteDifferenceFunction type. */
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  /** DemonsRegistrationFilterFunction type. */
  using DemonsRegistrationFunctionType =
    DemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This is value is only available for the previous iteration and
   * NOT the current iteration. */
  virtual double
  GetMetric() const;

  /** Switch between using the fixed image and moving image gradient
   * for computing the displacement field updates. */
  itkSetMacro(UseMovingImageGradient, bool);
  itkGetConstMacro(UseMovingImageGradient, bool);
  itkBooleanMacro(UseMovingImageGradient);

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

protected:
  DemonsRegistrationFilter();
  ~DemonsRegistrationFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration. */
  void
  InitializeIteration() override;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override
  {}

private:
  bool m_UseMovingImageGradient;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDemonsRegistrationFilter.hxx"
#endif

#endif
