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
#ifndef itkSymmetricForcesDemonsRegistrationFilter_h
#define itkSymmetricForcesDemonsRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkSymmetricForcesDemonsRegistrationFunction.h"

namespace itk
{
/** \class SymmetricForcesDemonsRegistrationFilter
 * \brief Deformably register two images using the demons algorithm.
 *
 * This class was contributed by Corinne Mattmann, ETH Zurich, Switzerland.
 * based on a variation of the DemonsRegistrationFilter. The basic modification
 * is to use equation (5) from Thirion's paper along with the modification for
 * avoiding large deformations when gradients have small values.
 *
 * SymmetricForcesDemonsRegistrationFilter implements the demons deformable algorithm that
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
 * The algorithm has one parameters: the number of iteration to be performed.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in DemonsRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \sa SymmetricForcesDemonsRegistrationFunction
 * \sa DemonsRegistrationFilter
 * \sa DemonsRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKPDEDeformableRegistration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT SymmetricForcesDemonsRegistrationFilter
  : public PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SymmetricForcesDemonsRegistrationFilter);

  /** Standard class type aliases. */
  using Self = SymmetricForcesDemonsRegistrationFilter;
  using Superclass = PDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SymmetricForcesDemonsRegistrationFilter, PDEDeformableRegistrationFilter);

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
    SymmetricForcesDemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This value is calculated for the current iteration */
  virtual double
  GetMetric() const;

  const double &
  GetRMSChange() const override;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void
  SetIntensityDifferenceThreshold(double);

  virtual double
  GetIntensityDifferenceThreshold() const;

protected:
  SymmetricForcesDemonsRegistrationFilter();
  ~SymmetricForcesDemonsRegistrationFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration. */
  void
  InitializeIteration() override;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSymmetricForcesDemonsRegistrationFilter.hxx"
#endif

#endif
