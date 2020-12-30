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
#ifndef itkGPUDemonsRegistrationFilter_h
#define itkGPUDemonsRegistrationFilter_h

#include "itkOpenCLUtil.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkGPUPDEDeformableRegistrationFilter.h"
#include "itkGPUDemonsRegistrationFunction.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class GPUDemonsRegistrationFilter
 *
 * \brief Deformably register two images using the demons algorithm with GPU.
 *
 * GPUDemonsRegistrationFilter implements the demons deformable algorithm that
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
 * for each iteration is computed in GPUDemonsRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \sa GPUDemonsRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKGPUPDEDeformableRegistration
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TParentImageFilter = itk::DemonsRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>>
class ITK_TEMPLATE_EXPORT GPUDemonsRegistrationFilter
  : public GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUDemonsRegistrationFilter);

  /** Standard class type aliases. */
  using Self = GPUDemonsRegistrationFilter;
  using GPUSuperclass =
    GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>;
  using CPUSuperclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUDemonsRegistrationFilter, GPUPDEDeformableRegistrationFilter);

  /** Inherit types from GPUSuperclass. */
  using TimeStepType = typename GPUSuperclass::TimeStepType;

  /** FixedImage image type. */
  using FixedImageType = typename GPUSuperclass::FixedImageType;
  using FixedImagePointer = typename GPUSuperclass::FixedImagePointer;

  /** MovingImage image type. */
  using MovingImageType = typename GPUSuperclass::MovingImageType;
  using MovingImagePointer = typename GPUSuperclass::MovingImagePointer;

  /** Deformation field type. */
  using DisplacementFieldType = typename GPUSuperclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename GPUSuperclass::DisplacementFieldPointer;

  /** FiniteDifferenceFunction type. */
  using FiniteDifferenceFunctionType = typename GPUSuperclass::FiniteDifferenceFunctionType;

  /** GPUDemonsRegistrationFilterFunction type. */
  using GPUDemonsRegistrationFunctionType =
    GPUDemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This is value is only available for the previous iteration and
   * NOT the current iteration. */
  double
  GetMetric() const override;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  void
  SetIntensityDifferenceThreshold(double) override;

  double
  GetIntensityDifferenceThreshold() const override;

protected:
  GPUDemonsRegistrationFilter();
  ~GPUDemonsRegistrationFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration. */
  void
  InitializeIteration() override;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

private:
  bool m_UseMovingImageGradient;
};

/** \class GPUDemonsRegistrationFilterFactory
 *
 * \brief Object Factory implementation for GPUDemonsRegistrationFilter
 * \ingroup ITKGPUPDEDeformableRegistration
 */
class GPUDemonsRegistrationFilterFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUDemonsRegistrationFilterFactory);

  using Self = GPUDemonsRegistrationFilterFactory;
  using GPUSuperclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override
  {
    return ITK_SOURCE_VERSION;
  }
  const char *
  GetDescription() const override
  {
    return "A Factory for GPUDemonsRegistrationFilter";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUDemonsRegistrationFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    GPUDemonsRegistrationFilterFactory::Pointer factory = GPUDemonsRegistrationFilterFactory::New();

    ObjectFactoryBase::RegisterFactory(factory);
  }

private:
#define OverrideDemonsRegistrationFilterTypeMacro(ipt, opt, dm)                                                        \
  {                                                                                                                    \
    using InputImageType = GPUImage<ipt, dm>;                                                                          \
    using OutputImageType = GPUImage<opt, dm>;                                                                         \
    using VectorPixelType = Vector<float, dm>;                                                                         \
    using DisplacementFieldType = GPUImage<VectorPixelType, dm>;                                                       \
    this->RegisterOverride(                                                                                            \
      typeid(DemonsRegistrationFilter<InputImageType, OutputImageType, DisplacementFieldType>).name(),                 \
      typeid(GPUDemonsRegistrationFilter<InputImageType, OutputImageType, DisplacementFieldType>).name(),              \
      "GPU Demons Registration Filter Override",                                                                       \
      true,                                                                                                            \
      CreateObjectFunction<                                                                                            \
        GPUDemonsRegistrationFilter<InputImageType, OutputImageType, DisplacementFieldType>>::New());                  \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

  GPUDemonsRegistrationFilterFactory()
  {
    if (IsGPUAvailable())
    {
      OverrideDemonsRegistrationFilterTypeMacro(unsigned char, unsigned char, 1);
      OverrideDemonsRegistrationFilterTypeMacro(char, char, 1);
      OverrideDemonsRegistrationFilterTypeMacro(float, float, 1);
      OverrideDemonsRegistrationFilterTypeMacro(int, int, 1);
      OverrideDemonsRegistrationFilterTypeMacro(unsigned int, unsigned int, 1);
      OverrideDemonsRegistrationFilterTypeMacro(double, double, 1);

      OverrideDemonsRegistrationFilterTypeMacro(unsigned char, unsigned char, 2);
      OverrideDemonsRegistrationFilterTypeMacro(char, char, 2);
      OverrideDemonsRegistrationFilterTypeMacro(float, float, 2);
      OverrideDemonsRegistrationFilterTypeMacro(int, int, 2);
      OverrideDemonsRegistrationFilterTypeMacro(unsigned int, unsigned int, 2);
      OverrideDemonsRegistrationFilterTypeMacro(double, double, 2);

      OverrideDemonsRegistrationFilterTypeMacro(unsigned char, unsigned char, 3);
      OverrideDemonsRegistrationFilterTypeMacro(char, char, 3);
      OverrideDemonsRegistrationFilterTypeMacro(float, float, 3);
      OverrideDemonsRegistrationFilterTypeMacro(int, int, 3);
      OverrideDemonsRegistrationFilterTypeMacro(unsigned int, unsigned int, 3);
      OverrideDemonsRegistrationFilterTypeMacro(double, double, 3);
    }
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUDemonsRegistrationFilter.hxx"
#endif

#endif
