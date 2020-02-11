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
#ifndef itkGPUGradientAnisotropicDiffusionImageFilterFactory_h
#define itkGPUGradientAnisotropicDiffusionImageFilterFactory_h

#include "itkGPUGradientAnisotropicDiffusionImageFilter.h"

namespace itk
{

/**
 * \class GPUGradientAnisotropicDiffusionImageFilterFactory
 * Object Factory implementation for GPUGradientAnisotropicDiffusionImageFilter
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
class GPUGradientAnisotropicDiffusionImageFilterFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUGradientAnisotropicDiffusionImageFilterFactory);

  using Self = GPUGradientAnisotropicDiffusionImageFilterFactory;
  using Superclass = ObjectFactoryBase;
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
    return "A Factory for GPUGradientAnisotropicDiffusionImageFilter";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUGradientAnisotropicDiffusionImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    GPUGradientAnisotropicDiffusionImageFilterFactory::Pointer factory =
      GPUGradientAnisotropicDiffusionImageFilterFactory::New();

    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
#define GradientAnisotropicDiffusionImageFilterTypeMacro(ipt, opt, dm)                                                 \
  {                                                                                                                    \
    using InputImageType = itk::Image<ipt, dm>;                                                                        \
    using OutputImageType = itk::Image<opt, dm>;                                                                       \
    this->RegisterOverride(                                                                                            \
      typeid(itk::GradientAnisotropicDiffusionImageFilter<InputImageType, OutputImageType>).name(),                    \
      typeid(itk::GPUGradientAnisotropicDiffusionImageFilter<InputImageType, OutputImageType>).name(),                 \
      "GPU GradientAnisotropicDiffusionImageFilter Override",                                                          \
      true,                                                                                                            \
      itk::CreateObjectFunction<GPUGradientAnisotropicDiffusionImageFilter<InputImageType, OutputImageType>>::New());  \
  }

  GPUGradientAnisotropicDiffusionImageFilterFactory()
  {
    if (IsGPUAvailable())
    {
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, float, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, float, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float, float, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int, double, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int, double, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double, double, 1);

      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, float, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, float, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float, float, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int, double, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int, double, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double, double, 2);

      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, float, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, float, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float, float, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int, double, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int, double, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double, double, 3);
    }
  }
};

} // namespace itk

#endif
