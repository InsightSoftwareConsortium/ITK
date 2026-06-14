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
#ifndef itkPocketFFTImageFilterInitFactory_h
#define itkPocketFFTImageFilterInitFactory_h
#include "ITKFFTExport.h"

#include "itkLightObject.h"

namespace itk
{
/**
 * \class PocketFFTImageFilterInitFactory
 * \brief Initialize PocketFFT image filter factory backends.
 *
 * The purpose of PocketFFTImageFilterInitFactory is to perform
 * one-time registration of factory objects that handle
 * creation of PocketFFT-backend FFT image filter classes
 * through the ITK object factory singleton mechanism.
 *
 * \ingroup ITKFFT
 */
class ITKFFT_EXPORT PocketFFTImageFilterInitFactory : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PocketFFTImageFilterInitFactory);

  /** Standard class type aliases. */
  using Self = PocketFFTImageFilterInitFactory;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(PocketFFTImageFilterInitFactory);

  /** Mimic factory interface for Python initialization  */
  static void
  RegisterOneFactory()
  {
    RegisterFactories();
  }

  /** Register all PocketFFT factories */
  static void
  RegisterFactories();

protected:
  PocketFFTImageFilterInitFactory();
  ~PocketFFTImageFilterInitFactory() override;
};
} // end namespace itk

#endif
