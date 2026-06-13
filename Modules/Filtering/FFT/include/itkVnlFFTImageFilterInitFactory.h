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
#ifndef itkVnlFFTImageFilterInitFactory_h
#define itkVnlFFTImageFilterInitFactory_h
#include "ITKFFTExport.h"

#include "itkLightObject.h"

#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
namespace itk
{
/**
 * \class VnlFFTImageFilterInitFactory
 * \brief Deprecated. Registers the (now PocketFFT-backed) Vnl FFT filters.
 *
 * \deprecated The VNL FFT backend was removed; the Vnl* FFT filters are
 * deprecated aliases of the PocketFFT* filters. Use
 * PocketFFTImageFilterInitFactory (registered by default).
 *
 * \ingroup ITKFFT
 */
class ITKFFT_EXPORT VnlFFTImageFilterInitFactory : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlFFTImageFilterInitFactory);

  using Self = VnlFFTImageFilterInitFactory;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkFactorylessNewMacro(Self);
  itkOverrideGetNameOfClassMacro(VnlFFTImageFilterInitFactory);

  /** Mimic factory interface for Python initialization  */
  static void
  RegisterOneFactory()
  {
    RegisterFactories();
  }

  static void
  RegisterFactories();

protected:
  VnlFFTImageFilterInitFactory();
  ~VnlFFTImageFilterInitFactory() override;
};
} // end namespace itk
#endif // !ITK_LEGACY_REMOVE && !ITK_FUTURE_LEGACY_REMOVE

#endif
