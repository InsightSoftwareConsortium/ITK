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
#ifndef itkFFTWFFTImageFilterInitFactory_h
#define itkFFTWFFTImageFilterInitFactory_h
#include "ITKFFTExport.h"

#include "itkLightObject.h"

namespace itk
{
/**
 * \class FFTWFFTImageFilterInitFactory
 * \brief Initialize FFTW FFT image filter factory backends.
 *
 * The purpose of FFTWFFTImageFilterInitFactory is to perform
 * one-time registration of factory objects that handle
 * creation of FFTW-backend FFT image filter classes
 * through the ITK object factory singleton mechanism.
 *
 * \ingroup ITKFFT
 */
class ITKFFT_EXPORT FFTWFFTImageFilterInitFactory : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWFFTImageFilterInitFactory);

  /** Standard class type aliases. */
  using Self = FFTWFFTImageFilterInitFactory;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWFFTImageFilterInitFactory, LightObject);

  /** Register one factory of this type.
   * The purpose of an InitFactory is to simply load other factories
   * in its constructor, so nothing is returned here.
   * Method is required for factory to load correct in Python.  */
  static void
  RegisterOneFactory()
  {
    RegisterFactories();
  }

  static void
  RegisterFactories();

protected:
  FFTWFFTImageFilterInitFactory();
  ~FFTWFFTImageFilterInitFactory() override;
};
} // end namespace itk

#endif
