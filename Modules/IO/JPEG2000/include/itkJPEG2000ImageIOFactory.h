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
#ifndef itkJPEG2000ImageIOFactory_h
#define itkJPEG2000ImageIOFactory_h


#include "ITKIOJPEG2000Export.h"
#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/**
 *\class JPEG2000ImageIOFactory
 * \brief Supports for the JPEG2000 file format based on openjpeg
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Support for Streaming the JPEG2000 File Format"
 * by Mosaliganti K., Ibanez L., Megason S
 * https://hdl.handle.net/10380/3187
 * http://www.insight-journal.org/browse/publication/741
 *
 *
 *  JPEG2000 offers a large collection of interesting features including:
 *  compression (lossless and lossy), streaming, multi-channel images.
 *
 * \ingroup ITKIOJPEG2000
 */
class ITKIOJPEG2000_EXPORT JPEG2000ImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JPEG2000ImageIOFactory);

  /** Standard class type aliases. */
  using Self = JPEG2000ImageIOFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override;

  const char *
  GetDescription() const override;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static JPEG2000ImageIOFactory *
  FactoryNew()
  {
    return new JPEG2000ImageIOFactory;
  }

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    JPEG2000ImageIOFactory::Pointer factory = JPEG2000ImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  JPEG2000ImageIOFactory();
  ~JPEG2000ImageIOFactory() override;
};
} // end namespace itk

#endif
