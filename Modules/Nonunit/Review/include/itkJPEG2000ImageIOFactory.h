/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class JPEG2000ImageIOFactory
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
 * \ingroup ITKReview
 */
class JPEG2000ImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef JPEG2000ImageIOFactory     Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion() const ITK_OVERRIDE;

  virtual const char * GetDescription() const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static JPEG2000ImageIOFactory * FactoryNew() { return new JPEG2000ImageIOFactory; }

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    JPEG2000ImageIOFactory::Pointer metaFactory = JPEG2000ImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  JPEG2000ImageIOFactory();
  ~JPEG2000ImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JPEG2000ImageIOFactory);
};
} // end namespace itk

#endif
