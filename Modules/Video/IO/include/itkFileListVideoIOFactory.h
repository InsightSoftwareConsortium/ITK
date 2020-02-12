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
#ifndef itkFileListVideoIOFactory_h
#define itkFileListVideoIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkVideoIOBase.h"
#include "ITKVideoIOExport.h"

namespace itk
{
/**
 *\class FileListVideoIOFactory
 * \brief Create instances of FileListVideoIO objects using an object factory.
 *
 * \ingroup ITKVideoIO
 */
class ITKVideoIO_EXPORT FileListVideoIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FileListVideoIOFactory);

  /** Standard class type aliases. */
  using Self = FileListVideoIOFactory;
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

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileListVideoIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    FileListVideoIOFactory::Pointer FileListFactory = FileListVideoIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(FileListFactory);
  }

protected:
  FileListVideoIOFactory();
  ~FileListVideoIOFactory() override;
};
} // end namespace itk

#endif
