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
#ifndef itkHDF5TransformIOFactory_h
#define itkHDF5TransformIOFactory_h
#include "ITKIOTransformHDF5Export.h"

#include "itkObjectFactoryBase.h"
#include "itkTransformIOBase.h"

namespace itk
{
/** \class HDF5TransformIOFactory
 * \brief Create instances of HDF5TransformIO objects using an object factory.
 *
 * \ingroup ITKIOTransformHDF5
 */
class ITKIOTransformHDF5_EXPORT HDF5TransformIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HDF5TransformIOFactory);

  /** Standard class type aliases. */
  using Self = HDF5TransformIOFactory;
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
  itkTypeMacro(HDF5TransformIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    HDF5TransformIOFactory::Pointer metaFactory = HDF5TransformIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(metaFactory);
  }

protected:
  HDF5TransformIOFactory();
  ~HDF5TransformIOFactory() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#endif
