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
#ifndef itkBruker2dseqImageIOFactory_h
#define itkBruker2dseqImageIOFactory_h
#include "ITKIOBrukerExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/**
 *\class Bruker2dseqImageIOFactory
 * \brief Create instances of Bruker2dseqImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://www.insight-journal.org/browse/publication/237
 *
 * \ingroup ITKIOBruker
 */
class ITKIOBruker_EXPORT Bruker2dseqImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Bruker2dseqImageIOFactory);

  /** Standard class type aliases. */
  using Self = Bruker2dseqImageIOFactory;
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
  itkTypeMacro(Bruker2dseqImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    Bruker2dseqImageIOFactory::Pointer factory = Bruker2dseqImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  Bruker2dseqImageIOFactory();
  ~Bruker2dseqImageIOFactory() override;
};
} // end namespace itk

#endif
