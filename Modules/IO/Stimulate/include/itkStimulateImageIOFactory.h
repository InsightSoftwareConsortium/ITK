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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkStimulateImageIOFactory_h
#define itkStimulateImageIOFactory_h
#include "ITKIOStimulateExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class StimulateImageIOFactory
 * \brief Create instances of StimulateImageIO objects using an object factory.
 * \ingroup ITKIOStimulate
 */
class ITKIOStimulate_EXPORT StimulateImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(StimulateImageIOFactory);

  /** Standard class type aliases. */
  using Self = StimulateImageIOFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class Methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override;

  const char *
  GetDescription() const override;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StimulateImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    StimulateImageIOFactory::Pointer stimulateFactory = StimulateImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(stimulateFactory);
  }

protected:
  StimulateImageIOFactory();
  ~StimulateImageIOFactory() override;
};
} // end namespace itk

#endif
