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
#ifndef itkAnalyzeObjectLabelMapImageIOFactory_h
#define itkAnalyzeObjectLabelMapImageIOFactory_h

#ifdef _MSC_VER
#  pragma warning(disable : 4786)
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class AnalyzeObjectLabelMapImageIOFactory
 *  \ingroup AnalyzeObjectMapIO
 *  \brief Create instances of AnalyzeObjectLabelMapImageIO objects using an object factory.
 */
class ITK_EXPORT AnalyzeObjectLabelMapImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class type alias. */
  using Self = AnalyzeObjectLabelMapImageIOFactory;
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
  itkTypeMacro(AnalyzeObjectLabelMapImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    AnalyzeObjectLabelMapImageIOFactory::Pointer metaFactory = AnalyzeObjectLabelMapImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  AnalyzeObjectLabelMapImageIOFactory();
  ~AnalyzeObjectLabelMapImageIOFactory() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  AnalyzeObjectLabelMapImageIOFactory(const Self &) = delete; // purposely not implemented
  void
  operator=(const Self &) = delete; // purposely not implemented
};

} // end namespace itk

#endif
