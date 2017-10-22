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
#ifndef itkBYUMeshIOFactory_h
#define itkBYUMeshIOFactory_h
#include "ITKIOMeshExport.h"

#include "itkMeshIOBase.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class BYUMeshIOFactory
   * \brief Create instances of BYUMeshIO objects using an object factory.
   * \ingroup ITKIOMesh
   */
class ITKIOMesh_EXPORT BYUMeshIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef BYUMeshIOFactory           Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BYUMeshIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
    BYUMeshIOFactory::Pointer byuFactory = BYUMeshIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(byuFactory);
    }

protected:
  BYUMeshIOFactory();
  ~BYUMeshIOFactory() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BYUMeshIOFactory);
};

} // end namespace itk

#endif
