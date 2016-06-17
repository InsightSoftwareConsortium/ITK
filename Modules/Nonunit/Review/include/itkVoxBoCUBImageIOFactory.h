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
#ifndef itkVoxBoCUBImageIOFactory_h
#define itkVoxBoCUBImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class VoxBoCUBImageIOFactory
 *
 * \brief Create instances of VoxBoCUBImageIO objects using an object factory.
 *
 * \author Burstein, Pablo D.; Yushkevich, Paul; Gee, James C.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/303
 *
 * \ingroup ITKReview
 */
class VoxBoCUBImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VoxBoCUBImageIOFactory     Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoxBoCUBImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    VoxBoCUBImageIOFactory::Pointer VoxBoCUBFactory = VoxBoCUBImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(VoxBoCUBFactory);
  }

protected:
  VoxBoCUBImageIOFactory();
  ~VoxBoCUBImageIOFactory();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoxBoCUBImageIOFactory);
};
} // end namespace itk

#endif
