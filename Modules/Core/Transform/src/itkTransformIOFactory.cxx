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
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkTransformIOFactory.h"
#include "itkTxtTransformIOFactory.h"
#include "itkMatlabTransformIOFactory.h"
#include "itkHDF5TransformIOFactory.h"

#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{
TransformIOBase::Pointer
TransformIOFactory::CreateTransformIO(const char *path, FileModeType mode)
{
  RegisterBuiltInFactories();

  std::list< TransformIOBase::Pointer > possibleTransformIO;
  std::list< LightObject::Pointer >     allobjects =
    ObjectFactoryBase::CreateAllInstance("itkTransformIOBase");
  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {
    TransformIOBase *io = dynamic_cast< TransformIOBase * >( i->GetPointer() );
    if ( io )
      {
      possibleTransformIO.push_back(io);
      }
    else
      {
      std::cerr << "Error TransformIO factory did not return an TransformIOBase: "
                << ( *i )->GetNameOfClass()
                << std::endl;
      }
    }
  for ( std::list< TransformIOBase::Pointer >::iterator k = possibleTransformIO.begin();
        k != possibleTransformIO.end(); ++k )
    {
    if ( mode == ReadMode )
      {
      if ( ( *k )->CanReadFile(path) )
        {
        return *k;
        }
      }
    else if ( mode == WriteMode )
      {
      if ( ( *k )->CanWriteFile(path) )
        {
        return *k;
        }
      }
    }
  return 0;
}

void
TransformIOFactory::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
      //      ObjectFactoryBase::RegisterFactory( GDCMTransformIOFactory::New()
      // );
      ObjectFactoryBase::RegisterFactory( MatlabTransformIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( TxtTransformIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( HDF5TransformIOFactory::New() );
      firstTime = false;
      }
    }
}
} // end namespace itk
