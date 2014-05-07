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

#include "itkBYUMeshIOFactory.h"
#include "itkFreeSurferAsciiMeshIOFactory.h"
#include "itkFreeSurferBinaryMeshIOFactory.h"
#include "itkGiftiMeshIOFactory.h"
#include "itkMeshIOFactory.h"
#include "itkOBJMeshIOFactory.h"
#include "itkOFFMeshIOFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"
#include "itkVTKPolyDataMeshIOFactory.h"

namespace itk
{

MeshIOFactory
::MeshIOFactory()
{
}

MeshIOFactory
::~MeshIOFactory()
{
}


MeshIOBase::Pointer
MeshIOFactory
::CreateMeshIO(const char *path, FileModeType mode)
{
  RegisterBuiltInFactories();

  std::list< MeshIOBase::Pointer >  possibleMeshIO;
  std::list< LightObject::Pointer > allobjects = ObjectFactoryBase::CreateAllInstance("itkMeshIOBase");

  for ( std::list< LightObject::Pointer >::iterator it = allobjects.begin(); it != allobjects.end(); ++it )
    {
    MeshIOBase *io = dynamic_cast< MeshIOBase * >( it->GetPointer() );

    if ( io )
      {
      possibleMeshIO.push_back(io);
      }
    else
      {
      std::cerr << "Error MeshIO factory did not return an MeshIOBase: "
                << ( *it )->GetNameOfClass()
                << std::endl;
      }
    }

  for ( std::list< MeshIOBase::Pointer >::iterator k = possibleMeshIO.begin(); k != possibleMeshIO.end(); ++k )
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

  return ITK_NULLPTR;
}


void
MeshIOFactory
::RegisterBuiltInFactories()
{
  static SimpleMutexLock mutex;

    {
    static bool firstTime = true;
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
      ObjectFactoryBase::RegisterFactory( BYUMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( FreeSurferAsciiMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( FreeSurferBinaryMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( GiftiMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( OBJMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( OFFMeshIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( VTKPolyDataMeshIOFactory::New() );

      firstTime = false;
      }
    }
}


} // end namespace itk
