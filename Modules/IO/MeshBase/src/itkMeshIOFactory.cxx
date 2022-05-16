/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkMeshIOFactory.h"

namespace itk
{

MeshIOFactory::MeshIOFactory() = default;

MeshIOFactory::~MeshIOFactory() = default;


MeshIOBase::Pointer
MeshIOFactory::CreateMeshIO(const char * path, IOFileModeEnum mode)
{
  std::list<MeshIOBase::Pointer> possibleMeshIO;

  for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkMeshIOBase"))
  {
    auto * io = dynamic_cast<MeshIOBase *>(allobject.GetPointer());

    if (io)
    {
      possibleMeshIO.emplace_back(io);
    }
    else
    {
      std::cerr << "Error MeshIO factory did not return an MeshIOBase: " << allobject->GetNameOfClass() << std::endl;
    }
  }

  for (auto & k : possibleMeshIO)
  {
    if (mode == IOFileModeEnum::ReadMode)
    {
      if (k->CanReadFile(path))
      {
        return k;
      }
    }
    else if (mode == IOFileModeEnum::WriteMode)
    {
      if (k->CanWriteFile(path))
      {
        return k;
      }
    }
  }

  return nullptr;
}

} // end namespace itk
