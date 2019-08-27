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

#include "itkMeshIOFactory.h"

namespace itk
{

MeshIOFactory ::MeshIOFactory() = default;

MeshIOFactory ::~MeshIOFactory() = default;


MeshIOBase::Pointer
MeshIOFactory ::CreateMeshIO(const char * path, FileModeType mode)
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
    if (mode == FileModeType::ReadMode)
    {
      if (k->CanReadFile(path))
      {
        return k;
      }
    }
    else if (mode == FileModeType::WriteMode)
    {
      if (k->CanWriteFile(path))
      {
        return k;
      }
    }
  }

  return nullptr;
}

/**Print enum values */
std::ostream &
operator<<(std::ostream & out, const MeshIOFactory::FileModeType value)
{
  const char * s = 0;
  switch (value)
  {
    case MeshIOFactory::FileModeType::ReadMode:
      s = "MeshIOFactory::FileModeType::ReadMode";
      break;
    case MeshIOFactory::FileModeType::WriteMode:
      s = "MeshIOFactory::FileModeType::WriteMode";
      break;
    default:
      s = "INVALID VALUE FOR MeshIOFactory::FileModeType";
  }
  return out << s;
}

} // end namespace itk
