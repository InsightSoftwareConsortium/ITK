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

#include "itkImageIOFactory.h"

#include <mutex>


namespace itk
{

namespace
{
std::mutex createImageIOLock;
}

ImageIOBase::Pointer
ImageIOFactory::CreateImageIO(const char * path, FileModeType mode)
{
  std::list<ImageIOBase::Pointer> possibleImageIO;

  std::lock_guard<std::mutex> mutexHolder(createImageIOLock);

  for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkImageIOBase"))
  {
    auto * io = dynamic_cast<ImageIOBase *>(allobject.GetPointer());
    if (io)
    {
      possibleImageIO.emplace_back(io);
    }
    else
    {
      std::cerr << "Error ImageIO factory did not return an ImageIOBase: " << allobject->GetNameOfClass() << std::endl;
    }
  }
  for (auto & k : possibleImageIO)
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

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ImageIOFactory::FileModeType value)
{
  return out << [value] {
    switch (value)
    {
      case ImageIOFactory::FileModeType::ReadMode:
        return "ImageIOFactory::FileModeType::ReadMode";
      case ImageIOFactory::FileModeType::WriteMode:
        return "ImageIOFactory::FileModeType::WriteMode";
      default:
        return "INVALID VALUE FOR ImageIOFactory::FileModeType";
    }
  }();
}
} // end namespace itk
