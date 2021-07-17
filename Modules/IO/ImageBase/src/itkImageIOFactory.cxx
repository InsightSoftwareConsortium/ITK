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

#include "itkImageIOFactory.h"

#include <mutex>
#include <vector>

namespace itk
{

namespace
{
std::vector<ImageIOBase::Pointer>
GetAllImageIOInstances()
{
  static std::mutex           createImageIOLock;
  std::lock_guard<std::mutex> mutexHolder(createImageIOLock);

  const auto                        allInstances = ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
  std::vector<ImageIOBase::Pointer> result;
  result.reserve(allInstances.size());
  for (auto & possibleImageIO : allInstances)
  {
    auto * io = dynamic_cast<ImageIOBase *>(possibleImageIO.GetPointer());
    if (io)
    {
      result.emplace_back(io);
    }
    else
    {
      std::cerr << "Error ImageIO factory did not return an ImageIOBase: " << possibleImageIO->GetNameOfClass() << '\n';
    }
  }
  return result;
}
} // namespace

ImageIOBase::Pointer
ImageIOFactory::CreateImageIO(const char * path, IOFileModeEnum mode)
{
  if (mode != FileModeType::ReadMode && mode != FileModeType::WriteMode)
  {
    return nullptr;
  }

  auto       imageIOInstances = GetAllImageIOInstances();
  const bool ignoreCase = true;
  // check instances that support file extension
  for (auto & imageIO : imageIOInstances)
  {
    if (mode == FileModeType::ReadMode)
    {
      if (imageIO->HasSupportedReadExtension(path, ignoreCase))
      {
        if (imageIO->CanReadFile(path))
        {
          return imageIO;
        }
        else
        {
          imageIO = nullptr; // don't check it again
        }
      }
    }
    else
    {
      if (imageIO->HasSupportedWriteExtension(path, ignoreCase))
      {
        if (imageIO->CanWriteFile(path))
        {
          return imageIO;
        }
        else
        {
          imageIO = nullptr;
        }
      }
    }
  }

  for (auto & imageIO : imageIOInstances)
  {

    if (!imageIO)
    {
      continue;
    }
    if (mode == IOFileModeEnum::ReadMode)
    {
      if (imageIO->CanReadFile(path))
      {
        return imageIO;
      }
    }
    else if (mode == IOFileModeEnum::WriteMode)
    {
      if (imageIO->CanWriteFile(path))
      {
        return imageIO;
      }
    }
  }
  return nullptr;
}

} // end namespace itk
