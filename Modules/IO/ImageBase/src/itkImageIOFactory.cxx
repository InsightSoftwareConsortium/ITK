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

#include "itkImageIOFactory.h"

#include <mutex>
#include <vector>

namespace itk
{

namespace
{
// Collect all currently-registered ImageIOBase instances. The mutex
// scopes only the call to ObjectFactoryBase::CreateAllInstance; the
// subsequent CanReadFile/CanWriteFile probing in CreateImageIO runs
// without the lock so independent reads on different files do not
// serialize.
std::vector<ImageIOBase::Pointer>
GetAllImageIOInstances()
{
  static std::mutex                 createImageIOLock;
  const std::lock_guard<std::mutex> mutexHolder(createImageIOLock);

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
  if (mode != IOFileModeEnum::ReadMode && mode != IOFileModeEnum::WriteMode)
  {
    return nullptr;
  }

  auto           imageIOInstances = GetAllImageIOInstances();
  constexpr bool ignoreCase = true;
  const bool     isRead = (mode == IOFileModeEnum::ReadMode);

  // Phase 1: probe only IOs whose advertised extension list already
  // matches `path`. CanReadFile / CanWriteFile typically opens the
  // file and parses its header, so trying extension-matching IOs
  // first turns the common case into a single header-parse instead
  // of one per registered IO.
  for (auto & imageIO : imageIOInstances)
  {
    const bool extensionMatch = isRead ? imageIO->HasSupportedReadExtension(path, ignoreCase)
                                       : imageIO->HasSupportedWriteExtension(path, ignoreCase);
    if (!extensionMatch)
    {
      continue;
    }
    const bool canHandle = isRead ? imageIO->CanReadFile(path) : imageIO->CanWriteFile(path);
    if (canHandle)
    {
      return imageIO;
    }
    // Extension matched but the IO cannot actually handle the file.
    // Null it out so phase 2 does not re-probe.
    imageIO = nullptr;
  }

  // Phase 2: probe IOs not claimed in Phase 1 (skipping nulled-out
  // matches), so an IO with no advertised extension can still rescue.
  for (auto & imageIO : imageIOInstances)
  {
    if (!imageIO)
    {
      continue;
    }
    const bool canHandle = isRead ? imageIO->CanReadFile(path) : imageIO->CanWriteFile(path);
    if (canHandle)
    {
      return imageIO;
    }
  }

  return nullptr;
}

} // end namespace itk
