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
#ifdef _MSC_VER
#  pragma warning(disable : 4786)
#endif

#include "itkVideoIOFactory.h"


namespace itk
{
VideoIOBase::Pointer
VideoIOFactory::CreateVideoIO(IOModeEnum mode, const char * arg)
{
  std::list<VideoIOBase::Pointer> possibleVideoIO;
  for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkVideoIOBase"))
  {

    auto * io = dynamic_cast<VideoIOBase *>(allobject.GetPointer());
    if (io)
    {
      possibleVideoIO.emplace_back(io);
    }
    else
    {
      itkGenericExceptionMacro("VideoIO factory did not return a VideoIOBase");
    }
  }

  for (auto & j : possibleVideoIO)
  {

    // Check file readability if reading from file
    if (mode == IOModeEnum::ReadFileMode)
    {
      if (j->CanReadFile(arg))
      {
        return j;
      }
    }

    // Check camera readability if reading from camera
    else if (mode == IOModeEnum::ReadCameraMode)
    {
      int cameraIndex = std::stoi(arg);
      if (j->CanReadCamera(cameraIndex))
      {
        return j;
      }
    }

    // Check file writability if writing
    else if (mode == IOModeEnum::WriteMode)
    {
      if (j->CanWriteFile(arg))
      {
        return j;
      }
    }
  }

  // Didn't find a usable VideoIO
  return nullptr;
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const VideoIOFactoryEnums::IOMode value)
{
  return out << [value] {
    switch (value)
    {
      case VideoIOFactoryEnums::IOMode::ReadFileMode:
        return "itk::VideoIOFactoryEnums::IOMode::ReadFileMode";
      case VideoIOFactoryEnums::IOMode::ReadCameraMode:
        return "itk::VideoIOFactoryEnums::IOMode::ReadCameraMode";
      case VideoIOFactoryEnums::IOMode::WriteMode:
        return "itk::VideoIOFactoryEnums::IOMode::WriteMode";
      default:
        return "INVALID VALUE FOR itk::VideoIOFactoryEnums::IOMode";
    }
  }();
}
} // end namespace itk
