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

#include "itkVideoIOFactory.h"


namespace itk
{
VideoIOBase::Pointer VideoIOFactory::CreateVideoIO( IOModeType mode, const char* arg )
{
  std::list< VideoIOBase::Pointer > possibleVideoIO;
  for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkVideoIOBase") )
    {

    auto * io = dynamic_cast< VideoIOBase* >( allobject.GetPointer() );
    if (io)
      {
      possibleVideoIO.emplace_back(io);
      }
    else
      {
      itkSpecializedMessageExceptionMacro( ExceptionObject,
                                           "VideoIO factory did not return "
                                           "a VideoIOBase");
      }
    }

  for (auto & j : possibleVideoIO)
    {

    // Check file readability if reading from file
    if (mode == IOModeType::ReadFileMode)
      {
      if (j->CanReadFile(arg))
        {
        return j;
        }
      }

    // Check camera readability if reading from camera
    else if (mode == IOModeType::ReadCameraMode)
      {
      int cameraIndex = std::stoi(arg);
      if (j->CanReadCamera(cameraIndex))
        {
        return j;
        }
      }

    // Check file writability if writing
    else if (mode == IOModeType::WriteMode)
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
/** Print Enumerations */
std::ostream& operator<<(std::ostream& out, const VideoIOFactory::IOModeType value)
{
    const char* s =0;
    switch(value)
    {
        case VideoIOFactory::IOModeType::ReadFileMode: s = "VideoIOFactory::IOModeType::ReadFileMode"; break;
        case VideoIOFactory::IOModeType::ReadCameraMode: s = "VideoIOFactory::IOModeType::ReadCameraMode"; break;
        case VideoIOFactory::IOModeType::WriteMode: s = "VideoIOFactory::IOModeType::WriteMode"; break;
        default: s = "INVALID VALUE FOR VideoIOFactory::IOModeType";
    }
    return out << s;
}
} // end namespace itk
