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
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkVideoIOBase");

  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {

    VideoIOBase* io = dynamic_cast< VideoIOBase* >( i->GetPointer() );
    if (io)
      {
      possibleVideoIO.push_back(io);
      }
    else
      {
      itkSpecializedMessageExceptionMacro( ExceptionObject,
                                           "VideoIO factory did not return "
                                           "a VideoIOBase");
      }
    }

  for ( std::list< VideoIOBase::Pointer >::iterator j = possibleVideoIO.begin();
        j != possibleVideoIO.end(); ++j )
    {

    // Check file readability if reading from file
    if (mode == ReadFileMode)
      {
      if ((*j)->CanReadFile(arg))
        {
        return *j;
        }
      }

    // Check camera readability if reading from camera
    else if (mode == ReadCameraMode)
      {
      int cameraIndex = atoi(arg);
      if ((*j)->CanReadCamera(cameraIndex))
        {
        return *j;
        }
      }

    // Check file writability if writing
    else if (mode == WriteMode)
      {
      if ((*j)->CanWriteFile(arg))
        {
        return *j;
        }
      }

    }

  // Didn't find a usable VideoIO
  return ITK_NULLPTR;

}

} // end namespace itk
