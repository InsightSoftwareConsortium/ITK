/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkVideoIOFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

#ifdef ITK_VIDEO_USE_OPENCV
#include "itkOpenCVVideoIOFactory.h"
#endif

#ifdef ITK_VIDEO_USE_VXL
#include "itkVXLIOFactory.h"
#endif

namespace itk
{
VideoIOBase::Pointer VideoIOFactory::CreateVideoIO( IOModeType mode, const char* arg )
{
  RegisterBuiltInFactories();

  std::list< VideoIOBase::Pointer > possibleVideoIO;
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkVideoIOBase");

  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end() ; ++i )
    {

    VideoIOBase* io = dynamic_cast< VideoIOBase* >( i->GetPointer() );
    if (io)
      {
      possibleVideoIO.push_back(io);
      }
    else
      {
      std::cerr << "Error VideoIO factory did not return a VideoIOBase: "
                << (*i)->GetNameOfClass() << std::endl;
      }
    }

  for ( std::list< VideoIOBase::Pointer >::iterator j = possibleVideoIO.begin();
        j != possibleVideoIO.end() ; ++j )
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
      if ((*j)->CanReadCamera(atoi(arg)))
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
  return 0;

}

void VideoIOFactory::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
#ifdef ITK_VIDEO_USE_OPENCV
      ObjectFactoryBase::RegisterFactory( OpenCVVideoIOFactory::New() );
#endif

#ifdef ITK_VIDEO_USE_VXL
      ObjectFactoryBase::RegisterFactory( VXLVideoIOFactory::New() );
#endif
      
      firstTime = false;
      }
    }
}

} // end namespace itk
