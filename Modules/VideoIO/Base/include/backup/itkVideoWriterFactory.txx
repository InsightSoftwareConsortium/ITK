/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoWriterFactory.cxx
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

#include "itkVideoWriterFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"


#ifdef ITK_VIDEO_USE_OPENCV
#include "itkOpenCVWriterFactory.h"
#endif

#ifdef ITK_VIDEO_USE_VXL
#include "itkVXLWriterFactory.h"
#endif

namespace itk
{
template <class TImage>
typename VideoWriterBase<TImage>::Pointer
VideoWriterFactory <TImage>::CreateVideoWriter( LIBRARY_USED lib )
{
  RegisterBuiltInFactories();

  typename VideoWriterBase<TImage>::Pointer ptr;
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkVideoWriterBase");
  std::list< LightObject::Pointer >::iterator i;
  int count = 0;

  for ( i = allobjects.begin() ; i != allobjects.end() ; ++i )
    {
    if ( count == lib )
      {
        ptr = dynamic_cast< VideoWriterBase<TImage>* >(i->GetPointer());
        if ( ptr.IsNotNull() )
        {
        return ptr; 
        }
      }
    ++count;
    }
  
  std::cerr << "Error VideoWriter factory did not return an VideoWriterBase "<< std::endl;
  return 0;
}

template <class TImage>
void
VideoWriterFactory <TImage> ::RegisterBuiltInFactories()
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
      ObjectFactoryBase::RegisterFactory( OpenCVWriterFactory<TImage>::New() );
#endif

#ifdef ITK_VIDEO_USE_VXL
      ObjectFactoryBase::RegisterFactory( VXLWriterFactory<TImage>::New() );
#endif
      
      firstTime = false;
      }
    }
}

} // end namespace itk
