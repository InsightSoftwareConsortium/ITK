/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoViewerFactory.cxx
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

#include "itkVideoViewerFactory.h"
#include "itkOpenCVViewerFactory.h"
#include "itkVXLViewerFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{
template <class TImage>
typename VideoViewerBase<TImage>::Pointer
VideoViewerFactory <TImage>::CreateVideoViewer( LIBRARY_USED lib )
{
  RegisterBuiltInFactories();

  typename VideoViewerBase<TImage>::Pointer ptr;
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkVideoViewerBase");
  std::list< LightObject::Pointer >::iterator i;
  int count = 0;

  for ( i = allobjects.begin() ; i != allobjects.end() ; ++i )
    {
    if ( count == lib )
      {
      ptr = dynamic_cast< VideoViewerBase<TImage>* >(i->GetPointer());
        if ( ptr )
        {
        return ptr; 
        }
      }
    ++count;
    }
  
  std::cerr << "Error VideoViewer factory did not return an VideoViewerBase "<< std::endl;
  return 0;
}

template <class TImage>
void
VideoViewerFactory <TImage> ::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
      ObjectFactoryBase::RegisterFactory( OpenCVViewerFactory<TImage>::New() );
      ObjectFactoryBase::RegisterFactory( VXLViewerFactory<TImage>::New() );
      
      firstTime = false;
      }
    }
}

} // end namespace itk
