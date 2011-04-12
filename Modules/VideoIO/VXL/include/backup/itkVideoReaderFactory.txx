/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoReaderFactory.txx
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

#include "itkVideoReaderFactory.h"
#include "itkOpenCVReaderFactory.h"
#include "itkVXLReaderFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{
template <class TImage>
typename VideoReaderBase<TImage>::Pointer
VideoReaderFactory <TImage>::CreateVideoReader( LIBRARY_USED lib )
{
  RegisterBuiltInFactories();

  typename VideoReaderBase<TImage>::Pointer ptr;
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkVideoReaderBase");
  std::list< LightObject::Pointer >::iterator i;
  int count = 0;

  for ( i = allobjects.begin() ; i != allobjects.end() ; ++i )
    {
    if ( count == lib )
      {
        ptr = dynamic_cast< VideoReaderBase<TImage>* >(i->GetPointer());
        if ( ptr.IsNotNull() )
        {
        return ptr; 
        }
      }
    ++count;
    }
  
  std::cerr << "Error VideoReader factory did not return a VideoReaderBase "<< std::endl;
  return 0;
}

template <class TImage>
void
VideoReaderFactory <TImage> ::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
      ObjectFactoryBase::RegisterFactory( OpenCVReaderFactory<TImage>::New() );
      ObjectFactoryBase::RegisterFactory( VXLReaderFactory<TImage>::New() );
      
      firstTime = false;
      }
    }
}

} // end namespace itk
