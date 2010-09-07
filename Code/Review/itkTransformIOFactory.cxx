/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformIOFactory.cxx
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

#include "itkTransformIOFactory.h"
#include "itkTxtTransformIOFactory.h"
#include "itkMatlabTransformIOFactory.h"

#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{
TransformIOBase::Pointer
TransformIOFactory::CreateTransformIO(const char *path, FileModeType mode)
{
  RegisterBuiltInFactories();

  std::list< TransformIOBase::Pointer > possibleTransformIO;
  std::list< LightObject::Pointer >     allobjects =
    ObjectFactoryBase::CreateAllInstance("itkTransformIOBase");
  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {
    TransformIOBase *io = dynamic_cast< TransformIOBase * >( i->GetPointer() );
    if ( io )
      {
      possibleTransformIO.push_back(io);
      }
    else
      {
      std::cerr << "Error TransformIO factory did not return an TransformIOBase: "
                << ( *i )->GetNameOfClass()
                << std::endl;
      }
    }
  for ( std::list< TransformIOBase::Pointer >::iterator k = possibleTransformIO.begin();
        k != possibleTransformIO.end(); ++k )
    {
    if ( mode == ReadMode )
      {
      if ( ( *k )->CanReadFile(path) )
        {
        return *k;
        }
      }
    else if ( mode == WriteMode )
      {
      if ( ( *k )->CanWriteFile(path) )
        {
        return *k;
        }
      }
    }
  return 0;
}

void
TransformIOFactory::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
      //      ObjectFactoryBase::RegisterFactory( GDCMTransformIOFactory::New()
      // );
      ObjectFactoryBase::RegisterFactory( MatlabTransformIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( TxtTransformIOFactory::New() );
      firstTime = false;
      }
    }
}
} // end namespace itk
