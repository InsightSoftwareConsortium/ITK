/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageIOFactory.h"
#include "itkDicomImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkVTKImageIOFactory.h"
#include "itkMutexLock.h"

namespace itk
{
  

  
ImageIOBase::Pointer 
ImageIOFactory::CreateImageIO(const char* path)
{

  RegisterBuiltInFactories();

  std::list<ImageIOBase::Pointer> possibleImageIO;
  std::list<LightObject::Pointer> allobjects = 
                  ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
  for(std::list<LightObject::Pointer>::iterator i = allobjects.begin();
      i != allobjects.end(); ++i)
    {
    ImageIOBase* io = dynamic_cast<ImageIOBase*>(i->GetPointer());
    if(io)
      {
      possibleImageIO.push_back(io);
      }
    else
      {
      std::cerr << "Error ImageIO factory did not return an ImageIOBase: "
                << (*i)->GetNameOfClass() 
                << std::endl;
      }
    }
  for(std::list<ImageIOBase::Pointer>::iterator k = possibleImageIO.begin();
      k != possibleImageIO.end(); ++k)
    { 
    if((*k)->CanReadFile(path))
      {
      return *k;
      }
    }
  return 0;
}



  
void
ImageIOFactory::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  SimpleMutexLock mutex;
  mutex.Lock();

  if( firstTime )
    {
    ObjectFactoryBase::RegisterFactory( DicomImageIOFactory::New() ); 
    ObjectFactoryBase::RegisterFactory( MetaImageIOFactory::New() ); 
    ObjectFactoryBase::RegisterFactory( PNGImageIOFactory::New() ); 
    ObjectFactoryBase::RegisterFactory( VTKImageIOFactory::New() ); 
    firstTime = false;
    }

  mutex.Unlock();

}




} // end namespace itk
