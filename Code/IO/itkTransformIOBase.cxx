/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformIOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTransformIOBase.h"
#include "itkTransformFactoryBase.h"
#include <iostream>
#include <fstream>
#include <string>

namespace itk
{
TransformIOBase::
TransformIOBase()
{
}
TransformIOBase::
~TransformIOBase()
{
}

void
TransformIOBase::
CreateTransform(TransformPointer &ptr,
                  const std::string &ClassName)
{
  // Instantiate the transform
  itkDebugMacro ( "About to call ObjectFactory" );
  LightObject::Pointer i;
  i = ObjectFactoryBase::CreateInstance ( ClassName.c_str() );
  itkDebugMacro ( "After call ObjectFactory");
  ptr = dynamic_cast<TransformBase*> ( i.GetPointer() );
  if ( ptr.IsNull() )
    {
    OStringStream msg;
    msg << "Could not create an instance of " << ClassName << std::endl
        << "The usual cause of this error is not registering the "
        << "transform with TransformFactory" << std::endl;
    msg << "Currently registered Transforms: " << std::endl;
    std::list<std::string> names = TransformFactoryBase::GetFactory()->GetClassOverrideWithNames();
    std::list<std::string>::iterator it;
    for ( it = names.begin(); it != names.end(); it++ )
      {
      msg << "\t\"" << *it << "\"" << std::endl;
      }
    itkExceptionMacro ( << msg.str() );
    }
}

void 
TransformIOBase
::OpenStream(std::ofstream &out, bool binary)
{
#ifdef __sgi
  // Create the file. This is required on some older sgi's
  if (this->m_AppendMode)
    {
    std::ofstream tFile(m_FileName.c_str(),std::ios::out | std::ios::app);
    tFile.close();   
    }
  else
    {
    std::ofstream tFile(m_FileName.c_str(),std::ios::out);
    tFile.close(); 
    }
#endif
  std::ios::openmode mode(std::ios::out);
  if(binary) 
    {
    mode |= std::ios::binary;
    }
  if (this->m_AppendMode)
    {
    mode |= std::ios::app; 
    }
  out.open(m_FileName.c_str(), mode);
}

} // itk
