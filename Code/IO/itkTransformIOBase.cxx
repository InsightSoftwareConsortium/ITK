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
  this->m_AppendMode = false;
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

void TransformIOBase::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "AppendMode: " << 
    (m_AppendMode ? "true" : "false") << std::endl;
  if(m_ReadTransformList.size() > 0)
    {
    os << indent << "ReadTransformList: " << std::endl;
    for(TransformListType::const_iterator it = m_ReadTransformList.begin();
        it != m_ReadTransformList.end(); it++)
      {
      os << (*it) << std::endl;
      }
    }
  if(m_WriteTransformList.size() > 0)
    {
    os << indent << "WriteTransformList: " << std::endl;
    for(ConstTransformListType::const_iterator it = m_WriteTransformList.begin();
        it != m_WriteTransformList.end(); it++)
      {
      os << (*it) << std::endl;
      }
    }
}

} // itk
