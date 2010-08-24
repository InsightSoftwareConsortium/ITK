/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReaderWithFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTransformFileReaderWithFactory.h"
#include "itkTransformIOBase.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"

namespace itk
{
/** Constructor */
TransformFileReader
::TransformFileReader()
{
  m_FileName = "";
  TransformFactoryBase::RegisterDefaultTransforms();
}

/** Destructor */
TransformFileReader
::~TransformFileReader()
{}

void TransformFileReader
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }
  TransformIOBase::Pointer transformIO =
    TransformIOFactory::CreateTransformIO(m_FileName.c_str(),
                                          TransformIOFactory::ReadMode);
  if ( transformIO.IsNull() )
    {
    itkExceptionMacro("Can't Create IO object for file "
                      << m_FileName);
    }

  transformIO->SetFileName(m_FileName);
  transformIO->Read();
  for ( TransformListType::iterator it =
          transformIO->GetTransformList().begin();
        it != transformIO->GetTransformList().end(); ++it )
    {
    this->m_TransformList.push_back( TransformPointer(*it) );
    }
}
} // namespace itk
