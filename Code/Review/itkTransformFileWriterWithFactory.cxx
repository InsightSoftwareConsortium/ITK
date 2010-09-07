/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileWriterWithFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileWriter_cxx
#define __itkTransformFileWriter_cxx

#include "itkTransformFileWriterWithFactory.h"
#include "itkTransformIOBase.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"

namespace itk
{
TransformFileWriter
::TransformFileWriter()
{
  m_FileName = "";
  this->m_AppendMode = false;
  TransformFactoryBase::RegisterDefaultTransforms();
}

TransformFileWriter
::~TransformFileWriter()
{}

/** Set the writer to append to the specified file */
void TransformFileWriter::SetAppendOn()
{
  this->SetAppendMode(true);
}

/** Set the writer to overwrite the specified file - This is the
 * default mode. */
void TransformFileWriter::SetAppendOff()
{
  this->SetAppendMode(false);
}

/** Set the writer mode (append/overwrite). */
void TransformFileWriter::SetAppendMode(bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode. */
bool TransformFileWriter::GetAppendMode()
{
  return ( this->m_AppendMode );
}

/** Set the input transform and reinitialize the list of transforms */
void TransformFileWriter::SetInput(const TransformType *transform)
{
  m_TransformList.clear();
  m_TransformList.push_back( ConstTransformPointer(transform) );
}

/** Add a transform to be written */
void TransformFileWriter::AddTransform(const TransformType *transform)
{
  m_TransformList.push_back( ConstTransformPointer(transform) );
}

void TransformFileWriter
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }
  TransformIOBase::Pointer transformIO =
    TransformIOFactory::CreateTransformIO(m_FileName.c_str(),
                                          TransformIOFactory::WriteMode);
  if ( transformIO.IsNull() )
    {
    itkExceptionMacro("Can't Create IO object for file "
                      << m_FileName);
    }
  transformIO->SetAppendMode(this->m_AppendMode);
  transformIO->SetFileName(m_FileName);
  transformIO->SetTransformList(this->m_TransformList);
  transformIO->Write();
}
} // namespace itk

#endif
