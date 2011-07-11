/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkTransformFileWriter_cxx
#define __itkTransformFileWriter_cxx

#include "itkTransformFileWriter.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"

namespace itk
{
TransformFileWriter
::TransformFileWriter()
{
  this->m_FileName = "";
  this->m_Precision = 7;
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
  /* Check for a CompositeTransform. This check can be removed
   * once the CompositeTransform IO is moved into this class and
   * TranformFileReader */
  std::string transformName = transform->GetNameOfClass();
  if( transformName.find("CompositeTransform") != std::string::npos )
    {
    itkExceptionMacro("Cannot write a transform of type CompositeTransform. "
                      "Use CompositeTransformWriter instead.");
    }

  m_TransformList.clear();
  m_TransformList.push_back( ConstTransformPointer(transform) );
}

const TransformFileWriter::TransformType * TransformFileWriter::GetInput()
{
  ConstTransformPointer res = *(m_TransformList.begin());
  return res.GetPointer();
}

/** Add a transform to be written */
void TransformFileWriter::AddTransform(const TransformType *transform)
{
  /* Check for a CompositeTransform. This check can be removed
   * once the CompositeTransform IO is moved into this class and
   * TranformFileReader */
  std::string transformName = transform->GetNameOfClass();
  if( transformName.find("CompositeTransform") != std::string::npos )
    {
    itkExceptionMacro("Cannot write a transform of type CompositeTransform. "
                      "Use CompositeTransformWriter instead.");
    }

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

void TransformFileWriter::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "Precision: " << m_Precision << std::endl;
}

} // namespace itk

#endif
