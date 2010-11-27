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

namespace itk
{
TransformFileWriter
::TransformFileWriter()
{
  m_FileName = "";
  this->m_AppendMode = false;
}

TransformFileWriter
::~TransformFileWriter()
{}

/** Set the writer to append to the specified file */
void TransformFileWriter::SetAppendOn()
{
  this->m_AppendMode = true;
}

/** Set the writer to overwrite the specified file - This is the
 * default mode. */
void TransformFileWriter::SetAppendOff()
{
  this->m_AppendMode = false;
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
  m_TransformList.push_back(transform);
}

/** Add a transform to be written */
void TransformFileWriter::AddTransform(const TransformType *transform)
{
  m_TransformList.push_back(transform);
}

/** Update the writer */
void TransformFileWriter
::Update()
{
  std::list< const TransformType * >::iterator it = m_TransformList.begin();
  vnl_vector< double >                         TempArray;
  std::ofstream                                out;
  if ( this->m_AppendMode )
    {
    out.open(m_FileName.c_str(), std::ios::out | std::ios::app);
    }
  else
    {
    out.open(m_FileName.c_str(), std::ios::out);
    }
  out << "#Insight Transform File V1.0" << std::endl;
  int count = 0;
  while ( it != m_TransformList.end() )
    {
    out << "#Transform " << count << std::endl;
    out << "Transform: " << ( *it )->GetTransformTypeAsString() << std::endl;

    TempArray = ( *it )->GetParameters();
    out << "Parameters: " << TempArray << std::endl;
    TempArray = ( *it )->GetFixedParameters();
    out << "FixedParameters: " << TempArray << std::endl;
    it++;
    count++;
    }
  out.close();
}

void TransformFileWriter::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "Precision: " << m_Precision << std::endl;
}

} // namespace itk

#endif
