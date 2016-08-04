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
#include "itkMetaArrayReader.h"

namespace itk
{

MetaArrayReader
::MetaArrayReader() :
  m_FileName( "" ),
  m_Buffer( ITK_NULLPTR )
{
}

MetaArrayReader
::~MetaArrayReader()
{}

void MetaArrayReader
::SetBuffer(void *_buffer)
{
  m_Buffer = _buffer;
}

MetaArray * MetaArrayReader
::GetMetaArrayPointer(void)
{
  return &m_MetaArray;
}

void MetaArrayReader
::Update()
{
  m_MetaArray.Read(m_FileName.c_str(), true, m_Buffer);
}

void
MetaArrayReader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;

}

} // namespace itk
