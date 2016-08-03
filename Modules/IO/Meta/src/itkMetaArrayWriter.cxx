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
#include "itkMetaArrayWriter.h"

namespace itk
{
MetaArrayWriter
::MetaArrayWriter() :
  m_Binary( false ),
  m_Precision( 6 ),
  m_FileName( "" ),
  m_DataFileName( "" ),
  m_Buffer( ITK_NULLPTR )
{
}

MetaArrayWriter
::~MetaArrayWriter()
{}

void MetaArrayWriter
::ConvertTo(MET_ValueEnumType _metaElementType)
{
  if ( m_Buffer != ITK_NULLPTR )
    {
    m_MetaArray.ImportBufferToElementData( m_Buffer,
                                           m_MetaArray.ElementType() );
    }
  m_MetaArray.ConvertElementDataTo(_metaElementType);
}

void MetaArrayWriter
::Update()
{
  m_MetaArray.SetDoublePrecision(m_Precision);

  m_MetaArray.BinaryData(m_Binary);
  if ( m_Buffer != ITK_NULLPTR )
    {
    m_MetaArray.Write(m_FileName.c_str(), m_DataFileName.c_str(),
                      true, m_Buffer);
    }

  m_DataFileName.erase();
}

void
MetaArrayWriter::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "DataFileName: " << m_DataFileName << std::endl;
  os << indent << "Precision: " << m_Precision << std::endl;
  os << indent << "Binary: " << m_Binary << std::endl;

}

} // namespace itk
