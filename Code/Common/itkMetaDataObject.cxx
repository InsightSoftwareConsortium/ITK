/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaDataObject.h"

//Specializations for printing off known types.

void
itk::MetaDataObject<float>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<double>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<char>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<unsigned char>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<int>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<unsigned int>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<short>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
void
itk::MetaDataObject<unsigned short>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << this->m_MetaDataObjectValue << std::endl;
}
