/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDirectory.h"

namespace itk
{

/**
 *
 */
Directory::Directory()
{
  m_Internal = new itksys::Directory;
}

/**
 *
 */
Directory::~Directory()
{
  delete m_Internal;
}

/**
 *
 */
void Directory::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Directory for: " << m_Internal->GetPath() << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  unsigned long numFiles = m_Internal->GetNumberOfFiles();
  for ( unsigned long i = 0; i < numFiles; ++i)
    {
    os << indent << m_Internal->GetFile(i) << "\n";
    }
}
/**
 *
 */
bool Directory::Load(const char* dir)
{
  return m_Internal->Load(dir);
}

/**
 *
 */
std::vector<std::string>::size_type Directory::GetNumberOfFiles()
{
  return m_Internal->GetNumberOfFiles();
}

/**
 *
 */
const char* Directory::GetFile(unsigned int index)
{
  return m_Internal->GetFile(index);
}

} // end namespace itk

