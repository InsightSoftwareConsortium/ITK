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
void Directory::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Directory for: " << m_Internal->GetPath() << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  unsigned long numFiles = m_Internal->GetNumberOfFiles();
  for ( unsigned long i = 0; i < numFiles; ++i )
    {
    os << indent << m_Internal->GetFile(i) << "\n";
    }
}

/**
 *
 */
bool Directory::Load(const char *dir)
{
  return m_Internal->Load(dir);
}

/**
 *
 */
std::vector< std::string >::size_type Directory::GetNumberOfFiles()
{
  return m_Internal->GetNumberOfFiles();
}

/**
 *
 */
const char * Directory::GetFile(unsigned int index)
{
  return m_Internal->GetFile(index);
}
} // end namespace itk
