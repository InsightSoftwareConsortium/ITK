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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkObjectFactory.h"

#define ITK_STD_INDENT 2
#define ITK_NUMBER_OF_BLANKS 40

namespace itk
{
static ITK_CONSTEXPR_VAR char blanks[ITK_NUMBER_OF_BLANKS + 1] =
  "                                        ";

/**
 * Instance creation.
 */
Indent *
Indent::New()
{
  return new Self;
}

/**
 * Determine the next indentation level. Keep indenting by two until the
 * max of forty.
 */
Indent
Indent
::GetNextIndent()
{
  int indent = m_Indent + ITK_STD_INDENT;

  if ( indent > ITK_NUMBER_OF_BLANKS )
    {
    indent = ITK_NUMBER_OF_BLANKS;
    }
  return indent;
}

/**
 * Print out the indentation. Basically output a bunch of spaces.
 */
std::ostream &
operator<<(std::ostream & os, const Indent & ind)
{
  os << blanks + ( ITK_NUMBER_OF_BLANKS - ind.m_Indent );
  return os;
}
} // end namespace itk
