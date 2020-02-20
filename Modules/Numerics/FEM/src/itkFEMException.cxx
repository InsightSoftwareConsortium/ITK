/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkFEMException.h"

#include <iostream>

namespace itk
{
namespace fem
{
FEMException::FEMException(const char * file, unsigned int lineNumber, std::string location)
  : ExceptionObject(file, lineNumber)
{
  SetDescription("Unhandled exception in FEM class!");
  SetLocation(location);
}

FEMException::~FEMException() noexcept = default;

FEMExceptionIO::FEMExceptionIO(const char * file,
                               unsigned int lineNumber,
                               std::string  location,
                               std::string  moreDescription)
  : FEMException(file, lineNumber)
{
  SetDescription("IO error in FEM class: " + moreDescription);
  SetLocation(location);
}

FEMExceptionIO::~FEMExceptionIO() noexcept = default;

FEMExceptionWrongClass::FEMExceptionWrongClass(const char * file, unsigned int lineNumber, std::string location)
  : FEMException(file, lineNumber, location)
{
  SetDescription("Object was of wrong class!");
}

FEMExceptionWrongClass::~FEMExceptionWrongClass() noexcept = default;

FEMExceptionObjectNotFound::FEMExceptionObjectNotFound(const char * file,
                                                       unsigned int lineNumber,
                                                       std::string  location,
                                                       std::string  baseClassName,
                                                       int          GN)
  : FEMException(file, lineNumber, location)
{
  m_baseClassName = baseClassName;
  m_GlobalNumber = GN;
  std::ostringstream buf;
  buf << "Object not found (" << m_baseClassName << ", GlobalNumber=" << m_GlobalNumber << ")!";
  SetDescription(buf.str().c_str());
}

FEMExceptionObjectNotFound::~FEMExceptionObjectNotFound() noexcept = default;

FEMExceptionSolution::FEMExceptionSolution(const char * file,
                                           unsigned int lineNumber,
                                           std::string  location,
                                           std::string  moreDescription)
  : FEMException(file, lineNumber)
{
  SetDescription("Error when solving FEM problem: " + moreDescription);
  SetLocation(location);
}

FEMExceptionSolution::~FEMExceptionSolution() noexcept = default;

} // end namespace fem
} // end namespace itk
