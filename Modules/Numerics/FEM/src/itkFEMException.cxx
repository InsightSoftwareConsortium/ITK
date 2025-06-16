/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

namespace itk::fem
{
FEMException::FEMException(std::string file, unsigned int lineNumber, std::string location)
  : ExceptionObject(std::move(file), lineNumber, "Unhandled exception in FEM class!", std::move(location))
{}

FEMException::~FEMException() noexcept = default;

FEMExceptionIO::FEMExceptionIO(std::string  file,
                               unsigned int lineNumber,
                               std::string  location,
                               std::string  moreDescription)
  : FEMException(std::move(file), lineNumber, std::move(location))
{
  SetDescription("IO error in FEM class: " + moreDescription);
}

FEMExceptionIO::~FEMExceptionIO() noexcept = default;

FEMExceptionWrongClass::FEMExceptionWrongClass(std::string file, unsigned int lineNumber, std::string location)
  : FEMException(std::move(file), lineNumber, std::move(location))
{
  SetDescription("Object was of wrong class!");
}

FEMExceptionWrongClass::~FEMExceptionWrongClass() noexcept = default;

FEMExceptionObjectNotFound::FEMExceptionObjectNotFound(std::string  file,
                                                       unsigned int lineNumber,
                                                       std::string  location,
                                                       std::string  baseClassName,
                                                       int          GN)
  : FEMException(std::move(file), lineNumber, std::move(location))
  , m_baseClassName(baseClassName)
  , m_GlobalNumber(GN)
{
  std::ostringstream buf;
  buf << "Object not found (" << m_baseClassName << ", GlobalNumber=" << m_GlobalNumber << ")!";
  SetDescription(buf.str().c_str());
}

FEMExceptionObjectNotFound::~FEMExceptionObjectNotFound() noexcept = default;

FEMExceptionSolution::FEMExceptionSolution(std::string  file,
                                           unsigned int lineNumber,
                                           std::string  location,
                                           std::string  moreDescription)
  : FEMException(std::move(file), lineNumber, std::move(location))
{
  SetDescription("Error when solving FEM problem: " + moreDescription);
}

FEMExceptionSolution::~FEMExceptionSolution() noexcept = default;

} // namespace itk::fem
