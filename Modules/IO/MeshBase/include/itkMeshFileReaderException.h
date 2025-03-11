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
#ifndef itkMeshFileReaderException_h
#define itkMeshFileReaderException_h
#include "ITKIOMeshBaseExport.h"

#include "itkMacro.h"

namespace itk
{
/**
 * \class MeshFileReaderException
 *
 * \brief Base exception class for IO conflicts.
 * \ingroup ITKIOMeshBase
 */
class ITKIOMeshBase_EXPORT MeshFileReaderException : public ExceptionObject
{
public:
  ITK_DEFAULT_COPY_AND_MOVE(MeshFileReaderException);

  /** Has to have empty throw(). */
  ~MeshFileReaderException() noexcept override;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MeshFileReaderException);

  /** Constructor. */
  MeshFileReaderException(std::string  file,
                          unsigned int line,
                          std::string  message = "Error in IO",
                          std::string  loc = {});
};
} // end namespace itk

#endif
