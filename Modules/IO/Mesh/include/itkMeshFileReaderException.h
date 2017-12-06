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
#ifndef itkMeshFileReaderException_h
#define itkMeshFileReaderException_h
#include "ITKIOMeshExport.h"

#include "itkMacro.h"
#include "itkExceptionObject.h"

namespace itk
{
/** \class MeshFileReaderException
 *
 * \brief Base exception class for IO conflicts.
 * \ingroup ITKIOMesh
 */
class ITKIOMesh_EXPORT MeshFileReaderException:public ExceptionObject
{
public:
  /** Has to have empty throw(). */
  virtual ~MeshFileReaderException() ITK_NOEXCEPT ITK_OVERRIDE;

  /** Run-time information. */
  itkTypeMacro(MeshFileReaderException, ExceptionObject);

  /** Constructor. */
  MeshFileReaderException(const char *file, unsigned int line,
                          const char *message = "Error in IO",
                          const char *loc = "Unknown");

  /** Constructor. */
  MeshFileReaderException(const std::string & file, unsigned int line,
                          const char *message = "Error in IO",
                          const char *loc = "Unknown");

};

} // namespace ITK

#endif
