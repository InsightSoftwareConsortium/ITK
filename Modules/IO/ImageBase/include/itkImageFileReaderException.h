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
#ifndef itkImageFileReaderException_h
#define itkImageFileReaderException_h
#include "ITKIOImageBaseExport.h"

#include "itkMacro.h"
#include "itkExceptionObject.h"

namespace itk
{
/** \class ImageFileReaderException
 *
 * \brief Base exception class for IO conflicts.
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageFileReaderException:public ExceptionObject
{
public:
  /** Run-time information. */
  itkTypeMacro(ImageFileReaderException, ExceptionObject);

  /** Constructor. */
  ImageFileReaderException(const char *file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Constructor. */
  ImageFileReaderException(const std::string & file, unsigned int line,
                           const char *message = "Error in IO",
                           const char *loc = "Unknown"):
    ExceptionObject(file, line, message, loc)
  {}

  /** Has to have empty throw(). */
  virtual ~ImageFileReaderException() ITK_NOEXCEPT ITK_OVERRIDE;
};
}
#endif // itkImageFileReaderException_h
