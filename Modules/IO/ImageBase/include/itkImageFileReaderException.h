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
#ifndef itkImageFileReaderException_h
#define itkImageFileReaderException_h
#include "ITKIOImageBaseExport.h"

#include "itkMacro.h"

namespace itk
{
/** \class ImageFileReaderException
 *
 * \brief Base exception class for IO conflicts.
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageFileReaderException : public ExceptionObject
{
public:
  ITK_DEFAULT_COPY_AND_MOVE(ImageFileReaderException);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ImageFileReaderException);

  /** Constructor. */
  ImageFileReaderException(std::string  file,
                           unsigned int line,
                           std::string  message = "Error in IO",
                           std::string  location = {})
    : ExceptionObject(std::move(file), line, std::move(message), std::move(location))
  {}

  /** Has to have empty throw(). */
  ~ImageFileReaderException() noexcept override;
};
} // namespace itk
#endif // itkImageFileReaderException_h
