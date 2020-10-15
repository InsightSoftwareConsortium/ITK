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
#ifndef itkImageIOFactory_h
#define itkImageIOFactory_h
#include "ITKIOImageBaseExport.h"

#include "itkObject.h"
#include "itkImageIOBase.h"
#include "ITKIOImageBaseExport.h"

namespace itk
{
/** \class ImageIOFactory
 * \brief Create instances of ImageIO objects using an object factory.
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageIOFactory : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageIOFactory);

  /** Standard class type aliases. */
  using Self = ImageIOFactory;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOFactory, Object);

  /** Convenient type alias. */
  using ImageIOBasePointer = ::itk::ImageIOBase::Pointer;

  using IOFileModeEnum = itk::IOFileModeEnum;
#if !defined(ITK_LEGACY_REMOVE)
  using FileModeEnum = itk::IOFileModeEnum;
  using FileModeType = IOFileModeEnum;
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr IOFileModeEnum ReadMode = IOFileModeEnum::ReadMode;
  static constexpr IOFileModeEnum WriteMode = IOFileModeEnum::WriteMode;
#endif
  /** Create the appropriate ImageIO depending on the particulars of the file.
   */
  static ImageIOBasePointer
  CreateImageIO(const char * path, IOFileModeEnum mode);

protected:
  ImageIOFactory();
  ~ImageIOFactory() override;
};

} // end namespace itk

#endif
