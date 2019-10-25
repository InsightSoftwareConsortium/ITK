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

#ifndef itkVideoIOFactory_h
#define itkVideoIOFactory_h

#include "itkObject.h"
#include "itkVideoIOBase.h"
#include "ITKVideoIOExport.h"

namespace itk
{

/** \class VideoIOFactory
 * \brief Create instances of VideoIO objects using an object factory.
 *
 * This class will create a VideoIO instance that can read/write to/from the
 * desired file or camera. In order for a specific VideoIO type to be
 * considered, it must be registered with the ITK ObjectFactoryBase.
 *
 * \ingroup ITKVideoIO
 */
class ITKVideoIO_EXPORT VideoIOFactory : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VideoIOFactory);

  /** Standard class type aliases. */
  using Self = VideoIOFactory;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \class IOModeType
   * \ingroup ITKVideoIO
   * Mode in which the VideoIO is intended to be used */
  enum class IOModeType : uint8_t
  {
    ReadFileMode,
    ReadCameraMode,
    WriteMode
  };
#if !defined(ITK_LEGACY_REMOVE) || defined(ITK_WRAPPING) /* castxml 'enum class' workaround */
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr IOModeType ReadFileMode = IOModeType::ReadFileMode;
  static constexpr IOModeType ReadCameraMode = IOModeType::ReadCameraMode;
  static constexpr IOModeType WriteMode = IOModeType::WriteMode;
#endif
  /** Runtime type information (and related methods). **/
  itkTypeMacro(VideoIOFactory, Object);

  /** Create the appropriate ImageIO depending on the particulars of the file.
   *  Note: arg can either be a path for reading/writing from/to a file or a
   *        a string containing an integer to use for a cameraID if reading
   *        from a camera
   */
  static VideoIOBase::Pointer
  CreateVideoIO(IOModeType mode, const char * arg);

protected:
  VideoIOFactory();
  ~VideoIOFactory() override;
};

// Define how to print enumeration
extern ITKVideoIO_EXPORT std::ostream &
                         operator<<(std::ostream & out, const VideoIOFactory::IOModeType value);

} // end namespace itk

#endif
