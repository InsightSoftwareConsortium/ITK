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
#ifndef itkDCMTKTransformIO_h
#define itkDCMTKTransformIO_h

#include "IOTransformDCMTKExport.h"

#include "itkTransformIOBase.h"

namespace itk
{

/** \class DCMTKTransformIO
 *
 *  \brief Read transforms in DICOM format.
 *
 *  \todo Detailed description.
 *
 * \ingroup IOTransformDCMTK
 */
template <typename TInternalComputationValueType>
class DCMTKTransformIO : public TransformIOBaseTemplate<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DCMTKTransformIO);

  using Self = DCMTKTransformIO;
  using Superclass = TransformIOBaseTemplate<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;

  using TransformType = typename Superclass::TransformType;
  using TransformListType = typename Superclass::TransformListType;
  using ConstTransformListType = typename Superclass::ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DCMTKTransformIO, TransformIOBaseTemplate);

  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  void
  Write() override;

  /** Set/Get the desired Frame of Reference UID, tag 0020|0052, for the
   * transform to be extracted. */
  itkSetStringMacro(FrameOfReferenceUID);
  itkGetStringMacro(FrameOfReferenceUID);

protected:
  DCMTKTransformIO();
  ~DCMTKTransformIO() override;

private:
  std::string m_FrameOfReferenceUID;
};

} // end namespace itk

// Note: Explicit instantiation is done in itkDCMTKTransformIO.cxx

#endif // itkDCMTKTransformIO_h

#ifndef ITK_TEMPLATE_EXPLICIT_DCMTKTransformIO
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#if defined(IOTransformDCMTK_EXPORTS)
//   We are building this library
#  define IOTransformDCMTK_EXPORT_EXPLICIT
#else
//   We are using this library
#  define IOTransformDCMTK_EXPORT_EXPLICIT IOTransformDCMTK_EXPORT
#endif
namespace itk
{

#if defined(__GNUC__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wattributes"
#endif

extern template class IOTransformDCMTK_EXPORT_EXPLICIT DCMTKTransformIO<double>;
extern template class IOTransformDCMTK_EXPORT_EXPLICIT DCMTKTransformIO<float>;

#if defined(__GNUC__)
#  pragma GCC diagnostic pop
#endif

} // end namespace itk
#undef IOTransformDCMTK_EXPORT_EXPLICIT
#endif
