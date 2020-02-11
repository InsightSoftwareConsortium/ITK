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
#ifndef itkHDF5TransformIO_h
#define itkHDF5TransformIO_h

#include "ITKIOTransformHDF5Export.h"

#include "itkTransformIOBase.h"
#include <memory>
#include <string>

// Avoids KWStyle error from forward declaration below.
namespace itk
{}

// Forward declaration of class H5::H5File
namespace H5
{
class H5File;
class PredType;
} // namespace H5

namespace itk
{


/** \class HDF5CommonPathNames
 * \brief Secondary bass class of HDF5CommonPathNames common between templates
 *
 * This class provides common non-templated code which can be compiled
 * and used by all templated versions of HDF5TransformIOTemplate.
 *
 * This class must be inherited privately, and light-weight adapting
 * of methods is required for virtual methods or non-private methods
 * for the HDF5TransformIOTemplate interface.
 *
 * \ingroup ITKIOTransformHDF5
 *
 */
struct ITKIOTransformHDF5_EXPORT HDF5CommonPathNames
{
  //
  // HDF uses hierarchical paths to find particular data
  // in a file. These strings are used by both reading and
  // writing.
  static const std::string transformGroupName;
  static const std::string transformTypeName;
  static const std::string transformFixedNameMisspelled;
  static const std::string transformParamsNameMisspelled;
  static const std::string transformFixedName;
  static const std::string transformParamsName;
  static const std::string transformFixedNameCorrected;
  static const std::string transformParamsNameCorrected;
  static const std::string ItkVersion;
  static const std::string HDFVersion;
  static const std::string OSName;
  static const std::string OSVersion;
};


/** \class HDF5TransformIOTemplate
 *  \brief Read&Write transforms in HDF5 Format
 *
 *  See hdfgroup.org/HDF5 -- HDF5 is a physics/astrophysics
 *  format, but it is very general and can store pretty much
 *  any sort of data.
 *
 * \ingroup ITKIOTransformHDF5
 */
template <typename TParametersValueType>
class ITK_TEMPLATE_EXPORT HDF5TransformIOTemplate
  : public TransformIOBaseTemplate<TParametersValueType>
  , private HDF5CommonPathNames
{
public:
  using Self = HDF5TransformIOTemplate;
  using Superclass = TransformIOBaseTemplate<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;
  using TransformListType = typename Superclass::TransformListType;
  using ParametersType = typename TransformType::ParametersType;
  using ParametersValueType = typename TransformType::ParametersValueType;
  using FixedParametersType = typename TransformType::FixedParametersType;
  using FixedParametersValueType = typename TransformType::FixedParametersValueType;

  using ConstTransformListType = typename TransformIOBaseTemplate<ParametersValueType>::ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HDF5TransformIOTemplate, TransformIOBaseTemplate);
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

protected:
  HDF5TransformIOTemplate();
  ~HDF5TransformIOTemplate() override;

private:
  /** Read a parameter array from the file location name */
  ParametersType
  ReadParameters(const std::string & DataSetName) const;
  FixedParametersType
  ReadFixedParameters(const std::string & DataSetName) const;

  /** Write a parameter array to the file location name */
  void
  WriteParameters(const std::string & name, const ParametersType & parameters);
  void
  WriteFixedParameters(const std::string & name, const FixedParametersType & parameters);

  /** write a string variable */
  void
  WriteString(const std::string & path, const std::string & value);
  void
  WriteString(const std::string & path, const char * value);
  void
  WriteOneTransform(const int transformIndex, const TransformType * transform);

  std::unique_ptr<H5::H5File> m_H5File;

  /** Utility function for inferring data storage type
   * from class template.
   * @return H5 code PredType
   */
  H5::PredType
  GetH5TypeFromString() const;
};

const std::string ITKIOTransformHDF5_EXPORT
                  GetTransformName(int);

/** This helps to meet backward compatibility */
using HDF5TransformIO = HDF5TransformIOTemplate<double>;

} // end namespace itk

// Note: Explicit instantiation is done in itkHDF5TransformIO.cxx

#endif // itkHDF5TransformIO_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_HDF5TransformIO
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#if defined(ITKIOTransformHDF5_EXPORTS)
//   We are building this library
#  define ITKIOTransformHDF5_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#else
//   We are using this library
#  define ITKIOTransformHDF5_EXPORT_EXPLICIT ITKIOTransformHDF5_EXPORT
#endif
namespace itk
{
ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKIOTransformHDF5_EXPORT_EXPLICIT HDF5TransformIOTemplate<double>;
extern template class ITKIOTransformHDF5_EXPORT_EXPLICIT HDF5TransformIOTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKIOTransformHDF5_EXPORT_EXPLICIT
#endif
