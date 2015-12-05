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
#ifndef itkHDF5TransformIO_h
#define itkHDF5TransformIO_h
#include "ITKIOTransformHDF5Export.h"

#include "itkTransformIOBase.h"
#include "itkAutoPointer.h"
#include <string>

// Avoids KWStyle error from forward declaration below.
namespace itk
{
}

// Forward declaration of class H5::H5File
namespace H5
{
class H5File;
}

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
  static const std::string transformFixedName;
  static const std::string transformParamsName;
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
template<typename TParametersValueType>
class HDF5TransformIOTemplate:public TransformIOBaseTemplate<TParametersValueType>,
private HDF5CommonPathNames
{
public:
  typedef HDF5TransformIOTemplate                          Self;
  typedef TransformIOBaseTemplate<TParametersValueType>    Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef typename Superclass::TransformType               TransformType;
  typedef typename Superclass::TransformPointer            TransformPointer;
  typedef typename Superclass::TransformListType           TransformListType;
  typedef typename TransformType::ParametersType           ParametersType;
  typedef typename TransformType::ParametersValueType      ParametersValueType;
  typedef typename TransformType::FixedParametersType      FixedParametersType;
  typedef typename TransformType::FixedParametersValueType FixedParametersValueType;

  typedef typename TransformIOBaseTemplate
                      <ParametersValueType>::ConstTransformListType
                                                                ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( HDF5TransformIOTemplate, TransformIOBaseTemplate );
  itkNewMacro( Self );

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void Write() ITK_OVERRIDE;

protected:
  HDF5TransformIOTemplate();
  virtual ~HDF5TransformIOTemplate();

private:
  /** Read a parameter array from the file location name */
  ParametersType ReadParameters(const std::string &DataSetName) const;
  FixedParametersType ReadFixedParameters(const std::string &DataSetName) const;

  /** Write a parameter array to the file location name */
  void WriteParameters(const std::string &name,
                       const ParametersType &parameters);
  void WriteFixedParameters(const std::string &name,
                       const FixedParametersType &parameters);

  /** write a string variable */
  void WriteString(const std::string &path, const std::string &value);
  void WriteString(const std::string &path, const char *value);
  void WriteOneTransform(const int transformIndex,
                         const TransformType *transform);

  AutoPointer<H5::H5File> m_H5File;
};
const std::string ITKIOTransformHDF5_EXPORT GetTransformName(int);

/** This helps to meet backward compatibility */
typedef HDF5TransformIOTemplate<double> HDF5TransformIO;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHDF5TransformIO.hxx"
#endif

#endif // itkHDF5TransformIO_h
