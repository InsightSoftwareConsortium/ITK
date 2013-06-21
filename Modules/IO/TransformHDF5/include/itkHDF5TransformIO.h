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
#ifndef __itkHDF5TransformIO_h
#define __itkHDF5TransformIO_h
#include "itkTransformIOBase.h"

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
/** \class HDF5TransformIOTemplate
 *  \brief Read&Write transforms in HDF5 Format
 *
 *  See hdfgroup.org/HDF5 -- HDF5 is a physics/astrophysics
 *  format, but it is very general and can store pretty much
 *  any sort of data.
 *
 * \ingroup ITKIOTransformHDF5
 */
template< class TInternalComputationValueType >
class HDF5TransformIOTemplate:public TransformIOBaseTemplate< TInternalComputationValueType >
{
public:
  typedef HDF5TransformIOTemplate                               Self;
  typedef TransformIOBaseTemplate< TInternalComputationValueType >    Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef typename Superclass::TransformType                    TransformType;
  typedef typename Superclass::TransformPointer                 TransformPointer;
  typedef typename Superclass::TransformListType                TransformListType;
  typedef typename TransformType::ParametersType                ParametersType;

  typedef typename TransformIOBaseTemplate
                      <TInternalComputationValueType>::ConstTransformListType
                                                                ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HDF5TransformIOTemplate, Superclass);
  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void Write();

protected:
  HDF5TransformIOTemplate();
  virtual ~HDF5TransformIOTemplate();

private:
  /** Read a parameter array from the file location name */
  ParametersType ReadParameters(const std::string &DataSetName);

  /** Write a parameter array to the file location name */
  void WriteParameters(const std::string &name,
                       const ParametersType &parameters);

  /** write a string variable */
  void WriteString(const std::string &path, const std::string &value);
  void WriteString(const std::string &path, const char *value);
  void WriteOneTransform(const int transformIndex,
                         const TransformType *transform);

  H5::H5File *m_H5File;
};
//
// HDF uses hierarchical paths to find particular data
// in a file. These strings are used by both reading and
// writing.
extern const std::string transformGroupName;
extern const std::string transformTypeName;
extern const std::string transformFixedName;
extern const std::string transformParamsName;
extern const std::string ItkVersion;
extern const std::string HDFVersion;
extern const std::string OSName;
extern const std::string OSVersion;

extern const std::string  GetTransformName(int);

/** This helps to meet backward compatibility */
typedef HDF5TransformIOTemplate<double> HDF5TransformIO;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHDF5TransformIO.hxx"
#endif

#endif // __itkHDF5TransformIO_h
