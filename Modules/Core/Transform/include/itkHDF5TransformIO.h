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
/** \class HDF5TransformIO
 *  \brief Read&Write transforms in HDF5 Format
 *
 *  See hdfgroup.org/HDF5 -- HDF5 is a physics/astrophysics
 *  format, but it is very general and can store pretty much
 *  any sort of data.
 *
 * \ingroup Transforms
 * \ingroup ITKTransform
 */
class HDF5TransformIO:public TransformIOBase
{
public:
  typedef HDF5TransformIO               Self;
  typedef TransformIOBase               Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef TransformBase                 TransformType;
  typedef Superclass::TransformPointer  TransformPointer;
  typedef Superclass::TransformListType TransformListType;
  typedef TransformType::ParametersType ParametersType;
  /** Run-time type information (and related methods). */
  itkTypeMacro(HDF5TransformIO, TransformIOBase);
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
  HDF5TransformIO();
  virtual ~HDF5TransformIO();
private:
  /** Read a parameter array from the file location name */
  ParametersType ReadParameters(const std::string &DataSetName);

  /** Write a parameter array to the file location name */
  void WriteParameters(const std::string &name,
                       const ParametersType &parameters);

  /** write a string variable */
  void WriteString(const std::string &path, const std::string &value);
  void WriteString(const std::string &path, const char *value);

  H5::H5File *m_H5File;
};
}
#endif // __itkHDF5TransformIO_h
