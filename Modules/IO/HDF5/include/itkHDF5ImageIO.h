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
/**
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 */

#ifndef itkHDF5ImageIO_h
#define itkHDF5ImageIO_h
#include "ITKIOHDF5Export.h"

#include "itkAutoPointer.h"
#include "itkMetaDataObjectBase.h"
#include "itkMetaDataDictionary.h"

// itk namespace first suppresses
// kwstyle error for the H5 namespace below
namespace itk
{
}
namespace H5
{
class H5File;
class DataSpace;
class DataSet;
}

#include "itkStreamingImageIOBase.h"

namespace itk
{
/** \class HDF5ImageIO
 *
 * \author Kent Williams
 * \brief Class that defines how to read HDF5 file format.
 * HDF5 IMAGE FILE FORMAT - As much information as I can determine from sourceforge.net/projects/HDF5lib
 *
 * \ingroup ITKIOHDF5
 *
 * HDF5 paths for elements in file
 * \li N is dimension of image
 * \li \/ITKVersion                   ITK Library Version string
 * \li \/HDFVersion                   HDF Version String
 * \li \/ITKImage                     Root Image Group
 * \li \/ITKImage\/\<name\>             name is arbitrary, but to parallel Transform I/O
 *                                    keep an image in a subgroup. The idea is to
 *                                    parallel transform file structure.
 * \li \/ITKImage\/\<name\>\/Origin     N-D point double
 * \li \/ITKImage\/\<name\>\/Directions N N-vectors double
 * \li \/ITKImage\/\<name\>\/Spacing    N-vector double
 * \li \/ITKImage\/\<name\>\/Dimensions N-vector ::itk::SizeValueType
 * \li \/ITKImage\/\<name\>\/VoxelType  String representing voxel type.
 *                             This can be inferred from the VoxelData
 *                             type info, but it makes the file more
 *                             user friendly with respect to HDF5 viewers.
 * \li \/ITKImage\/\<name\>\/VoxelData  multi-dim array of voxel data
 *                             in the case of non-scalar voxel type,
 *                             keep voxel components together, to make
 *                             loading possible without
 * \li \/ITKImage\/\<name\>\/MetaData   Group for storing metadata from
 *                             MetaDataDictionary
 * \li \/ITKImage\/\<name\>\/MetaData\/\<item-name\>
 *                             Dataset containing data for item-name
 *                             in the MetaDataDictionary
 * re-arrangement.
 *
 *
 */

class ITKIOHDF5_EXPORT HDF5ImageIO: public StreamingImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef HDF5ImageIO          Self;
  typedef StreamingImageIOBase Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HDF5ImageIO, StreamingImageIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

protected:
  HDF5ImageIO();
  ~HDF5ImageIO() ITK_OVERRIDE;

  virtual SizeType GetHeaderSize(void) const ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HDF5ImageIO);

  void WriteString(const std::string &path,
                   const std::string &value);
  void WriteString(const std::string &path,
                   const char *s);
  std::string ReadString(const std::string &path);

  void WriteScalar(const std::string &path,
                   const bool &value);
  void WriteScalar(const std::string &path,
                   const long &value);
  void WriteScalar(const std::string &path,
                   const unsigned long &value);
  void WriteScalar(const std::string &path,
                   const long long &value);
  void WriteScalar(const std::string &path,
                   const unsigned long long &value);

  template <typename TScalar>
  void WriteScalar(const std::string &path,
                   const TScalar &value);

  template <typename TScalar>
  TScalar ReadScalar(const std::string &DataSetName);

  template <typename TScalar>
  void WriteVector(const std::string &path,
                   const std::vector<TScalar> &vec);

  template <typename TScalar>
  std::vector<TScalar> ReadVector(const std::string &DataSetName);

  void WriteDirections(const std::string &path,
                       const std::vector<std::vector<double> > &dir);

  std::vector<std::vector<double> > ReadDirections(const std::string &path);

  template <typename TType>
    bool WriteMeta(const std::string &name,
                   MetaDataObjectBase *metaObj);
  template <typename TType>
    bool WriteMetaArray(const std::string &name,
                   MetaDataObjectBase *metaObj);
  template <typename TType>
    void StoreMetaData(MetaDataDictionary *metaDict,
                       const std::string &HDFPath,
                       const std::string &name,
                       unsigned long numElements);
  void SetupStreaming(H5::DataSpace *imageSpace,
                      H5::DataSpace *slabSpace);

  void CloseH5File();
  void CloseDataSet();

  H5::H5File  *m_H5File;
  H5::DataSet *m_VoxelDataSet;
  bool         m_ImageInformationWritten;
};
} // end namespace itk

#endif // itkHDF5ImageIO_h
