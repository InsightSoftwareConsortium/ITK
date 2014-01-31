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
#ifndef __itkMGHImageIO_h
#define __itkMGHImageIO_h

#include "itkMatrix.h"
#include "itkImageIOBase.h"
#include <fstream>
#include "itk_zlib.h"

namespace itk
{
/** \class MGHImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read MGH file format.
 * Originally developed as part of the Slicer software
 * package under grants XXXX
 *
 * \ingroup IOFilters
 * \ingroup MGHIO
 */
class MGHImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef MGHImageIO         Self;
  typedef ImageIOBase        Superclass;
  typedef SmartPointer<Self> Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MGHImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

protected:
  MGHImageIO();
  ~MGHImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void ReadVolumeHeader();

private:
  static const int MRI_UCHAR = 0;
  static const int MRI_INT   = 1;
  static const int MRI_FLOAT = 3;
  static const int MRI_SHORT = 4;
  static const int MRI_TENSOR = 6;
  static const unsigned int FS_DIMENSION_HEADER_SIZE = sizeof(int) * 7;
  static const unsigned int FS_RAS_HEADER_SIZE = (sizeof(float) * 15) + sizeof(short);
  static const unsigned int FS_UNUSED_HEADER_SIZE = 256 - FS_RAS_HEADER_SIZE;
  static const unsigned int FS_WHOLE_HEADER_SIZE =
    FS_RAS_HEADER_SIZE + FS_DIMENSION_HEADER_SIZE + FS_UNUSED_HEADER_SIZE;

  /** check if a filename is for a compressed file */
  bool IsCompressedFilename(const std::string fname);
  /// processes the actual data buffer
  void SwapBytesIfNecessary(void * const buffer, const unsigned long numberOfPixels);

  /// examines the direction cosines and creates encapsulation data
  // void MriDirCos();

  void WriteHeader();

  void WriteData(const void* buffer);

  void PermuteFrameValues(const void* buffer, char* tempmemory);

  unsigned int GetComponentSize() const;

  std::string GetOrientation( itk::Matrix<double> directions );

  bool          m_IsCompressed;
  gzFile        m_GZFile;
  std::ofstream m_Output;

  template <class T> int TWrite(T out);
  template <class T> int TRead(T &out);

  int TWrite(const char *buf,unsigned long count);
  void OpenFile();
  void CloseFile();
};
} // end namespace itk

#endif // __itkMGHImageIO_h
