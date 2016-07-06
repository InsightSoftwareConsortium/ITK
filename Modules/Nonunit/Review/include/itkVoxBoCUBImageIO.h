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
#ifndef itkVoxBoCUBImageIO_h
#define itkVoxBoCUBImageIO_h


#include <fstream>
#include <string>
#include <map>
#include "itkImageIOBase.h"
#include "itkSpatialOrientation.h"
#include <cstdio>

namespace itk
{
// Forward declaration
class GenericCUBFileAdaptor;

/** \class VoxBoCUBImageIO
 *
 *  \brief Read VoxBoCUBImage file format.
 *
 * A generic reader and writer object for VoxBo files. Basically it
 * provides uniform access to gzip and normal files
 *
 * \author Burstein, Pablo D.; Yushkevich, Paul; Gee, James C.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/303
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKReview
 */
class VoxBoCUBImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef VoxBoCUBImageIO      Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoxBoCUBImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  VoxBoCUBImageIO();
  ~VoxBoCUBImageIO();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoxBoCUBImageIO);

  bool CheckExtension(const char *, bool & isCompressed);

  GenericCUBFileAdaptor * CreateReader(const char *filename);

  GenericCUBFileAdaptor * CreateWriter(const char *filename);

  GenericCUBFileAdaptor *m_Reader, *m_Writer;

  // Initialize the orientation map (from strings to ITK)
  void InitializeOrientationMap();

  // Orientation stuff
  typedef SpatialOrientation::ValidCoordinateOrientationFlags OrientationFlags;
  typedef std::map< std::string, OrientationFlags >           OrientationMap;
  typedef std::map< OrientationFlags, std::string >           InverseOrientationMap;

  OrientationMap        m_OrientationMap;
  InverseOrientationMap m_InverseOrientationMap;

  // Method to swap bytes in read buffer
  void SwapBytesIfNecessary(void *buffer, BufferSizeType numberOfBytes);

  // Strings used in VoxBo files
  static const char *m_VB_IDENTIFIER_SYSTEM;
  static const char *m_VB_IDENTIFIER_FILETYPE;
  static const char *m_VB_DIMENSIONS;
  static const char *m_VB_SPACING;
  static const char *m_VB_ORIGIN;
  static const char *m_VB_DATATYPE;
  static const char *m_VB_BYTEORDER;
  static const char *m_VB_ORIENTATION;
  static const char *m_VB_BYTEORDER_MSB;
  static const char *m_VB_BYTEORDER_LSB;
  static const char *m_VB_DATATYPE_BYTE;
  static const char *m_VB_DATATYPE_INT;
  static const char *m_VB_DATATYPE_FLOAT;
  static const char *m_VB_DATATYPE_DOUBLE;
};
} // end namespace itk

#endif // itkVoxBoCUBImageIO_h
