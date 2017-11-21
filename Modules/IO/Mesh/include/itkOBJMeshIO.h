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

#ifndef itkOBJMeshIO_h
#define itkOBJMeshIO_h
#include "ITKIOMeshExport.h"

#include "itkMeshIOBase.h"
#include "itkNumberToString.h"
#include <fstream>

namespace itk
{
/** \class OBJMeshIO
 * \brief This class defines how to read and write Object file format.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class ITKIOMesh_EXPORT OBJMeshIO:public MeshIOBase
{
public:
  /** Standard class typedefs. */
  typedef OBJMeshIO                  Self;
  typedef MeshIOBase                 Superclass;
  typedef SmartPointer< const Self > ConstPointer;
  typedef SmartPointer< Self >       Pointer;

  typedef Superclass::SizeValueType    SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OBJMeshIO, MeshIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
  * \param FileNameToRead The name of the file to test for reading.
  * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
  * \return Returns true if this MeshIO can read the file specified.
  */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadMeshInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void ReadPoints(void *buffer) ITK_OVERRIDE;

  virtual void ReadCells(void *buffer) ITK_OVERRIDE;

  virtual void ReadPointData(void *buffer) ITK_OVERRIDE;

  virtual void ReadCellData(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this MeshIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteMeshInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void WritePoints(void *buffer) ITK_OVERRIDE;

  virtual void WriteCells(void *buffer) ITK_OVERRIDE;

  virtual void WritePointData(void *buffer) ITK_OVERRIDE;

  virtual void WriteCellData(void *buffer) ITK_OVERRIDE;

  virtual void Write() ITK_OVERRIDE;

protected:
  /** Write points to output stream */
  template< typename T >
  void WritePoints(T *buffer, std::ofstream & outputFile)
  {
    NumberToString<T> convert;
    SizeValueType index = itk::NumericTraits< SizeValueType >::ZeroValue();

    for ( SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++ )
      {
      outputFile << "v ";
      for ( unsigned int jj = 0; jj < this->m_PointDimension; jj++ )
        {
        outputFile << convert(buffer[index++]) << "  ";
        }
      outputFile << '\n';
      }
  }

  template< typename T >
  void WriteCells(T *buffer, std::ofstream & outputFile)
  {
    SizeValueType index = itk::NumericTraits< SizeValueType >::ZeroValue();

    for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
      {
      outputFile << "f ";
      index++;
      unsigned int numberOfCellPoints = static_cast< unsigned int >( buffer[index++] );

      for ( unsigned int jj = 0; jj < numberOfCellPoints; jj++ )
        {
        outputFile << buffer[index++] + 1 << "  ";
        }
      outputFile << '\n';
      }
  }

  /** Write point data to output stream */
  template< typename T >
  void WritePointData(T *buffer, std::ofstream & outputFile)
  {
    NumberToString<T> convert;
    SizeValueType index = itk::NumericTraits< SizeValueType >::ZeroValue();

    for ( SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ii++ )
      {
      outputFile << "vn ";
      for ( unsigned int jj = 0; jj < this->m_PointDimension; jj++ )
        {
        outputFile << convert(buffer[index++]) << "  ";
        }

      outputFile << '\n';
      }
  }

  static bool SplitLine(const std::string& line, std::string& type, std::string& content);

protected:
  OBJMeshIO();
  virtual ~OBJMeshIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void OpenFile();

  void CloseFile();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OBJMeshIO);

  std::ifstream  m_InputFile;
  std::streampos m_PointsStartPosition;  // file position for points rlative to
                                         // std::ios::beg
};
} // end namespace itk

#endif
