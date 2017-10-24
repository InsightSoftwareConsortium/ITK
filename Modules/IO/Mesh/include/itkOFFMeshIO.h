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

#ifndef itkOFFMeshIO_h
#define itkOFFMeshIO_h
#include "ITKIOMeshExport.h"

#include "itkMeshIOBase.h"

#include <fstream>

namespace itk
{
/** \class OFFMeshIO
 * \brief this class defines how to read and write Object file format.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class ITKIOMesh_EXPORT OFFMeshIO:public MeshIOBase
{
public:
  /** Standard class typedefs. */
  typedef OFFMeshIO                    Self;
  typedef MeshIOBase                   Superclass;
  typedef SmartPointer< const Self >   ConstPointer;
  typedef SmartPointer< Self >         Pointer;

  typedef Superclass::SizeValueType    SizeValueType;
  typedef Superclass::StreamOffsetType StreamOffsetType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OFFMeshIO, MeshIOBase);

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
  /** Read buffer as ascii stream */
  template< typename T >
  void ReadCellsBufferAsAscii(T *buffer, std::ifstream & inputFile)
    {
    SizeValueType index = 0;
    unsigned int numberOfPoints = 0;
    std::string  line;

    for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
      {
      inputFile >> numberOfPoints;
      buffer[index++] = static_cast< T >( numberOfPoints );
      for ( unsigned int jj = 0; jj < numberOfPoints; jj++ )
        {
        inputFile >> buffer[index++];
        }
      std::getline(inputFile, line, '\n');
      }
    }

  /** Read cells from a data buffer, used when writting cells. This function
    write all kind of cells as it is stored in cells container. It is used when
    cells container have only one kind of cells */
  template< typename TInput, typename TOutput >
  void ReadCellsBuffer(TInput *input, TOutput *output)
    {
    if ( input && output )
      {
      SizeValueType indInput  = 0;
      SizeValueType indOutput = 0;
      for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
        {
        indInput++; // ignore the cell type
        unsigned int numberOfPoints = static_cast< unsigned int >( input[indInput++] );
        output[indOutput++] = static_cast< TOutput >( numberOfPoints );
        for ( unsigned int jj = 0; jj < numberOfPoints; jj++ )
          {
          output[indOutput++] = static_cast< TOutput >( input[indInput++] );
          }
        }
      }
    }

  template< typename T >
  void WriteCellsAsAscii(T *buffer, std::ofstream & outputFile)
    {
    SizeValueType index = 0;

    for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
      {
      index++;
      unsigned int numberOfCellPoints = static_cast< unsigned int >( buffer[index++] );
      outputFile << numberOfCellPoints << "  ";

      for ( unsigned int jj = 0; jj < numberOfCellPoints; jj++ )
        {
        outputFile << buffer[index++] << "  ";
        }

      outputFile << '\n';
      }
    }

  template< typename TOutput, typename TInput >
  void WriteCellsAsBinary(TInput *buffer, std::ofstream & outputFile)
    {
    TOutput *data = new TOutput[m_CellBufferSize - this->m_NumberOfCells];

    ReadCellsBuffer(buffer, data);
    WriteBufferAsBinary< TOutput >(data, outputFile, m_CellBufferSize - this->m_NumberOfCells);

    delete[] data;
    }

protected:
  OFFMeshIO();
  virtual ~OFFMeshIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void OpenFile();

  void CloseFile();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OFFMeshIO);

  std::ifstream    m_InputFile;
  StreamOffsetType m_PointsStartPosition; // file position for points rlative to std::ios::beg
  bool             m_TriangleCellType;    // if all cells are trinalge it is true. otherwise, it is false.
};
} // end namespace itk

#endif
