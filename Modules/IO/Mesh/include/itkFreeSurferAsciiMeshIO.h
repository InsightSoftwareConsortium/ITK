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

#ifndef itkFreeSurferAsciiMeshIO_h
#define itkFreeSurferAsciiMeshIO_h
#include "ITKIOMeshExport.h"

#include "itkMeshIOBase.h"

#include <fstream>

namespace itk
{
/** \class FreeSurferAsciiMeshIO
 * \brief This class defines how to read and write freesurfer ASCII surface format.
 * To use IO factory, define the suffix as *.fsa.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class ITKIOMesh_EXPORT FreeSurferAsciiMeshIO:public MeshIOBase
{
public:
  /** Standard class typedefs. */
  typedef FreeSurferAsciiMeshIO      Self;
  typedef MeshIOBase                 Superclass;
  typedef SmartPointer< const Self > ConstPointer;
  typedef SmartPointer< Self >       Pointer;

  typedef Superclass::SizeValueType    SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FreeSurferAsciiMeshIO, MeshIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
  * \param FileNameToRead The name of the file to test for reading.
  * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
  * \return Returns true if this MeshIO can read the file specified.
  */
  bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  void ReadMeshInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  void ReadPoints(void *buffer) ITK_OVERRIDE;

  void ReadCells(void *buffer) ITK_OVERRIDE;

  void ReadPointData(void *buffer) ITK_OVERRIDE;

  void ReadCellData(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this MeshIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  bool CanWriteFile(const char *FileNameToWrite) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  void WriteMeshInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void WritePoints(void *buffer) ITK_OVERRIDE;

  void WriteCells(void *buffer) ITK_OVERRIDE;

  void WritePointData(void *buffer) ITK_OVERRIDE;

  void WriteCellData(void *buffer) ITK_OVERRIDE;

  void Write() ITK_OVERRIDE;

protected:
  /** Write points to output stream */
  template< typename T >
  void WritePoints(T *buffer, std::ofstream & outputFile, T label = itk::NumericTraits< T >::ZeroValue())
  {
    outputFile.precision(6);
    SizeValueType index = 0;
    for ( SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++ )
      {
      for ( unsigned int jj = 0; jj < this->m_PointDimension; jj++ )
        {
        outputFile << std::fixed << buffer[index++] << "  ";
        }
      outputFile << label << '\n';
      }
  }

  template< typename T >
  void WriteCells(T *buffer, std::ofstream & outputFile, T label = itk::NumericTraits< T >::ZeroValue())
  {
    const unsigned int numberOfCellPoints = 3;
    SizeValueType      index = 0;

    T *data = new T[this->m_NumberOfCells * numberOfCellPoints];

    ReadCellsBuffer(buffer, data);

    for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
      {
      for ( unsigned int jj = 0; jj < numberOfCellPoints; jj++ )
        {
        outputFile << data[index++] << "  ";
        }
      outputFile << label << '\n';
      }
    delete[] data;
  }

  /** Read cells from a data buffer, used when writting cells */
  template< typename TInput, typename TOutput >
  void ReadCellsBuffer(TInput *input, TOutput *output)
    {
    if ( input && output )
      {
      for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
        {
        for ( unsigned int jj = 0; jj < 3; jj++ )
          {
          /** point identifiers start from the third elements, first element is cellType, the second is numberOfPoints */
          output[ii * 3 + jj] = static_cast< TOutput >( input[5 * ii + jj + 2] );
          }
        }
      }
    }

protected:
  FreeSurferAsciiMeshIO();
  ~FreeSurferAsciiMeshIO() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void OpenFile();

  void CloseFile();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FreeSurferAsciiMeshIO);

  std::ifstream m_InputFile;
};
} // end namespace itk

#endif
