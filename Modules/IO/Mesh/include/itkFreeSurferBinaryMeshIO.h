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

#ifndef itkFreeSurferBinaryMeshIO_h
#define itkFreeSurferBinaryMeshIO_h
#include "ITKIOMeshExport.h"

#include "itkByteSwapper.h"
#include "itkMeshIOBase.h"
#include "itkIntTypes.h"

#include <fstream>

namespace itk
{
/** \class FreeSurferBinaryMeshIO
 * \brief This class defins how to read Freesurfer binary surface file format.
 * To use IO factory, define the suffix as *.fsb.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class ITKIOMesh_EXPORT FreeSurferBinaryMeshIO:public MeshIOBase
{
public:
  /** Standard class typedefs. */
  typedef FreeSurferBinaryMeshIO       Self;
  typedef MeshIOBase                   Superclass;
  typedef SmartPointer< const Self >   ConstPointer;
  typedef SmartPointer< Self >         Pointer;

  typedef Superclass::SizeValueType    SizeValueType;
  typedef Superclass::StreamOffsetType StreamOffsetType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FreeSurferBinaryMeshIO, MeshIOBase);

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
    float *data = new float[this->m_NumberOfPoints * this->m_PointDimension];

    for ( SizeValueType ii = 0; ii < this->m_NumberOfPoints; ii++ )
      {
      for ( unsigned int jj = 0; jj < this->m_PointDimension; jj++ )
        {
        data[ii * this->m_PointDimension + jj] = static_cast< float >( buffer[ii * this->m_PointDimension + jj] );
        }
      }

    itk::ByteSwapper< float >::SwapWriteRangeFromSystemToBigEndian(data, this->m_NumberOfPoints * this->m_PointDimension, &outputFile);
    delete[] data;
  }

  /** Write cells to utput stream */
  template< typename T >
  void WriteCells(T *buffer, std::ofstream & outputFile)
  {
    const itk::uint32_t numberOfCellPoints = 3;

    itk::uint32_t *data = new itk::uint32_t[this->m_NumberOfCells * numberOfCellPoints];

    ReadCellsBuffer(buffer, data);
    itk::ByteSwapper< itk::uint32_t >::SwapWriteRangeFromSystemToBigEndian(data, this->m_NumberOfCells * numberOfCellPoints, &outputFile);

    delete[] data;
  }

  /** Read cells from a data buffer, used when writting mesh */
  template< typename TInput, typename TOutput >
  void ReadCellsBuffer(TInput *input, TOutput *output)
  {
    if ( input && output )
      {
      for ( SizeValueType ii = 0; ii < this->m_NumberOfCells; ii++ )
        {
        for ( unsigned int jj = 0; jj < 3; jj++ )
          {
          /** point identifiers start from the third elements, first element is
            cellType, the second is numberOfPoints. */
          output[ii * 3 + jj] = static_cast< TOutput >( input[5 * ii + jj + 2] );
          }
        }
      }
  }

  /** Write points to output stream */
  template< typename T >
  void WritePointData(T *buffer, std::ofstream & outputFile)
  {
    float *data = new float[this->m_NumberOfPointPixels];

    for ( SizeValueType ii = 0; ii < this->m_NumberOfPointPixels; ii++ )
      {
      data[ii] = static_cast< float >( buffer[ii] );
      }

    itk::ByteSwapper< float >::SwapWriteRangeFromSystemToBigEndian(data, this->m_NumberOfPointPixels, &outputFile);
    delete[] data;
  }

protected:
  FreeSurferBinaryMeshIO();
  virtual ~FreeSurferBinaryMeshIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void OpenFile();

  void CloseFile();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FreeSurferBinaryMeshIO);

  StreamOffsetType m_FilePosition;
  itk::uint32_t    m_FileTypeIdentifier;
  std::ifstream    m_InputFile;
};
} // end namespace itk

#endif
