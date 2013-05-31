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
#ifndef __itkGiftiMeshIO_h
#define __itkGiftiMeshIO_h

#include "itkMapContainer.h"
#include "itkMatrix.h"
#include "itkMeshIOBase.h"
#include "itkRGBAPixel.h"

#include "gifti_io.h"

#include <fstream>
#include <string>

namespace itk
{
/** \class GiftiMeshIO
 * \brief This class defines how to read and write Gifti file format.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class GiftiMeshIO:public MeshIOBase
{
public:
  /** Standard class typedefs. */
  typedef GiftiMeshIO                         Self;
  typedef MeshIOBase                          Superclass;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef SmartPointer< Self >                Pointer;

  typedef Superclass::SizeValueType           SizeValueType;
  typedef Matrix< double, 4, 4 >              DirectionType;
  typedef RGBAPixel<float>                    RGBAPixelType;
  typedef MapContainer<int, RGBAPixelType>    LabelColorContainer;
  typedef MapContainer<int, std::string>      LabelNameContainer;
  typedef LabelColorContainer::Pointer        LabelColorContainerPointer;
  typedef LabelNameContainer::Pointer         LabelNameContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GiftiMeshIO, MeshIOBase);

  itkGetConstMacro(ReadPointData, bool);
  itkSetMacro(ReadPointData, bool);
  itkBooleanMacro(ReadPointData);

  void SetDirection(const DirectionType direction);

  itkGetConstReferenceMacro(Direction, DirectionType);

  LabelColorContainerPointer GetLabelColorTable();
  LabelNameContainerPointer  GetLabelNameTable();
  void SetLabelColorTable(const LabelColorContainer * colorMap);
  void SetLabelNameTable(const LabelNameContainer * labelMap);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
  * \param FileNameToRead The name of the file to test for reading.
  * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
  * \return Returns true if this MeshIO can read the file specified.
  */
  virtual bool CanReadFile(const char *FileNameToRead);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadMeshInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void ReadPoints(void *buffer);

  virtual void ReadCells(void *buffer);

  virtual void ReadPointData(void *buffer);

  virtual void ReadCellData(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this MeshIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteMeshInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void WritePoints(void *buffer);

  virtual void WriteCells(void *buffer);

  virtual void WritePointData(void *buffer);

  virtual void WriteCellData(void *buffer);

  virtual void Write();

protected:
  GiftiMeshIO();
  virtual ~GiftiMeshIO();

  void PrintSelf(std::ostream & os, Indent indent) const;

  template< class TInput, class TOutput >
  void ConvertBuffer(TInput *input, TOutput *output, SizeValueType numberOfElements)
  {
    if ( input && output )
      {
      for ( SizeValueType ii = 0; ii < numberOfElements; ii++ )
        {
        output[ii] = static_cast< TOutput >( input[ii] );
        }
      }
  }

private:
  bool          m_ReadPointData;
  gifti_image * m_GiftiImage;
  DirectionType m_Direction;

private:
  GiftiMeshIO(const Self &);    // purposely not implemented
  void operator=(const Self &); // purposely not implemented

};
} // end namespace itk

#endif
