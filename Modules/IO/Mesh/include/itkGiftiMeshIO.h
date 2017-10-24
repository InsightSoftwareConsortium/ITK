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
#ifndef itkGiftiMeshIO_h
#define itkGiftiMeshIO_h
#include "ITKIOMeshExport.h"

#include "itkAutoPointer.h"
#include "itkMapContainer.h"
#include "itkMatrix.h"
#include "itkMeshIOBase.h"
#include "itkRGBAPixel.h"

#include <fstream>
#include <string>

namespace itk
{
/** \class GiftiMeshIO
 * \brief This class defines how to read and write Gifti file format.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */

class ITKIOMesh_EXPORT GiftiMeshIO:public MeshIOBase
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

  void SetDirection(const DirectionType & direction);

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
  GiftiMeshIO();
  virtual ~GiftiMeshIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  template< typename TInput, typename TOutput >
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
  //This proxy class provides a gifti_image pointer interface to the internal implementation
  //of itk::GiftiImageIO, while hiding the gifticlib interface from the external ITK interface.
  class GiftiImageProxy;

  //Note that it is essential that m_GiftiImageHolder is defined before m_GiftiImage, to ensure that
  //m_GiftiImage can directly get a proxy from m_GiftiImageHolder during GiftiImageIO construction.
  const AutoPointer<GiftiImageProxy> m_GiftiImageHolder;

  GiftiImageProxy& m_GiftiImage;

  bool          m_ReadPointData;
  DirectionType m_Direction;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GiftiMeshIO);

};
} // end namespace itk

#endif
