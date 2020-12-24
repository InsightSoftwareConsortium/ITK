/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "ITKIOMeshGiftiExport.h"

#include <memory>
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
 * \ingroup ITKIOMeshGifti
 */

class ITKIOMeshGifti_EXPORT GiftiMeshIO : public MeshIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GiftiMeshIO);

  /** Standard class type aliases. */
  using Self = GiftiMeshIO;
  using Superclass = MeshIOBase;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  using SizeValueType = Superclass::SizeValueType;
  using DirectionType = Matrix<double, 4, 4>;
  using RGBAPixelType = RGBAPixel<float>;
  using LabelColorContainer = MapContainer<int, RGBAPixelType>;
  using LabelNameContainer = MapContainer<int, std::string>;
  using LabelColorContainerPointer = LabelColorContainer::Pointer;
  using LabelNameContainerPointer = LabelNameContainer::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GiftiMeshIO, MeshIOBase);

  itkGetConstMacro(ReadPointData, bool);
  itkSetMacro(ReadPointData, bool);
  itkBooleanMacro(ReadPointData);

  void
  SetDirection(const DirectionType & direction);

  itkGetConstReferenceMacro(Direction, DirectionType);

  LabelColorContainerPointer
  GetLabelColorTable();
  LabelNameContainerPointer
  GetLabelNameTable();
  void
  SetLabelColorTable(const LabelColorContainer * colorMap);
  void
  SetLabelNameTable(const LabelNameContainer * labelMap);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can read the file specified.
   */
  bool
  CanReadFile(const char * fileName) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadMeshInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  ReadPoints(void * buffer) override;

  void
  ReadCells(void * buffer) override;

  void
  ReadPointData(void * buffer) override;

  void
  ReadCellData(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this MeshIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  bool
  CanWriteFile(const char * fileName) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteMeshInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  WritePoints(void * buffer) override;

  void
  WriteCells(void * buffer) override;

  void
  WritePointData(void * buffer) override;

  void
  WriteCellData(void * buffer) override;

  void
  Write() override;

protected:
  GiftiMeshIO();
  ~GiftiMeshIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  template <typename TInput, typename TOutput>
  void
  ConvertBuffer(TInput * input, TOutput * output, SizeValueType numberOfElements)
  {
    if (input && output)
    {
      for (SizeValueType ii = 0; ii < numberOfElements; ii++)
      {
        output[ii] = static_cast<TOutput>(input[ii]);
      }
    }
  }

private:
  // This proxy class provides a gifti_image pointer interface to the internal implementation
  // of itk::GiftiImageIO, while hiding the gifticlib interface from the external ITK interface.
  class GiftiImageProxy;

  // Note that it is essential that m_GiftiImageHolder is defined before m_GiftiImage, to ensure that
  // m_GiftiImage can directly get a proxy from m_GiftiImageHolder during GiftiImageIO construction.
  const std::unique_ptr<GiftiImageProxy> m_GiftiImageHolder;

  GiftiImageProxy & m_GiftiImage;

  bool          m_ReadPointData;
  DirectionType m_Direction;

  // Translate (G|N)ifti datatypes to IOComponentEnum
  IOComponentEnum
  GetComponentTypeFromGifti(int datatype);

  // Translate (G|N)ifti datatypes to IOPixelEnum
  IOPixelEnum
  GetPixelTypeFromGifti(int datatype);

  int
  GetNumberOfPixelComponentsFromGifti(int datatype);
};
} // end namespace itk

#endif
