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
#ifndef itkMeshFileReader_h
#define itkMeshFileReader_h
#include "ITKIOMeshExport.h"

#include "itkMeshFileReaderException.h"
#include "itkMacro.h"
#include "itkHexahedronCell.h"
#include "itkLineCell.h"
#include "itkMeshIOBase.h"
#include "itkMeshSource.h"
#include "itkPolygonCell.h"
#include "itkQuadrilateralCell.h"
#include "itkQuadraticEdgeCell.h"
#include "itkQuadraticTriangleCell.h"
#include "itkTetrahedronCell.h"
#include "itkTriangleCell.h"
#include "itkVertexCell.h"

#include "itkDefaultConvertPixelTraits.h"
#include "itkMeshConvertPixelTraits.h"

namespace itk
{

/** \class MeshFileReader
 * \brief Mesh source that reads mesh data from a single file.
 *
 * This source object is a general filter to read data from
 * a variety of file formats. It works with a MeshIOBase subclass
 * to actually do the reading of the data. Object factory machinery
 * can be used to automatically create the MeshIOBase, or the
 * MeshIOBase can be manually created and set.
 *
 * TOutputMesh is the type expected by the external users of the
 * filter. If data stored in the file is stored in a different format
 * then specified by TOutputMesh, than this filter converts data
 * between the file type and the external expected type.  The
 * ConvertTraits template argument is used to do the conversion.
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class. Normally just setting the FileName with the
 * appropriate suffix is enough to get the reader to instantiate the
 * correct MeshIO and read the file properly. However, some files have
 * no accepted suffix, so you will have to
 * manually create the MeshIO instance of the write type.
 *
 * \sa MeshIOBase
 *
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 *
 * \author Wanlin Zhu. Uviversity of New South Wales, Australia.
 */

template< typename TOutputMesh,
          typename ConvertPointPixelTraits = MeshConvertPixelTraits< typename TOutputMesh::PixelType >,
          class ConvertCellPixelTraits = MeshConvertPixelTraits< typename TOutputMesh::CellPixelType > >
class ITK_TEMPLATE_EXPORT MeshFileReader:public MeshSource< TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef MeshFileReader             Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< const Self > ConstPointer;
  typedef SmartPointer< Self >       Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshFileReader, MeshSource);

  /** Define output mesh types */
  typedef TOutputMesh                              OutputMeshType;
  typedef typename OutputMeshType::CoordRepType    OutputCoordRepType;
  typedef typename OutputMeshType::PixelType       OutputPointPixelType;
  typedef typename OutputMeshType::CellPixelType   OutputCellPixelType;
  typedef typename OutputMeshType::PointType       OutputPointType;
  typedef typename OutputMeshType::PointIdentifier OutputPointIdentifier;
  typedef typename OutputMeshType::CellIdentifier  OutputCellIdentifier;
  typedef typename OutputMeshType::CellAutoPointer OutputCellAutoPointer;
  typedef typename OutputMeshType::CellType        OutputCellType;
  typedef typename MeshIOBase::SizeValueType       SizeValueType;

  typedef VertexCell< OutputCellType >            OutputVertexCellType;
  typedef LineCell< OutputCellType >              OutputLineCellType;
  typedef TriangleCell< OutputCellType >          OutputTriangleCellType;
  typedef PolygonCell< OutputCellType >           OutputPolygonCellType;
  typedef TetrahedronCell< OutputCellType >       OutputTetrahedronCellType;
  typedef HexahedronCell< OutputCellType >        OutputHexahedronCellType;
  typedef QuadrilateralCell< OutputCellType >     OutputQuadrilateralCellType;
  typedef QuadraticEdgeCell< OutputCellType >     OutputQuadraticEdgeCellType;
  typedef QuadraticTriangleCell< OutputCellType > OutputQuadraticTriangleCellType;

  /** Define output point dimension */
  itkStaticConstMacro(OutputPointDimension, unsigned int, OutputMeshType::PointDimension);

  /** Specify the file to read. This is forwarded to the IO instance. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Set/Get the MeshIO helper class. Often this is created via the object
  * factory mechanism that determines whether a particular MeshIO can
  * read a certain file. This method provides a way to get the MeshIO
  * instance that is created. Or you can directly specify the MeshIO
  * to use to read a particular file in case the factory mechanism will
  * not work properly (e.g., unknown or unusual extension). */
  void  SetMeshIO(MeshIOBase *meshIO);
  itkGetModifiableObjectMacro(MeshIO, MeshIOBase);

  /** Prepare the allocation of the output mesh during the first back
   * propagation of the pipeline. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  template< typename T >
  void ReadPoints(T *buffer);

  template< typename T >
  void ReadCells(T *buffer);

  void ReadPointData();

  void ReadCellData();

protected:
  MeshFileReader();
  ~MeshFileReader() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Convert a block of pixels from one type to another. */
  template< typename T >
  void ConvertPointPixelBuffer(void *inputData, T *outputData, size_t numberOfPixels);

  template< typename T >
  void ConvertCellPixelBuffer(void *inputData, T *outputData, size_t numberOfPixels);

  /** Test whether the given filename exist and it is readable, this
   * is intended to be called before attempting to use  MeshIO
   * classes for actually reading the file. If the file doesn't exist
   * or it is not readable, and exception with an approriate message
   * will be thrown. */
  void TestFileExistanceAndReadability();

  /** Does the real work. */
  virtual void GenerateData() ITK_OVERRIDE;

  MeshIOBase::Pointer m_MeshIO;
  bool                m_UserSpecifiedMeshIO; // keep track whether the MeshIO is
                                             // user specified
  std::string m_FileName;                    // The file to be read

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeshFileReader);

  std::string m_ExceptionMessage;
};
} // namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshFileReader.hxx"
#endif

#endif
