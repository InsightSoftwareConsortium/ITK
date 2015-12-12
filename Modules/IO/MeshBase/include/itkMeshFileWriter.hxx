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
#ifndef itkMeshFileWriter_hxx
#define itkMeshFileWriter_hxx

#include "itkCommand.h"
#include "itkDataObject.h"
#include "itkMeshConvertPixelTraits.h"
#include "itkMeshIOFactory.h"
#include "itkMeshFileWriter.h"
#include "itkObjectFactoryBase.h"

#include "vnl/vnl_vector.h"

namespace itk
{
template< typename TInputMesh >
MeshFileWriter< TInputMesh >
::MeshFileWriter()
{
  m_MeshIO = ITK_NULLPTR;
  m_UseCompression = false;
  m_FactorySpecifiedMeshIO = false;
  m_UserSpecifiedMeshIO = false;
  m_FileTypeIsBINARY = false;
}

template< typename TInputMesh >
MeshFileWriter< TInputMesh >
::~MeshFileWriter()
{}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::SetInput(const InputMeshType *input)
{
  this->ProcessObject::SetNthInput( 0, const_cast< TInputMesh * >( input ) );
}

template< typename TInputMesh >
const typename MeshFileWriter< TInputMesh >::InputMeshType *
MeshFileWriter< TInputMesh >
::GetInput(void)
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return static_cast< TInputMesh * >( this->ProcessObject::GetInput(0) );
}

template< typename TInputMesh >
const typename MeshFileWriter< TInputMesh >::InputMeshType *
MeshFileWriter< TInputMesh >
::GetInput(unsigned int idx)
{
  return static_cast< TInputMesh * >( this->ProcessObject::GetInput(idx) );
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::Write()
{
  const InputMeshType *input = this->GetInput();

  itkDebugMacro(<< "Writing an mesh file");

  // Make sure input is available
  if ( input == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "No input to writer!");
    }

  // Make sure that we can write the file given the name
  if ( m_FileName == "" )
    {
    throw MeshFileWriterException(__FILE__, __LINE__, "FileName must be specified", ITK_LOCATION);
    }

  if ( !( m_UserSpecifiedMeshIO && !m_MeshIO.IsNull() ) )
    {
    // Try creating via factory
    if ( m_MeshIO.IsNull() )
      {
      itkDebugMacro(<< "Attempting factory creation of MeshIO for file: " << m_FileName);
      m_MeshIO = MeshIOFactory::CreateMeshIO(m_FileName.c_str(), MeshIOFactory::WriteMode);
      m_FactorySpecifiedMeshIO = true;
      }
    else
      {
      if ( m_FactorySpecifiedMeshIO && !m_MeshIO->CanWriteFile( m_FileName.c_str() ) )
        {
        itkDebugMacro(<< "MeshIO exists but doesn't know how to write file:" << m_FileName);
        itkDebugMacro(<< "Attempting creation of MeshIO with a factory for file:" << m_FileName);
        m_MeshIO = MeshIOFactory::CreateMeshIO(m_FileName.c_str(), MeshIOFactory::WriteMode);
        m_FactorySpecifiedMeshIO = true;
        }
      }
    }

  if ( m_MeshIO.IsNull() )
    {
    MeshFileWriterException e(__FILE__, __LINE__);
    std::ostringstream      msg;
    msg << " Could not create IO object for file " << m_FileName.c_str() << std::endl;
    msg << "  Tried to create one of the following:" << std::endl;
      {
      std::list< LightObject::Pointer > allobjects = ObjectFactoryBase::CreateAllInstance("itkMeshIOBase");
      for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin(); i != allobjects.end(); ++i )
        {
        MeshIOBase *io = dynamic_cast< MeshIOBase * >( i->GetPointer() );
        msg << "    " << io->GetNameOfClass() << std::endl;
        }
      }

    msg << "  You probably failed to set a file suffix, or" << std::endl;
    msg << "    set the suffix to an unsupported type." << std::endl;
    e.SetDescription( msg.str().c_str() );
    e.SetLocation(ITK_LOCATION);
    throw e;
    }

  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputMeshType *nonConstInput = const_cast< InputMeshType * >( input );

  // Update the input.
  // Streaming is not supported at this time.
  nonConstInput->SetRequestedRegionToLargestPossibleRegion();
  nonConstInput->Update();

  if ( m_FileTypeIsBINARY )
    {
    m_MeshIO->SetFileType(MeshIOBase::BINARY);
    }
  else
    {
    m_MeshIO->SetFileType(MeshIOBase::ASCII);
    }

  if ( m_UseCompression )
    {
    m_MeshIO->UseCompressionOn();
    }
  else
    {
    m_MeshIO->UseCompressionOff();
    }

  // Setup the MeshIO
  m_MeshIO->SetFileName( m_FileName.c_str() );

  // Whether write points
  if ( input->GetPoints() && input->GetNumberOfPoints() )
    {
    m_MeshIO->SetUpdatePoints(true);
    m_MeshIO->SetNumberOfPoints( input->GetNumberOfPoints() );
    m_MeshIO->SetPointDimension(TInputMesh::PointDimension);
    m_MeshIO->SetPointComponentType(MeshIOBase::MapComponentType< typename TInputMesh::PointType::ValueType >::CType);
    }

  // Whether write cells
  if ( input->GetCells() && input->GetNumberOfCells() )
    {
    SizeValueType cellsBufferSize = 2 * input->GetNumberOfCells();
    for ( typename TInputMesh::CellsContainerConstIterator ct = input->GetCells()->Begin(); ct != input->GetCells()->End(); ++ct )
      {
      cellsBufferSize += ct->Value()->GetNumberOfPoints();
      }
    m_MeshIO->SetCellBufferSize(cellsBufferSize);
    m_MeshIO->SetUpdateCells(true);
    m_MeshIO->SetNumberOfCells( input->GetNumberOfCells() );
    m_MeshIO->SetCellComponentType(MeshIOBase::MapComponentType< typename TInputMesh::PointIdentifier >::CType);
    }

  // Whether write point data
  if ( input->GetPointData() && input->GetPointData()->Size() )
    {
    m_MeshIO->SetUpdatePointData(true);
    m_MeshIO->SetNumberOfPointPixels( input->GetPointData()->Size() );
    // m_MeshIO->SetNumberOfPointPixelComponents(MeshConvertPixelTraits<typename
    // TInputMesh::PixelType>::GetNumberOfComponents());
    m_MeshIO->SetPixelType(input->GetPointData()->ElementAt(0), true);
    }

  // Whether write cell data
  if ( input->GetCellData() && input->GetCellData()->Size() )
    {
    m_MeshIO->SetUpdateCellData(true);
    m_MeshIO->SetNumberOfCellPixels( input->GetCellData()->Size() );
    // m_MeshIO->SetNumberOfCellPixelComponents(MeshConvertPixelTraits<typename
    // TInputMesh::CellPixelType>::GetNumberOfComponents());
    m_MeshIO->SetPixelType(input->GetCellData()->ElementAt(0), false);
    }

  this->InvokeEvent( StartEvent() );

  // Write mesh information
  m_MeshIO->WriteMeshInformation();

  // write points
  if ( input->GetPoints() && input->GetNumberOfPoints() )
    {
    WritePoints();
    }

  // Write cells
  if ( input->GetCells() && input->GetNumberOfCells() )
    {
    WriteCells();
    }

  // Write point data
  if ( input->GetPointData() && input->GetPointData()->Size() )
    {
    WritePointData();
    }

  // Write cell data
  if ( input->GetCellData() && input->GetCellData()->Size() )
    {
    WriteCellData();
    }

  // Write to disk
  m_MeshIO->Write();

  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  this->ReleaseInputs();
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::WritePoints(void)
{
  const InputMeshType *input = this->GetInput();

  itkDebugMacro(<< "Writing points: " << m_FileName);
  SizeValueType pointsBufferSize = input->GetNumberOfPoints() * TInputMesh::PointDimension;
  typename TInputMesh::PointType::ValueType * buffer = new typename TInputMesh::PointType::ValueType[pointsBufferSize];
  CopyPointsToBuffer(buffer);
  m_MeshIO->WritePoints(buffer);
  delete[] buffer;
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::WriteCells(void)
{
  itkDebugMacro(<< "Writing cells: " << m_FileName);

  SizeValueType cellsBufferSize = m_MeshIO->GetCellBufferSize();
  typename TInputMesh::PointIdentifier * buffer = new typename TInputMesh::PointIdentifier[cellsBufferSize];
  CopyCellsToBuffer(buffer);
  m_MeshIO->WriteCells(buffer);
  delete[] buffer;
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::WritePointData(void)
{
  const InputMeshType *input = this->GetInput();

  itkDebugMacro(<< "Writing point data: " << m_FileName);

  if ( input->GetPointData()->Size() )
    {
    const SizeValueType numberOfComponents = input->GetPointData()->Size()
                                       * MeshConvertPixelTraits< typename TInputMesh::PixelType >::GetNumberOfComponents(
      input->GetPointData()->ElementAt(0) );

    typedef typename itk::NumericTraits< typename TInputMesh::PixelType >::ValueType ValueType;
    ValueType * buffer = new ValueType[numberOfComponents];
    CopyPointDataToBuffer(buffer);
    m_MeshIO->WritePointData(buffer);
    delete[] buffer;
    }
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::WriteCellData(void)
{
  const InputMeshType *input = this->GetInput();

  itkDebugMacro(<< "Writing cell data: " << m_FileName);

  if ( input->GetCellData()->Size() )
    {
    const SizeValueType numberOfComponents = input->GetCellData()->Size()
                                       * MeshConvertPixelTraits< typename TInputMesh::CellPixelType >::GetNumberOfComponents(
      input->GetCellData()->ElementAt(0) );

    typedef typename itk::NumericTraits< typename TInputMesh::CellPixelType >::ValueType ValueType;
    ValueType * buffer = new ValueType[numberOfComponents];
    CopyCellDataToBuffer(buffer);
    m_MeshIO->WriteCellData(buffer);
    delete[] buffer;
    }
}

template< typename TInputMesh >
template< typename Output >
void
MeshFileWriter< TInputMesh >
::CopyPointsToBuffer(Output *data)
{
  const typename InputMeshType::PointsContainer * points = this->GetInput()->GetPoints();

  typename TInputMesh::PointType point;
  SizeValueType index = NumericTraits< SizeValueType >::ZeroValue();
  typename TInputMesh::PointsContainerConstIterator pter = points->Begin();

  while ( pter != points->End() )
    {
    for ( unsigned int jj = 0; jj < TInputMesh::PointDimension; jj++ )
      {
      data[index++] = static_cast< Output >( pter.Value()[jj] );
      }

    ++pter;
    }
}

template< typename TInputMesh >
template< typename Output >
void
MeshFileWriter< TInputMesh >
::CopyCellsToBuffer(Output *data)
{
  // Get input mesh pointer
  const typename InputMeshType::CellsContainer * cells = this->GetInput()->GetCells();

  // Define required variables
  typename TInputMesh::PointIdentifier const  *ptIds;
  typename TInputMesh::CellType * cellPtr;

  // For each cell
  SizeValueType index = NumericTraits< SizeValueType >::ZeroValue();
  typename TInputMesh::CellsContainerConstIterator cter = cells->Begin();
  while ( cter != cells->End() )
    {
    cellPtr = cter.Value();

    // Write the cell type
    switch ( cellPtr->GetType() )
      {
      case InputMeshCellType::VERTEX_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::VERTEX_CELL );
        break;
      case InputMeshCellType::LINE_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::LINE_CELL );
        break;
      case InputMeshCellType::TRIANGLE_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::TRIANGLE_CELL );
        break;
      case InputMeshCellType::QUADRILATERAL_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::QUADRILATERAL_CELL );
        break;
      case InputMeshCellType::POLYGON_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::POLYGON_CELL );
        break;
      case InputMeshCellType::TETRAHEDRON_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::TETRAHEDRON_CELL );
        break;
      case InputMeshCellType::HEXAHEDRON_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::HEXAHEDRON_CELL );
        break;
      case InputMeshCellType::QUADRATIC_EDGE_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::QUADRATIC_EDGE_CELL );
        break;
      case InputMeshCellType::QUADRATIC_TRIANGLE_CELL:
        data[index++] = static_cast< Output >( MeshIOBase::QUADRATIC_TRIANGLE_CELL );
        break;
      default:
        itkExceptionMacro(<< "Unknown mesh cell");
      }

    // The second element is number of points for each cell
    data[index++] = cellPtr->GetNumberOfPoints();

    // Others are point identifiers in the cell
    ptIds = cellPtr->GetPointIds();
    unsigned int numberOfPoints = cellPtr->GetNumberOfPoints();
    for ( unsigned int ii = 0; ii < numberOfPoints; ii++ )
      {
      data[index++] = static_cast< Output >( ptIds[ii] );
      }

    ++cter;
    }
}

template< typename TInputMesh >
template< typename Output >
void
MeshFileWriter< TInputMesh >
::CopyPointDataToBuffer(Output *data)
{
  const typename InputMeshType::PointDataContainer * pointData = this->GetInput()->GetPointData();

  //  typename TInputMesh::PixelType value = NumericTraits< typename
  // TInputMesh::PixelType >::ZeroValue();
  // TODO? NumericTraitsVariableLengthVectorPixel should define ZeroValue()
  // Should define NumericTraitsArrayPixel

  unsigned int numberOfComponents = MeshConvertPixelTraits< typename TInputMesh::PixelType >::GetNumberOfComponents(
    pointData->ElementAt(0) );

  SizeValueType index = 0;
  typename TInputMesh::PointDataContainer::ConstIterator pter = pointData->Begin();
  while ( pter != pointData->End() )
    {
    for ( unsigned int jj = 0; jj < numberOfComponents; jj++ )
      {
      data[index++] = static_cast< Output >
                    ( MeshConvertPixelTraits< typename TInputMesh::PixelType >::GetNthComponent( jj, pter.Value() ) );
      }

    ++pter;
    }
}

template< typename TInputMesh >
template< typename Output >
void
MeshFileWriter< TInputMesh >
::CopyCellDataToBuffer(Output *data)
{
  const typename InputMeshType::CellDataContainer * cellData = this->GetInput()->GetCellData();

  //  typename TInputMesh::CellPixelType value = NumericTraits< typename
  // TInputMesh::CellPixelType >::ZeroValue();
  // TODO? NumericTraitsVariableLengthVectorPixel should define ZeroValue()
  // Should define NumericTraitsArrayPixel

  unsigned int numberOfComponents = MeshConvertPixelTraits< typename TInputMesh::CellPixelType >::GetNumberOfComponents(
    cellData->ElementAt(0) );
  SizeValueType index = 0;
  typename TInputMesh::CellDataContainer::ConstIterator cter = cellData->Begin();
  while ( cter != cellData->End() )
    {
    for ( unsigned int jj = 0; jj < numberOfComponents; jj++ )
      {
      data[index++] = static_cast< Output >
                    ( MeshConvertPixelTraits< typename TInputMesh::CellPixelType >::GetNthComponent( jj, cter.Value() ) );
      }
    ++cter;
    }
}

template< typename TInputMesh >
void
MeshFileWriter< TInputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "File Name: "
     << ( m_FileName.data() ? m_FileName.data() : "(none)" ) << std::endl;

  os << indent << "Mesh IO: ";
  if ( m_MeshIO.IsNull() )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_MeshIO << "\n";
    }

  if ( m_UseCompression )
    {
    os << indent << "Compression: On\n";
    }
  else
    {
    os << indent << "Compression: Off\n";
    }

  if ( m_FactorySpecifiedMeshIO )
    {
    os << indent << "FactorySpecifiedMeshIO: On\n";
    }
  else
    {
    os << indent << "FactorySpecifiedMeshIO: Off\n";
    }
}
} // end namespace itk

#endif
