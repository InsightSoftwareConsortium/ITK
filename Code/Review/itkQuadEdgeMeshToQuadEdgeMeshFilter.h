/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshToQuadEdgeMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshToQuadEdgeMeshFilter_h
#define __itkQuadEdgeMeshToQuadEdgeMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
/** \class QuadEdgeMeshToQuadEdgeMeshFilter
 *  \brief Duplicates the content of a Mesh.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/306
 *
 */
template< typename TInputMesh, typename TOutputMesh >
class QuadEdgeMeshToQuadEdgeMeshFilter
  : public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Basic types. */
  typedef QuadEdgeMeshToQuadEdgeMeshFilter            Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Input types. */
  typedef TInputMesh                              InputMeshType;
  typedef typename InputMeshType::Pointer         InputMeshPointer;
  typedef typename InputMeshType::ConstPointer    InputMeshConstPointer;
  typedef typename InputMeshType::CoordRepType    InputCoordRepType;
  typedef typename InputMeshType::PointType       InputPointType;
  typedef typename InputMeshType::PointIdentifier InputPointIdentifier;
  typedef typename InputMeshType::QEPrimal        InputQEPrimal;
  typedef typename InputMeshType::VectorType      InputVectorType;

  typedef typename InputMeshType::PointDataContainer   InputPointDataContainer;
  typedef typename InputMeshType::CellDataContainer    InputCellDataContainer;

  typedef typename InputPointDataContainer::ConstPointer 
    InputPointDataContainerConstPointer; 
  typedef typename InputMeshType::PointsContainerConstIterator
    InputPointsContainerConstIterator;
  typedef typename InputMeshType::PointsContainerConstPointer
    InputPointsContainerConstPointer;
  typedef typename InputMeshType::CellsContainerConstIterator
    InputCellsContainerConstIterator;
  typedef typename InputMeshType::CellsContainerConstPointer
    InputCellsContainerConstPointer; 

  typedef typename InputMeshType::EdgeCellType    InputEdgeCellType;
  typedef typename InputMeshType::PolygonCellType InputPolygonCellType;
  typedef typename InputMeshType::PointIdList     InputPointIdList;
  typedef typename InputMeshType::CellTraits      InputCellTraits;
  typedef typename InputCellTraits::PointIdInternalIterator
                                                  InputPointsIdInternalIterator;

  typedef typename InputQEPrimal::IteratorGeom    InputQEIterator;

  /** Output types. */
  typedef TOutputMesh                               OutputMeshType;
  typedef typename OutputMeshType::Pointer          OutputMeshPointer;
  typedef typename OutputMeshType::ConstPointer     OutputMeshConstPointer;
  typedef typename OutputMeshType::CoordRepType     OutputCoordRepType;
  typedef typename OutputMeshType::PointType        OutputPointType;
  typedef typename OutputMeshType::PointIdentifier  OutputPointIdentifier;
  typedef typename OutputMeshType::QEPrimal         OutputQEPrimal;
  typedef typename OutputMeshType::VectorType       OutputVectorType;
  typedef typename OutputQEPrimal::IteratorGeom     OutputQEIterator;
  typedef typename OutputMeshType::PointsContainerIterator
      OutputPointsContainerIterator;
  typedef typename OutputMeshType::PointsContainerPointer
      OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerConstPointer
      OutputPointsContainerConstPointer;

  typedef typename OutputMeshType::PointDataContainer  OutputPointDataContainer;
  typedef typename OutputMeshType::CellDataContainer   OutputCellDataContainer;

public:
  itkNewMacro( Self );
  itkTypeMacro( QuadEdgeMeshToQuadEdgeMeshFilter, MeshToMeshFilter );

protected:
  QuadEdgeMeshToQuadEdgeMeshFilter();
  virtual ~QuadEdgeMeshToQuadEdgeMeshFilter() { }

  virtual void CopyInputMeshToOutputMesh();
  virtual void CopyInputMeshToOutputMeshGeometry();
  virtual void CopyInputMeshToOutputMeshPoints();
  virtual void CopyInputMeshToOutputMeshCells();
  virtual void CopyInputMeshToOutputMeshEdgeCells();
  virtual void CopyInputMeshToOutputMeshFieldData();
  virtual void CopyInputMeshToOutputMeshPointData();
  virtual void CopyInputMeshToOutputMeshCellData(); 

private:
  QuadEdgeMeshToQuadEdgeMeshFilter( const Self& ); // Not impl.
  void operator=( const Self& );  // Not impl.
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.txx"
#endif

#endif
