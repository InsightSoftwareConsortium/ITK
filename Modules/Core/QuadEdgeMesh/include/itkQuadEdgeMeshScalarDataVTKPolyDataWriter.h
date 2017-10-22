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
#ifndef itkQuadEdgeMeshScalarDataVTKPolyDataWriter_h
#define itkQuadEdgeMeshScalarDataVTKPolyDataWriter_h

#include "itkVTKPolyDataWriter.h"
#include <fstream>

namespace itk
{
/**
 * \class QuadEdgeMeshScalarDataVTKPolyData
 *
 * \brief This class saves a QuadMesh into a VTK-legacy file format,
 *        including its scalar data associated with points.
 *
 * \ingroup Writers
 *
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshScalarDataVTKPolyDataWriter:public VTKPolyDataWriter< TMesh >
{
public:
  typedef QuadEdgeMeshScalarDataVTKPolyDataWriter Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef VTKPolyDataWriter< TMesh >              Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshScalarDataVTKPolyDataWriter, VTKPolyDataWriter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef TMesh                       MeshType;
  typedef typename MeshType::Pointer  MeshPointer;
  typedef typename MeshType::CellType CellType;

  typedef typename MeshType::PointsContainerPointer  PointsContainerPointer;
  typedef typename MeshType::PointsContainerIterator PointsContainerIterator;

  typedef typename MeshType::PointDataContainerPointer      PointDataContainerPointer;
  typedef typename MeshType::PointDataContainerConstPointer PointDataContainerConstPointer;
  typedef typename MeshType::PointDataContainerIterator     PointDataContainerIterator;

  typedef typename MeshType::CellsContainer      CellsContainer;
  typedef typename CellsContainer::Pointer       CellsContainerPointer;
  typedef typename CellsContainer::ConstPointer  CellsContainerConstPointer;
  typedef typename CellsContainer::Iterator      CellsContainerIterator;
  typedef typename CellsContainer::ConstIterator CellsContainerConstIterator;

  typedef typename MeshType::CellDataContainer      CellDataContainer;
  typedef typename CellDataContainer::Iterator      CellDataContainerIterator;
  typedef typename CellDataContainer::ConstIterator CellDataContainerConstIterator;
  typedef typename CellDataContainer::Pointer       CellDataContainerPointer;
  typedef typename CellDataContainer::ConstPointer  CellDataContainerConstPointer;

  /** Set/Get the name of the CellDataName where data are written. */
  itkSetStringMacro(CellDataName);
  itkGetStringMacro(CellDataName);

  /** Set/Get the name of the PointDataName where data are written. */
  itkSetStringMacro(PointDataName);
  itkGetStringMacro(PointDataName);

protected:
  QuadEdgeMeshScalarDataVTKPolyDataWriter();
  ~QuadEdgeMeshScalarDataVTKPolyDataWriter() ITK_OVERRIDE;

  std::string m_CellDataName;
  std::string m_PointDataName;

  void GenerateData() ITK_OVERRIDE;

  void WriteCellData();

  void WritePointData();

private:
  QuadEdgeMeshScalarDataVTKPolyDataWriter(const Self &);
  void operator=(const Self &);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.hxx"
#endif

#endif
