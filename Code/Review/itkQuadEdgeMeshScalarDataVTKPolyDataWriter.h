/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshScalarDataVTKPolyDataWriter_h
#define __itkQuadEdgeMeshScalarDataVTKPolyDataWriter_h

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
 */
template< class TMesh >
class QuadEdgeMeshScalarDataVTKPolyDataWriter:public VTKPolyDataWriter< TMesh >
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
  ~QuadEdgeMeshScalarDataVTKPolyDataWriter();

  std::string m_CellDataName;
  std::string m_PointDataName;

  void GenerateData();

  void WriteCellData();

  void WritePointData();

private:
  QuadEdgeMeshScalarDataVTKPolyDataWriter(const Self &);
  void operator=(const Self &);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.txx"
#endif

#endif
