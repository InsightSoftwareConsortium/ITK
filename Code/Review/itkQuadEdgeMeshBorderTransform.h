/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshBorderTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshBorderTransform_h
#define __itkQuadEdgeMeshBorderTransform_h

#include <itkQuadEdgeMesh.h>
#include <itkQuadEdgeMeshToQuadEdgeMeshFilter.h>
#include <itkQuadEdgeMeshBoundaryEdgesMeshFunction.h>

namespace itk
{
/**
 * \class QuadEdgeMeshBorderTransform
 * \brief Transform the mandatoryly unique border of an \ref itkQE::Mesh
 * into either a circle (conformal) or a square (arclenght-wise).
 * 
 * To Write.
 */
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshBorderTransform :
       public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Basic types. */
  typedef QuadEdgeMeshBorderTransform                   Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh,
    TOutputMesh >                                       Superclass;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;

  typedef TInputMesh                                    InputMeshType;
  typedef typename InputMeshType::ConstPointer          InputMeshConstPointer;
  typedef typename InputMeshType::CoordRepType          InputCoordRepType;
  typedef typename InputMeshType::PointType             InputPointType;
  typedef typename InputMeshType::Traits                InputTraits;
  typedef typename InputMeshType::PointIdentifier       InputPointIdentifier;
  typedef typename InputMeshType::QEType                InputQEType;
  typedef typename InputQEType::IteratorGeom            InputIteratorGeom;
  typedef typename InputMeshType::VectorType            InputVectorType;
  typedef typename InputMeshType::EdgeListType          InputEdgeListType;
  typedef typename InputMeshType::EdgeListPointerType   InputEdgeListPointerType;
  typedef typename InputEdgeListType::iterator          InputEdgeListIterator;
  typedef typename InputMeshType::EdgeCellType          InputEdgeCellType;
  typedef typename InputMeshType::PolygonCellType       InputPolygonCellType;
  typedef typename InputMeshType::PointIdList           InputPointIdList;
  typedef typename InputMeshType::PointsContainer       InputPointsContainer;
  typedef typename InputMeshType::PointsContainerConstIterator
                                                        InputPointsContainerConstIterator;
  typedef typename InputMeshType::CellsContainerConstIterator
                                                        InputCellsContainerConstIterator;

  typedef TOutputMesh                                   OutputMeshType;
  typedef typename OutputMeshType::Pointer              OutputMeshPointer;
  typedef typename OutputMeshType::CoordRepType         OutputCoordRepType;
  typedef typename OutputMeshType::PointType            OutputPointType;
  typedef typename OutputMeshType::Traits               OutputTraits;
  typedef typename OutputMeshType::PointIdentifier      OutputPointIdentifier;
  typedef typename OutputMeshType::QEType               OutputQEType;
  typedef typename OutputMeshType::VectorType           OutputVectorType;
  typedef typename OutputMeshType::EdgeListType         OutputEdgeListType;
  typedef typename OutputMeshType::EdgeCellType         OutputEdgeCellType;
  typedef typename OutputMeshType::PolygonCellType      OutputPolygonCellType;
  typedef typename OutputMeshType::PointIdList          OutputPointIdList;
  typedef typename OutputMeshType::PointsContainer      OutputPointsContainer;
  typedef typename OutputMeshType::PointsContainerConstIterator
                                                        OutputPointsContainerConstIterator;
  typedef typename OutputMeshType::CellsContainerConstIterator
                                                        OutputCellsContainerConstIterator;

  itkNewMacro( Self );
  itkTypeMacro( QuadEdgeMeshBorderTransform, QuadEdgeMeshToQuadEdgeMeshFilter );
  itkStaticConstMacro( PointDimension, unsigned int, 
     InputTraits::PointDimension );
  
  typedef std::vector< InputPointType >                             InputVectorPointType;
  typedef std::map< InputPointIdentifier, OutputPointIdentifier >   MapPointIdentifier;
  typedef typename MapPointIdentifier::iterator                     MapPointIdentifierIterator;

  typedef QuadEdgeMeshBoundaryEdgesMeshFunction< InputMeshType >    BoundaryRepresentativeEdgesType;
  typedef typename BoundaryRepresentativeEdgesType::Pointer         BoundaryRepresentativeEdgesPointer;

public:

  enum BorderTransformType 
    {
    SQUARE_BORDER_TRANSFORM = 0,
    DISK_BORDER_TRANSFORM
    };

  itkSetMacro( TransformType, BorderTransformType );
  itkGetConstMacro( TransformType, BorderTransformType );

  itkSetMacro( Radius, InputCoordRepType );
  itkGetConstMacro( Radius, InputCoordRepType );

  void ComputeTransform( );
  MapPointIdentifier GetBoundaryPtMap( );
  InputVectorPointType GetBorder( );

protected:
  QuadEdgeMeshBorderTransform( );
  ~QuadEdgeMeshBorderTransform( ) {};

  BorderTransformType m_TransformType;

  InputCoordRepType         m_Radius;
  InputVectorPointType      m_Border;

  MapPointIdentifier   m_BoundaryPtMap;

  void GenerateData( );
  void ComputeBoundary( );

  InputEdgeListIterator ComputeLongestBorder( );
  InputEdgeListIterator ComputeLargestBorder( );


  void DiskTransform( );
  InputPointType GetMeshBarycentre( );
  InputCoordRepType RadiusMaxSquare( );

  void ArcLengthSquareTransform( );

private:
  /** Not implemented */
  QuadEdgeMeshBorderTransform( const Self& );

  /** Not implemented */
  void operator = ( const Self& );

};
}
#include "itkQuadEdgeMeshBorderTransform.txx"

#endif
