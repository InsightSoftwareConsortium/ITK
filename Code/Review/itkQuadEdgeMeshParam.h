/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshParam.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshParam_h
#define __itkQuadEdgeMeshParam_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshBorderTransform.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 *  \class QuadEdgeMeshParam
 *
 *  \brief Compute a planar parameterization of the input mesh.
 *  \note Here we have only implemented some parameterizations with fixed
 *        boundary.
 */
template< class TInputMesh, class TOutputMesh, class TSolverTraits >
class QuadEdgeMeshParam :
        public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
   /** Basic types. */
   typedef QuadEdgeMeshParam                         Self;
   typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh,
     TOutputMesh >                                   Superclass;
   typedef SmartPointer< Self >                      Pointer;
   typedef SmartPointer< const Self >                ConstPointer;

   /** Input types. */
   typedef TInputMesh                              InputMeshType;
   typedef typename InputMeshType::Pointer         InputMeshPointer;
   typedef typename InputMeshType::ConstPointer    InputMeshConstPointer;
   typedef typename InputMeshType::CoordRepType    InputCoordRepType;
   typedef typename InputMeshType::PointType       InputPointType;
   typedef typename InputPointType::VectorType     InputPointVectorType;
   typedef typename InputMeshType::PointIdentifier InputPointIdentifier;
   typedef typename InputMeshType::QEType          InputQEType;
   typedef typename InputMeshType::VectorType      InputVectorType;
   typedef typename InputMeshType::EdgeListType    InputEdgeListType;
   typedef typename InputMeshType::PixelType       InputPixelType;
   typedef typename InputMeshType::Traits          InputTraits;

   itkStaticConstMacro( InputVDimension, unsigned int,
       InputMeshType::PointDimension );

   typedef typename InputMeshType::PointsContainer InputPointsContainer;
   typedef typename InputMeshType::PointsContainerConstIterator
      InputPointsContainerConstIterator;

   typedef typename InputMeshType::CellsContainerConstIterator
      InputCellsContainerConstIterator;
   typedef typename InputMeshType::EdgeCellType    InputEdgeCellType;
   typedef typename InputMeshType::PolygonCellType InputPolygonCellType;
   typedef typename InputMeshType::PointIdList     InputPointIdList;

   typedef typename InputQEType::IteratorGeom InputQEIterator;

   typedef std::map< InputPointIdentifier, InputPointIdentifier >
      InputMapPointIdentifier;
   typedef typename InputMapPointIdentifier::iterator
      InputMapPoinIdentifierIterator;

   /** Output types. */
   typedef TOutputMesh                               OutputMeshType;
   typedef typename OutputMeshType::Pointer          OutputMeshPointer;
   typedef typename OutputMeshType::ConstPointer     OutputMeshConstPointer;
   typedef typename OutputMeshType::CoordRepType     OutputCoordRepType;
   typedef typename OutputMeshType::PointType        OutputPointType;
   typedef typename OutputMeshType::PointIdentifier  OutputPointIdentifier;
   typedef typename OutputMeshType::QEType           OutputQEType;
   typedef typename OutputMeshType::VectorType       OutputVectorType;
   typedef typename OutputQEType::IteratorGeom       OutputQEIterator;
   typedef typename OutputMeshType::PointsContainerIterator
     OutputPointsContainerIterator;

   itkStaticConstMacro( OutputVDimension, unsigned int,
     OutputMeshType::PointDimension );

   typedef TSolverTraits                             SolverTraits;
   typedef typename SolverTraits::ValueType          ValueType;
   typedef typename SolverTraits::MatrixType         MatrixType;
   typedef typename SolverTraits::VectorType         VectorType;

   typedef QuadEdgeMeshBorderTransform< 
    InputMeshType, InputMeshType >                   MeshBorderTransform;
   typedef typename MeshBorderTransform::Pointer     MeshBorderTransformPointer;

   typedef MatrixCoefficients< InputMeshType >       CoefficientsComputation;

public:

  void SetCoefficientsMethod( CoefficientsComputation* iMethod )
    { 
    this->m_CoefficientsMethod = iMethod;
    }

  itkNewMacro( Self );
  itkTypeMacro( MeshParam, QuadEdgeMeshToQuadEdgeMeshFilter );

  itkSetObjectMacro( BorderTransform, MeshBorderTransform );
  itkGetObjectMacro( BorderTransform, MeshBorderTransform );

protected:

  QuadEdgeMeshParam( );
  virtual ~QuadEdgeMeshParam( ) {};

  CoefficientsComputation * m_CoefficientsMethod;

  MeshBorderTransformPointer m_BorderTransform;

  // first is the id of the input mesh and second is the corresponding id
  // in m_BoundaryPtMap
  InputMapPointIdentifier m_BoundaryPtMap;

  // first is the id of the input mesh and second is the corresponding id
  // in m_InternalPtList
  InputMapPointIdentifier m_InternalPtMap;

  std::vector< OutputPointType > m_Border;

  void CopyToOutputBorder( );

  /**
   *  \brief From the list of all vertices from the input mesh InputList
   *  and the list of boundary vertices BoundaryList, Store in
   *  m_InternalPtList the list of interior vertices (i.e. vertices in
   *  InputList and not in BoundaryList )
   *
   *  \note I consider ids of points are well chosen (from 0 to
   *        NumberOfPoints)
   */
  void ComputeListOfInteriorVertices( );

  /**
   *  \brief Fill matrix iM and vectors Bx and m_By depending on if one
   *  vertex is on the border or not.
   *  \param[in] iM
   *  \param[in,out] ioBx
   *  \param[in,out] ioBy
   */
  void FillMatrix( MatrixType& iM, VectorType& ioBx, VectorType& ioBy );

  /**
   *  \brief Solve linears systems : \f$ iM \cdot oX = iBx \f$ and
   * \f$ iM \cdot oY = iBy \f$
   *
   *  \param[in] iM
   *  \param[in] iBx
   *  \param[in] iBy
   *  \param[out] oX
   *  \param[out] oY
   */
  void SolveLinearSystems( const MatrixType& iM,
                            const VectorType& iBx,
                            const VectorType& iBy,
                            VectorType& oX,
                            VectorType& oY );

  void GenerateData( );

private:

  QuadEdgeMeshParam( const Self& ); // purposely not implemented
  void operator=( const Self& );  // purposely not implemented

};

} // end namespace itk

#include "itkQuadEdgeMeshParam.txx"

#endif
