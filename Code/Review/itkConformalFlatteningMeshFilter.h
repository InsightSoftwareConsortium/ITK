/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkConformalFlatteningMeshFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConformalFlatteningMeshFilter_h
#define __itkConformalFlatteningMeshFilter_h

// ITK headers
#include "itkMeshToMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkConceptChecking.h"
#include "itkMesh.h"

// Standard headers
#include <assert.h>
#include <vector>

// vnl headers
#include <vcl_iostream.h>
#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_conjugate_gradient.h>


namespace itk
{

/** \class ConformalFlatteningMeshFilter
 * \brief This filter maps a Surface in 3D to a plane or to a sphere.
 *
 * FIXME: Add a couple of paragraphs describing the filter. It may well
 *        be the introductory paragraph from the paper, or even the 
 *        abstract.
 *
 * FIXME: Add all the authors in the paper \author John Melonakos et al
 *
 * FIXME: Add reference to the Insight Journal paper
 *
 * FIXME: Add credits to NAMIC funding
 * 
 * \ingroup MeshFilters
 */
template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT ConformalFlatteningMeshFilter : 
  public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef ConformalFlatteningMeshFilter              Self;
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh>   Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConformalFlatteningMeshFilter, MeshToMeshFilter);

  /** Convenient constants obtained from TMeshTraits template parameter. */
  itkStaticConstMacro(InputPointDimension, unsigned int,
     ::itk::GetMeshDimension< TInputMesh >::PointDimension );
  itkStaticConstMacro(OutputPointDimension, unsigned int,
     ::itk::GetMeshDimension< TOutputMesh >::PointDimension );


  ///////////////////////
  typedef typename InputMeshType::PointsContainer           PointsContainer;
  typedef typename InputMeshType::CellsContainer            CellsContainer;
  typedef typename PointsContainer::ConstIterator           PointIterator;
  typedef typename CellsContainer::ConstIterator            CellIterator;  
  typedef typename InputMeshType::CellType                  CellType;
  typedef typename CellType::PointIdIterator                PointIdIterator;
  typedef typename InputMeshType::PointType                 PointType;
  typedef typename InputMeshType::CellIdentifier            CellIdentifier;

  /** Select the cell that will be used as reference for the flattening.  This
   * value must be the identifier of a cell existing in the input Mesh.  A
   * point of this cell will be mapped to infinity on the plane, or it will be
   * mapped to the north-pole on the sphere. It is recommended to select a cell
   * whose curvature is relatively flat. */
  void SetPolarCellIdentifier( CellIdentifier cellId );
  
  /** Define the scale of the mapping. The largest coordinates of the furthest
   * point in the plane is m_MapScale. */
  void SetScale( double );
  
  /** Define that the input surface will be mapped to a sphere */
  void MapToSphere( void );
  
  /** Define that the input surface will be mapped to a plane.
   *  This skips the steps of the stereographic projection. */
  void MapToPlane( void );
  
#ifdef ITK_USE_CONCEPT_CHECKING
/** Begin concept checking */
itkConceptMacro(DimensionShouldBe3,
  (Concept::SameDimension<itkGetStaticConstMacro(InputPointDimension),3>));
//itkConceptMacro(DimensionShouldBe3,
//  (Concept::SameDimension<itkGetStaticConstMacro(OutputPointDimension),3>));
/** End concept checking */
#endif

protected:
  ConformalFlatteningMeshFilter();
  ~ConformalFlatteningMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Generate Requested Data */
  virtual void GenerateData( void );

private:
  ConformalFlatteningMeshFilter( const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void PerformMapping( 
    InputMeshPointer inputMesh, OutputMeshPointer outputMesh);

  typedef vnl_vector< CoordRepType >       VectorCoordType;
  typedef vnl_sparse_matrix<CoordRepType>  SparseMatrixCoordType;

  void StereographicProject( VectorCoordType const& x,
                             VectorCoordType const& y,
                             OutputMeshPointer outputMesh);

  void PrepareLinearSystem(OutputMeshPointer mesh, 
             SparseMatrixCoordType & D,
             VectorCoordType & bR,
             VectorCoordType & bI);  

  VectorCoordType 
  SolveLinearSystem( SparseMatrixCoordType const& A, 
                     VectorCoordType const& b);
                                         
  /** Cell Id  in which the point P, which is used 
   * to define the mapping, lies in. */
  unsigned int m_PolarCellIdentifier;
  
  /** Whether the result is sphere or plane.  */
  bool      m_MapToSphere;
  
  /** The scale when mapping to the plane. 
   *  Determines how far the farthest point goes. */
  double    m_MapScale;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConformalFlatteningMeshFilter.txx"
#endif

#endif
