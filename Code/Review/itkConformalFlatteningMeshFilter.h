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

#include "itkMesh.h"
#include "itkMeshToMeshFilter.h"

// vnl headers
#include <vnl/vnl_math.h>
#include <vnl/vnl_sparse_matrix.h>

namespace itk
{

/** \class ConformalFlatteningMeshFilter
 * \brief
 *
 * ConformalFlatteningMeshFilter applies a conformal mapping from 3D to 2D.
 *
 * \ingroup MeshFilters
 * \sa TransformMeshFilter
 */

template <class TPixelType>
class ITK_EXPORT ConformalFlatteningMeshFilter :
    public MeshToMeshFilter< Mesh<TPixelType, 3>,Mesh<TPixelType, 3> >
{
public:
  /** Standard class typedefs. */
  typedef ConformalFlatteningMeshFilter  Self;
  typedef Mesh< TPixelType, 3 > Mesh3DType;

  typedef Mesh3DType   TInputMesh;
  typedef Mesh3DType   TOutputMesh;
  typedef TInputMesh   InputMeshType;
  typedef TOutputMesh  OutputMeshType;

  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename InputMeshType::Pointer      InputMeshPointer;
  typedef typename OutputMeshType::Pointer     OutputMeshPointer;
  typedef typename InputMeshType::PointType    InputPointType;
  typedef typename OutputMeshType::PointType   OutputPointType;

  /** Type for representing coordinates. */
  //typedef typename TInputMesh::CoordRepType          CoordRepType;
  typedef double CoordRepType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConformalFlatteningMeshFilter,MeshToMeshFilter);

  void SetInput(TInputMesh *input);

  /** Convenient constants obtained from TMeshTraits template parameter. */
  itkStaticConstMacro(InputPointDimension, unsigned int,
     ::itk::GetMeshDimension< TInputMesh >::PointDimension );
  itkStaticConstMacro(OutputPointDimension, unsigned int,
     ::itk::GetMeshDimension< TOutputMesh >::PointDimension );

  typedef typename InputMeshType::PointsContainer    PointsContainer;
  typedef typename InputMeshType::CellsContainer     CellsContainer;
  typedef typename InputMeshType::PointIdentifier    PointIdentifier;
  typedef typename InputMeshType::CellIdentifier     CellIdentifier;
  typedef typename PointsContainer::ConstIterator    PointIterator;
  typedef typename CellsContainer::ConstIterator     CellIterator;
  typedef typename InputMeshType::CellType           CellType;
  typedef typename CellType::PointIdIterator         PointIdIterator;
  typedef typename CellType::CellAutoPointer         CellAutoPointer;

  /** Select the cell that will be used as reference for the flattening.
   * This value must be the identifier of a cell existing in the input Mesh.
   * A point of this cell will be mapped to infinity on the plane, or it
   * will be mapped to the north-pole on the sphere. It is recommended to
   * select a cell whose curvature is relatively flat. */
  void SetPolarCellIdentifier( CellIdentifier cellId );

  /** Define the scale of the mapping. The largest coordinates of the
   * furthest point in the plane is m_MapScale. */
  void SetScale( double );

  /** Define that the input surface will be mapped to a sphere */
  void MapToSphere( void );

  /** Define that the input surface will be mapped to a plane.
   *  This skips the steps of the stereographic projection. */
  void MapToPlane( void );

protected:
  ConformalFlatteningMeshFilter();
  ~ConformalFlatteningMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Generate Requested Data */
  virtual void GenerateData( void );

private:
  //purposely not implemented
  ConformalFlatteningMeshFilter(const ConformalFlatteningMeshFilter&);
  //purposely not implemented
  void operator=(const ConformalFlatteningMeshFilter&);

  typedef vnl_vector< CoordRepType >       VectorCoordType;
  typedef vnl_sparse_matrix<CoordRepType>  SparseMatrixCoordType;

  /** Cell Id  in which the point P, which is used
   * to define the mapping, lies in. */
  unsigned int m_PolarCellIdentifier;

  /** Whether the result is sphere or plane.  */
  bool    m_MapToSphere;

  /** The scale when mapping to the plane.
   *  Determines how far the farthest point goes. */
  double  m_MapScale;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConformalFlatteningMeshFilter.txx"
#endif

#endif
