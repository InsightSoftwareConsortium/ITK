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
#ifndef itkRegularSphereMeshSource_h
#define itkRegularSphereMeshSource_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTriangleCell.h"

namespace itk
{
/** \class RegularSphereMeshSource
 * \brief
 * Inputs are the center of the mesh, the scale (radius in each dimension) of the mesh
 * and a resolution parameter, which corresponds to the recursion
 * depth whlie creating a spherical triangle mesh.
 *
 * Don't use recursion depths larger than 5, because mesh generation gets very slow.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
template< typename TOutputMesh >
class ITK_TEMPLATE_EXPORT RegularSphereMeshSource:public MeshSource< TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef RegularSphereMeshSource         Self;
  typedef itk::MeshSource< TOutputMesh >  Superclass;
  typedef itk::SmartPointer< Self >       Pointer;
  typedef itk::SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularSphereMeshSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh                         OutputMeshType;
  typedef typename OutputMeshType::MeshTraits MeshTraits;
  typedef typename OutputMeshType::PointType  PointType;
  typedef typename MeshTraits::PixelType      PixelType;
  typedef typename PointType::VectorType      VectorType;

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer                OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits             CellTraits;
  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer        PointsContainer;

  /** Define the triangular cell types which form the surface  */
  typedef itk::CellInterface< PixelType, CellTraits > CellInterfaceType;
  typedef itk::TriangleCell< CellInterfaceType >      TriCellType;
  typedef typename TriCellType::SelfAutoPointer       TriCellAutoPointer;
  typedef typename TriCellType::CellAutoPointer       CellAutoPointer;

  typedef std::pair< IdentifierType, IdentifierType >         IndexPairType;
  typedef itk::MapContainer< IndexPairType, IdentifierType >  PointMapType;

  /** Set the resolution level to be used for generating cells in the Sphere.
   *  High values of this parameter will produce sphere with more triangles. */
  itkSetMacro(Resolution, unsigned int);
  itkGetConstMacro(Resolution, unsigned int);

  /** Set/Get Coordinates of the Sphere center. */
  itkSetMacro(Center, PointType);
  itkGetConstMacro(Center, PointType);

  /** Set/Get scales of the Sphere. This is a vector of values that can
   * actually be used for generating ellipsoids aligned with the coordinate
   * axis. */
  itkSetMacro(Scale,  VectorType);
  itkGetConstMacro(Scale,  VectorType);

protected:
  RegularSphereMeshSource();
  ~RegularSphereMeshSource() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, itk::Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

  PointType Divide(const PointType & p1, const PointType & p2) const;

  void AddCell(OutputMeshType *mesh, const typename OutputMeshType::PointIdentifier *pointIds, IdentifierType idx);

  /** model center */
  PointType m_Center;

  /** models resolution */
  unsigned int m_Resolution;

  /** model scales */
  VectorType m_Scale;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegularSphereMeshSource);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularSphereMeshSource.hxx"
#endif

#endif //_itkRegularSphereMeshSource_h
