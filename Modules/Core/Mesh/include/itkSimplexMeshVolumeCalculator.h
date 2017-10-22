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
#ifndef itkSimplexMeshVolumeCalculator_h
#define itkSimplexMeshVolumeCalculator_h

#include "itkIntTypes.h"
#include "itkPolygonCell.h"
#include "itkVector.h"
#include "itkSimplexMesh.h"
#include "itkVectorContainer.h"

namespace itk
{
/**  \class SimplexMeshVolumeCalculator
 * \brief
 *
 * Adapted from itkSimplexMeshToTriangleFilter to calculate the volume of
 * a simplex mesh using the barycenters and normals.
 * call Compute() to calculate the volume and GetVolume() to get the
 * value. For an example see itkDeformableSimplexMesh3DFilter.cxx
 * (Thomas Boettger. Division Medical and Biological Informatics,
 *  German Cancer Research Center, Heidelberg.)
 * \author Leila Baghdadi MICe, Hospital for Sick Children, Toronto, Canada.
 *
 * The original implementation has been replaced with an algorithm
 * based on the discrete form of the divergence theorem.  The general
 * assumption here is that the model is of  closed surface.  For more
 * details see the following reference  (Alyassin A.M. et al,
 * "Evaluation of new algorithms for the interactive measurement of
 * surface area and volume", Med Phys 21(6) 1994.).
 * \ingroup ITKMesh
 */

template< typename TInputMesh >
class ITK_TEMPLATE_EXPORT SimplexMeshVolumeCalculator:public Object
{
public:
  /** Standard "Self" typedef. */
  typedef SimplexMeshVolumeCalculator Self;

  /** Standard "Superclass" typedef. */
  typedef Object Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshVolumeCalculator, Object);

  typedef TInputMesh                           InputMeshType;
  typedef typename InputMeshType::Pointer      InputMeshPointer;
  typedef typename InputMeshType::ConstPointer InputMeshConstPointer;

  typedef typename InputMeshType::PointType              InputPointType;
  typedef typename InputMeshType::PixelType              InputPixelType;
  typedef typename InputMeshType::MeshTraits::CellTraits InputCellTraitsType;

  typedef typename InputMeshType::PointsContainer      InputPointsContainer;
  typedef typename InputPointsContainer::ConstPointer  InputPointsContainerPointer;
  typedef typename InputPointsContainer::ConstIterator InputPointsContainerIterator;

  typedef typename InputMeshType::NeighborListType           InputNeighbors;
  typedef typename InputMeshType::NeighborListType::iterator InputNeighborsIterator;

  typedef typename InputMeshType::CellType             SimplexCellType;
  typedef          itk::PolygonCell< SimplexCellType > SimplexPolygonType;

  // stores the center for each simplex mesh cell, key is the point id
  typedef          itk::MapContainer< IdentifierType, InputPointType > PointMapType;
  typedef typename PointMapType::Pointer                               PointMapPointer;

  typedef typename InputPointType::VectorType VectorType;
  typedef CovariantVector<
    typename VectorType::ValueType, 3 >   CovariantVectorType;

  /**
   * \class SimplexCellVisitor
   * This class provides methods for visiting
   * each simplex cell of a simplex mesh
   * It computes the center of each visited cell.
   * \ingroup ITKMesh
   */
  class SimplexCellVisitor
  {
public:
    /**
     * default constructor
     */
    SimplexCellVisitor()
    {
      m_CenterMap = PointMapType::New();
    }
    virtual ~SimplexCellVisitor() {}

    /**
     * \brief visits all polygon cells and compute the cell centers
     */
    void Visit(IdentifierType cellId, SimplexPolygonType *poly)
    {
      typedef typename SimplexPolygonType::PointIdIterator PointIdIterator;
      PointIdIterator it =  poly->PointIdsBegin();
      InputPointType  center, p;
      center.Fill(0);
      p.Fill(0.0);

      while ( it != poly->PointIdsEnd() )
        {
        m_Mesh->GetPoint(*it, &p);
        center += p.GetVectorFromOrigin();
        it++;
        }

      center[0] /= poly->GetNumberOfPoints();
      center[1] /= poly->GetNumberOfPoints();
      center[2] /= poly->GetNumberOfPoints();

      m_CenterMap->InsertElement(cellId, center);
    }

    PointMapPointer GetCenterMap()
    {
      return m_CenterMap;
    }

    void SetMesh(InputMeshPointer mesh)
    {
      m_Mesh = mesh;
    }

protected:
    InputMeshPointer m_Mesh;
    PointMapPointer  m_CenterMap;
  };

  typedef itk::CellInterfaceVisitorImplementation< InputPixelType,
                                                   InputCellTraitsType,
                                                   SimplexPolygonType,
                                                   SimplexCellVisitor >
  SimplexVisitorInterfaceType;

  typedef typename SimplexVisitorInterfaceType::Pointer SimplexVisitorInterfacePointer;
  typedef typename SimplexCellType::MultiVisitor        CellMultiVisitorType;
  typedef typename CellMultiVisitorType::Pointer        CellMultiVisitorPointer;

  /** Set the input mesh. */
  itkSetObjectMacro(SimplexMesh, InputMeshType);

  /** Compute the volume of the entire simplex mesh. */
  void Compute();

  /** Return the computed volume. */
  itkGetConstMacro(Volume, double);

  /** Return the computed area. */
  itkGetConstMacro(Area, double);

protected:
  SimplexMeshVolumeCalculator();
  virtual ~SimplexMeshVolumeCalculator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimplexMeshVolumeCalculator);

  void Initialize();

  void Finalize();

  /** creates dual triangles for all simplex cells */
  void CreateTriangles();

  /** intermediate volume computation */
  void CalculateTriangleVolume(InputPointType p1, InputPointType p2, InputPointType p3);

  /** part of algorithm */
  IdentifierType FindCellId(IdentifierType id1, IdentifierType id2, IdentifierType id3);

  /** attribute stores the result of the simplex cell visitor */
  PointMapPointer m_Centers;

  InputMeshPointer m_SimplexMesh;

  double m_Volume;
  double m_VolumeX;
  double m_VolumeY;
  double m_VolumeZ;
  double m_Area;
  double m_Kx;
  double m_Ky;
  double m_Kz;
  double m_Wxyz;
  double m_Wxy;
  double m_Wxz;
  double m_Wyz;

  IndexValueType   m_Muncx;
  IndexValueType   m_Muncy;
  IndexValueType   m_Muncz;

  SizeValueType m_NumberOfTriangles;
};
} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshVolumeCalculator.hxx"
#endif

#endif /* __SimplexMeshVolumeCalculator_h */
