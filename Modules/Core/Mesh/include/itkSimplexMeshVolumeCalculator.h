/*=========================================================================
 *
 *  Copyright NumFOCUS
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
 *
 * \sphinx
 * \sphinxexample{Core/Mesh/CalculateAreaAndVolumeOfSimplexMesh,Calculate Area And Volume Of Simplex Mesh}
 * \endsphinx
 */

template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT SimplexMeshVolumeCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SimplexMeshVolumeCalculator);

  /** Standard "Self" type alias. */
  using Self = SimplexMeshVolumeCalculator;

  /** Standard "Superclass" type alias. */
  using Superclass = Object;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshVolumeCalculator, Object);

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using InputMeshConstPointer = typename InputMeshType::ConstPointer;

  using InputPointType = typename InputMeshType::PointType;
  using InputPixelType = typename InputMeshType::PixelType;
  using InputCellTraitsType = typename InputMeshType::MeshTraits::CellTraits;

  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerPointer = typename InputPointsContainer::ConstPointer;
  using InputPointsContainerIterator = typename InputPointsContainer::ConstIterator;

  using InputNeighbors = typename InputMeshType::NeighborListType;
  using InputNeighborsIterator = typename InputMeshType::NeighborListType::iterator;

  using SimplexCellType = typename InputMeshType::CellType;
  using SimplexPolygonType = itk::PolygonCell<SimplexCellType>;

  // stores the center for each simplex mesh cell, key is the point id
  using PointMapType = itk::MapContainer<IdentifierType, InputPointType>;
  using PointMapPointer = typename PointMapType::Pointer;

  using VectorType = typename InputPointType::VectorType;
  using CovariantVectorType = CovariantVector<typename VectorType::ValueType, 3>;

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
    SimplexCellVisitor() { m_CenterMap = PointMapType::New(); }
    virtual ~SimplexCellVisitor() = default;

    /**
     * \brief visits all polygon cells and compute the cell centers
     */
    void
    Visit(IdentifierType cellId, SimplexPolygonType * poly)
    {
      using PointIdIterator = typename SimplexPolygonType::PointIdIterator;
      PointIdIterator it = poly->PointIdsBegin();
      InputPointType  center, p;
      center.Fill(0);
      p.Fill(0.0);

      while (it != poly->PointIdsEnd())
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

    PointMapPointer
    GetCenterMap()
    {
      return m_CenterMap;
    }

    void
    SetMesh(InputMeshPointer mesh)
    {
      m_Mesh = mesh;
    }

  protected:
    InputMeshPointer m_Mesh;
    PointMapPointer  m_CenterMap;
  };

  using SimplexVisitorInterfaceType = itk::
    CellInterfaceVisitorImplementation<InputPixelType, InputCellTraitsType, SimplexPolygonType, SimplexCellVisitor>;

  using SimplexVisitorInterfacePointer = typename SimplexVisitorInterfaceType::Pointer;
  using CellMultiVisitorType = typename SimplexCellType::MultiVisitor;
  using CellMultiVisitorPointer = typename CellMultiVisitorType::Pointer;

  /** Set the input mesh. */
  itkSetObjectMacro(SimplexMesh, InputMeshType);

  /** Compute the volume of the entire simplex mesh. */
  void
  Compute();

  /** Return the computed volume. */
  itkGetConstMacro(Volume, double);

  /** Return the computed area. */
  itkGetConstMacro(Area, double);

protected:
  SimplexMeshVolumeCalculator() = default;
  ~SimplexMeshVolumeCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  Initialize();

  void
  Finalize();

  /** creates dual triangles for all simplex cells */
  void
  CreateTriangles();

  /** intermediate volume computation */
  void
  CalculateTriangleVolume(InputPointType p1, InputPointType p2, InputPointType p3);

  /** part of algorithm */
  IdentifierType
  FindCellId(IdentifierType id1, IdentifierType id2, IdentifierType id3);

  /** attribute stores the result of the simplex cell visitor */
  PointMapPointer m_Centers;

  InputMeshPointer m_SimplexMesh;

  double m_Volume{ 0.0 };
  double m_VolumeX{ 0.0 };
  double m_VolumeY{ 0.0 };
  double m_VolumeZ{ 0.0 };
  double m_Area{ 0.0 };
  double m_Kx{ 0.0 };
  double m_Ky{ 0.0 };
  double m_Kz{ 0.0 };
  double m_Wxyz{ 0.0 };
  double m_Wxy{ 0.0 };
  double m_Wxz{ 0.0 };
  double m_Wyz{ 0.0 };

  IndexValueType m_Muncx{ 0 };
  IndexValueType m_Muncy{ 0 };
  IndexValueType m_Muncz{ 0 };

  SizeValueType m_NumberOfTriangles{ 0 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSimplexMeshVolumeCalculator.hxx"
#endif

#endif /* __SimplexMeshVolumeCalculator_h */
