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
#ifndef itkSimplexMeshAdaptTopologyFilter_h
#define itkSimplexMeshAdaptTopologyFilter_h

#include "itkPolygonCell.h"
#include "itkCellInterfaceVisitor.h"

#include "itkSimplexMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"

#include "vxl_version.h"
#include "vnl/vnl_cross.h"

namespace itk
{
/** \class SimplexMeshAdaptTopologyFilter
 *  \brief This filter changes the topology of a 2-simplex mesh
 *
 * Currently only one transformation for inserting new cells into a mesh is implemented.
 * For insertion several criteria are compute, e.g. the curvature in a mesh point. The user
 * can set a threshold value to control how many cells will be manipulated.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT SimplexMeshAdaptTopologyFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SimplexMeshAdaptTopologyFilter);

  /** Standard "Self" type alias. */
  using Self = SimplexMeshAdaptTopologyFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;

  /** Smart pointer type alias support */
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshAdaptTopologyFilter, MeshToMeshFilter);

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using InputPointType = typename InputMeshType::PointType;
  using InputVectorType = typename InputMeshType::VectorType;
  using InputPixelType = typename InputMeshType::PixelType;
  using InputCellTraitsType = typename InputMeshType::MeshTraits::CellTraits;
  using InputCellType = typename InputMeshType::CellType;
  using PointIdentifier = typename InputMeshType::PointIdentifier;
  using CellIdentifier = typename InputMeshType::CellIdentifier;
  using InputCellPointIdIterator = typename InputCellType::PointIdIterator;
  using InputCellAutoPointer = typename InputCellType::CellAutoPointer;
  using CellAutoPointer = typename InputMeshType::CellAutoPointer;
  using InputPolygonType = itk::PolygonCell<InputCellType>;
  using InputPolygonPointIdIterator = typename InputPolygonType::PointIdIterator;
  using CovariantVectorType = CovariantVector<typename InputVectorType::ValueType, 3>;
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputCellType = typename OutputMeshType::CellType;
  using OutputPolygonType = itk::PolygonCell<OutputCellType>;

  using DoubleValueMapType = typename itk::MapContainer<CellIdentifier, double>;
  using DoubleContainerIterator = typename DoubleValueMapType::Iterator;

  /** \class SimplexCellVisitor
   * class for visiting all polygonal cells.
   * The visitor computes the area and curvature
   * of each cell and stores them in the area
   * map.
   * \ingroup ITKMesh
   */
  class SimplexCellVisitor
  {
  public:
    InputMeshPointer                     mesh;
    double                               totalArea;
    double                               totalCurvature;
    double                               minCellSize;
    double                               maxCellSize;
    typename DoubleValueMapType::Pointer areaMap;
    typename DoubleValueMapType::Pointer curvatureMap;

    double minCurvature;
    double maxCurvature;

    SimplexCellVisitor()
    {
      areaMap = DoubleValueMapType::New();
      curvatureMap = DoubleValueMapType::New();
      totalArea = 0;
      totalCurvature = 0;
      minCellSize = NumericTraits<double>::max();
      maxCellSize = 0;
      minCurvature = NumericTraits<double>::max();
      maxCurvature = 0;
    }

    /** \brief visits all polygon cells and computes the area,
     *  NOTE: works for convex polygons only!!!
     */
    void
    Visit(CellIdentifier cellId, InputPolygonType * poly)
    {
      typename InputPolygonType::PointIdIterator it = poly->PointIdsBegin();

      double          meanCurvature = 0;
      PointIdentifier refPoint = *it;
      double          val = mesh->GetMeanCurvature(*it++);
      meanCurvature += std::abs(val);

      PointIdentifier id1 = *it;
      val = mesh->GetMeanCurvature(*it++);
      meanCurvature += std::abs(val);

      PointIdentifier id2;

      double area = 0;

      int cnt = 0;

      while (it != poly->PointIdsEnd())
      {
        id2 = *it;
        area += ComputeArea(refPoint, id1, id2);
        id1 = id2;
        val = mesh->GetMeanCurvature(*it);
        meanCurvature += std::abs(val);
        cnt++;
        it++;
      }

      meanCurvature /= (double)cnt;
      totalArea += area;
      totalCurvature += meanCurvature;

      areaMap->InsertElement(cellId, area);
      curvatureMap->InsertElement(cellId, meanCurvature);

      if (area > maxCellSize)
      {
        maxCellSize = area;
      }
      if (area < minCellSize)
      {
        minCellSize = area;
      }
      if (meanCurvature > maxCurvature)
      {
        maxCurvature = meanCurvature;
      }
      if (meanCurvature < minCurvature)
      {
        minCurvature = meanCurvature;
      }
    }

    double
    ComputeArea(PointIdentifier p1, PointIdentifier p2, PointIdentifier p3)
    {
      InputPointType v1, v2, v3;

      v1.Fill(0);
      v2.Fill(0);
      v3.Fill(0);

      mesh->GetPoint(p1, &v1);
      mesh->GetPoint(p2, &v2);
      mesh->GetPoint(p3, &v3);
      return std::abs(vnl_cross_3d((v2 - v1).GetVnlVector(), (v3 - v1).GetVnlVector()).two_norm() / 2.0);
    }

    typename DoubleValueMapType::Pointer
    GetAreaMap()
    {
      return areaMap;
    }

    typename DoubleValueMapType::Pointer
    GetCurvatureMap()
    {
      return curvatureMap;
    }

    double
    GetTotalMeshArea()
    {
      return totalArea;
    }

    double
    GetTotalMeanCurvature()
    {
      return totalCurvature / (curvatureMap->Size());
    }

    double
    GetMaximumCellSize()
    {
      return maxCellSize;
    }

    double
    GetMinimumCellSize()
    {
      return minCellSize;
    }

    double
    GetMaximumCurvature()
    {
      return maxCurvature;
    }

    double
    GetMinimumCurvature()
    {
      return minCurvature;
    }
  };

  // cell visitor stuff
  using SimplexVisitorInterfaceType =
    itk::CellInterfaceVisitorImplementation<InputPixelType, InputCellTraitsType, InputPolygonType, SimplexCellVisitor>;

  using SimplexVisitorInterfacePointer = typename SimplexVisitorInterfaceType::Pointer;
  using CellMultiVisitorType = typename InputCellType::MultiVisitor;
  using CellMultiVisitorPointer = typename CellMultiVisitorType::Pointer;

  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  itkSetMacro(SelectionMethod, int);
  itkGetConstMacro(SelectionMethod, int);

  itkGetConstMacro(ModifiedCount, int);

protected:
  SimplexMeshAdaptTopologyFilter();
  ~SimplexMeshAdaptTopologyFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /**
   * Initialize this filters containers
   */
  void
  Initialize();

  /**
   * Method computes and evaluates cell properties,
   * like area and curvature and determines whether
   * a cell should be refined or not.
   */
  void
  ComputeCellParameters();

  /** Copy Geometry data from the input mesh to the output mesh. */
  void
  CopyInputMeshToOutputMeshGeometryData();

  /**
   * Update topology neighbor relations for all cells
   * which are were influenced by he insertion of new
   * points.
   */
  void
  ModifyNeighborCells(CellIdentifier id1, CellIdentifier id2, PointIdentifier insertPointId);

  /**
   * Compute the center of a cell
   */
  InputPointType
  ComputeCellCenter(InputCellAutoPointer & simplexCell);

  /**
   * class member storing cell id offset
   */
  CellIdentifier m_IdOffset;

  /**
   * threshold controls the percentage of cells
   * to satisfy the selection criteria
   */
  double m_Threshold{ 0.5 };

  /**
   * different criteria for cell refinement selection
   */
  int m_SelectionMethod{ 0 };

  /**
   * attribute contains the number of cells
   * which were modified during the last Update()
   */
  int m_ModifiedCount{ 0 };

  /**
   * \brief member for accessing the filter result during
   *  creation
   */
  OutputMeshPointer m_Output;

  InputCellAutoPointer m_NewSimplexCellPointer;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSimplexMeshAdaptTopologyFilter.hxx"
#endif

#endif // itkSimplexMeshAdaptTopologyFilter_h
