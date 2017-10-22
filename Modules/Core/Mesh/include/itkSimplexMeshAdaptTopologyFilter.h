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
#ifndef itkSimplexMeshAdaptTopologyFilter_h
#define itkSimplexMeshAdaptTopologyFilter_h

#include "itkPolygonCell.h"
#include "itkCellInterfaceVisitor.h"

#include "itkSimplexMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"

#include "vxl_version.h"
#if VXL_VERSION_DATE_FULL > 20040406
#include "vnl/vnl_cross.h"
#define itk_cross_3d vnl_cross_3d
#else
#define itk_cross_3d cross_3d
#endif

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
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT SimplexMeshAdaptTopologyFilter:public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef SimplexMeshAdaptTopologyFilter Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self > Pointer;

  /** Smart pointer typedef support */
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshAdaptTopologyFilter, MeshToMeshFilter);

  typedef TInputMesh                                                InputMeshType;
  typedef typename InputMeshType::Pointer                           InputMeshPointer;
  typedef typename InputMeshType::PointType                         InputPointType;
  typedef typename InputMeshType::VectorType                        InputVectorType;
  typedef typename InputMeshType::PixelType                         InputPixelType;
  typedef typename InputMeshType::MeshTraits::CellTraits            InputCellTraitsType;
  typedef typename InputMeshType::CellType                          InputCellType;
  typedef typename InputMeshType::PointIdentifier                   PointIdentifier;
  typedef typename InputMeshType::CellIdentifier                    CellIdentifier;
  typedef typename InputCellType::PointIdIterator                   InputCellPointIdIterator;
  typedef typename InputCellType::CellAutoPointer                   InputCellAutoPointer;
  typedef typename InputMeshType::CellAutoPointer                   CellAutoPointer;
  typedef          itk::PolygonCell< InputCellType >                InputPolygonType;
  typedef typename InputPolygonType::PointIdIterator                InputPolygonPointIdIterator;
  typedef CovariantVector< typename InputVectorType::ValueType, 3 > CovariantVectorType;
  typedef  TOutputMesh                                              OutputMeshType;
  typedef typename OutputMeshType::Pointer                          OutputMeshPointer;
  typedef typename OutputMeshType::CellType                         OutputCellType;
  typedef          itk::PolygonCell< OutputCellType >               OutputPolygonType;

  typedef typename itk::MapContainer< CellIdentifier, double > DoubleValueMapType;
  typedef typename DoubleValueMapType::Iterator                DoubleContainerIterator;

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
    InputMeshPointer            mesh;
    double                      totalArea;
    double                      totalCurvature;
    double                      minCellSize;
    double                      maxCellSize;
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
      minCellSize = NumericTraits< double >::max();
      maxCellSize = 0;
      minCurvature = NumericTraits< double >::max();
      maxCurvature = 0;
    }

    /** \brief visits all polygon cells and computes the area,
     *  NOTE: works for convex polygons only!!!
     */
    void Visit(CellIdentifier cellId, InputPolygonType *poly)
    {
      typename InputPolygonType::PointIdIterator it =  poly->PointIdsBegin();

      double        meanCurvature = 0;
      PointIdentifier refPoint = *it;
      double        val = mesh->GetMeanCurvature(*it++);
      meanCurvature += std::abs(val);

      PointIdentifier id1 = *it;
      val = mesh->GetMeanCurvature(*it++);
      meanCurvature += std::abs(val);

      PointIdentifier id2;

      double area = 0;

      int cnt = 0;

      while ( it != poly->PointIdsEnd() )
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

      if ( area > maxCellSize ) { maxCellSize = area; }
      if ( area < minCellSize ) { minCellSize = area; }
      if ( meanCurvature > maxCurvature ) { maxCurvature = meanCurvature; }
      if ( meanCurvature < minCurvature ) { minCurvature = meanCurvature; }
    }

    double ComputeArea(PointIdentifier p1, PointIdentifier p2, PointIdentifier p3)
    {
      InputPointType v1, v2, v3;

      v1.Fill(0);
      v2.Fill(0);
      v3.Fill(0);

      mesh->GetPoint(p1, &v1);
      mesh->GetPoint(p2, &v2);
      mesh->GetPoint(p3, &v3);
      return std::abs (itk_cross_3d( ( v2 - v1 ).GetVnlVector(), ( v3 - v1 ).GetVnlVector() ).two_norm() / 2.0);
    }

    typename DoubleValueMapType::Pointer GetAreaMap()
    {
      return areaMap;
    }

    typename DoubleValueMapType::Pointer GetCurvatureMap()
    {
      return curvatureMap;
    }

    double GetTotalMeshArea()
    {
      return totalArea;
    }

    double GetTotalMeanCurvature()
    {
      return totalCurvature / ( curvatureMap->Size() );
    }

    double GetMaximumCellSize()
    {
      return maxCellSize;
    }

    double GetMinimumCellSize()
    {
      return minCellSize;
    }

    double GetMaximumCurvature()
    {
      return maxCurvature;
    }

    double GetMinimumCurvature()
    {
      return minCurvature;
    }
  };

  // cell visitor stuff
  typedef itk::CellInterfaceVisitorImplementation< InputPixelType,
                                                   InputCellTraitsType,
                                                   InputPolygonType,
                                                   SimplexCellVisitor >
  SimplexVisitorInterfaceType;

  typedef typename SimplexVisitorInterfaceType::Pointer SimplexVisitorInterfacePointer;
  typedef typename InputCellType::MultiVisitor          CellMultiVisitorType;
  typedef typename CellMultiVisitorType::Pointer        CellMultiVisitorPointer;

  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  itkSetMacro(SelectionMethod, int);
  itkGetConstMacro(SelectionMethod, int);

  itkGetConstMacro(ModifiedCount, int);

protected:

  SimplexMeshAdaptTopologyFilter();
  ~SimplexMeshAdaptTopologyFilter() ITK_OVERRIDE;
  SimplexMeshAdaptTopologyFilter(const Self &) {}

  void operator=(const Self &) {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  /**
   * Initialize this filters containers
   */
  void Initialize();

  /**
   * Method computes and evaluates cell propeties,
   * like area and curvature and determines whether
   * a cell should be refined or not.
   */
  void ComputeCellParameters();

  /** Copy Geometry data from the input mesh to the output mesh. */
  void CopyInputMeshToOutputMeshGeometryData();

  /**
   * Update topology neighbor relations for all cells
   * which are were influenced by he insertion of new
   * points.
   */
  void ModifyNeighborCells(CellIdentifier id1, CellIdentifier id2, PointIdentifier insertPointId);

  /**
   * Compute the center of a cell
   */
  InputPointType ComputeCellCenter(InputCellAutoPointer & simplexCell);

  /**
   * class member storing cell id offset
   */
  CellIdentifier m_IdOffset;

  /**
   * threshold controls the percentage of cells
   * to satify the selection criteria
   */
  double m_Threshold;

  /**
   * different criteria for cell refinement selection
   */
  int m_SelectionMethod;

  /**
   * atttribute contains the number of cells
   * which were modified during the last Update()
   */
  int m_ModifiedCount;

  /**
   * \brief member for accessing the filter result during
   *  creation
   */
  OutputMeshPointer m_Output;

  InputCellAutoPointer m_NewSimplexCellPointer;
};
} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshAdaptTopologyFilter.hxx"
#endif

#endif // itkSimplexMeshAdaptTopologyFilter_h
