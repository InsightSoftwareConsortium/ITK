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
#ifndef itkPolyData_h
#define itkPolyData_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{

/** \class PolyData
 *
 * \brief Geometry class compatible with vtk.js PolyData
 *
 * \ingroup MeshToPolyData
 */
template< typename TPixel >
class ITK_TEMPLATE_EXPORT PolyData: public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolyData);

  using Self = PolyData;
  using Superclass = DataObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  static constexpr unsigned int PointDimension = 3;


  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolyData, DataObject);

  /** Type of PointData or CellData */
  using PixelType = TPixel;
  using MeshTraits = DefaultStaticMeshTraits< PixelType, PointDimension, PointDimension >;

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  using CoordRepType = typename MeshTraits::CoordRepType;
  using PointIdentifier = typename MeshTraits::PointIdentifier;
  using PointType = typename MeshTraits::PointType;
  using PointsContainer = typename MeshTraits::PointsContainer;
  using PointDataContainer = typename MeshTraits::PointDataContainer;
  using CellIdentifier = typename MeshTraits::CellIdentifier;
  using CellDataContainer = typename MeshTraits::CellDataContainer;
  using CellContainer = VectorContainer< CellIdentifier, uint32_t >;

  void Initialize() override;

  PointIdentifier GetNumberOfPoints() const;

  /** Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container. */
  void SetPoints(PointsContainer *);
  PointsContainer * GetPoints();
  const PointsContainer * GetPoints() const;

  /** Lines in format [1 pointIndex1 1 pointIndex2 1 pointIndex3 ... ] */
  void SetVertices(CellContainer *);
  CellContainer * GetVertices();
  const CellContainer* GetVertices() const;

  /** Lines in format [nPointsLine1 pointIndex1 pointIndex2 nPointsLine2 pointIndex1 pointIndex2 ... ] */
  void SetLines(CellContainer *);
  CellContainer * GetLines();
  const CellContainer* GetLines() const;

  /** Polygons in format [nPointsPolygon1 pointIndex1 pointIndex2 nPointsPolygon2 pointIndex1 pointIndex2 ... ] */
  void SetPolygons(CellContainer *);
  CellContainer * GetPolygons();
  const CellContainer* GetPolygons() const;

  /** TriangleStrips in format [nPointsTriangleStrip1 pointIndex1 pointIndex2 nPointsTriangleStrip2 pointIndex1 pointIndex2 ... ] */
  void SetTriangleStrips(CellContainer *);
  CellContainer * GetTriangleStrips();
  const CellContainer* GetTriangleStrips() const;

  void SetPointData(PointDataContainer *);
  PointDataContainer * GetPointData();
  const PointDataContainer * GetPointData() const;

  /** Access routines to fill the Points container, and get information
   * from it. */
  void SetPoint(PointIdentifier, PointType);
  bool GetPoint(PointIdentifier, PointType *) const;
  PointType GetPoint(PointIdentifier) const;

  /** Access routines to fill the PointData container, and get information
   * from it. */
  void SetPointData(PointIdentifier, PixelType);
  bool GetPointData(PointIdentifier, PixelType *) const;

  /** Access m_CellDataContainer, which contains data associated with
   *  the mesh's cells.  Optionally, this can be nullptr, indicating that
   *  no data are associated with the cells.  The data for a cell can
   *  be accessed through its cell identifier.  */
  void SetCellData(CellDataContainer *);
  CellDataContainer *  GetCellData();
  const CellDataContainer * GetCellData() const;

  /** Access routines to fill the CellData container, and get information
   *  from it.  */
  void SetCellData(CellIdentifier, PixelType);
  bool GetCellData(CellIdentifier, PixelType *) const;

protected:
  PolyData();
  ~PolyData() override = default;

  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  typename PointsContainer::Pointer m_PointsContainer;

  typename CellContainer::Pointer m_VerticesContainer;
  typename CellContainer::Pointer m_LinesContainer;
  typename CellContainer::Pointer m_PolygonsContainer;
  typename CellContainer::Pointer m_TriangleStripsContainer;

  /** An object containing data associated with the mesh's points.
   * Optionally, this can be nullptr, indicating that no data are associated with
   * the points.  The data for a point can be accessed through its point
   * identifier. */
  typename PointDataContainer::Pointer m_PointDataContainer;

  typename CellDataContainer::Pointer m_CellDataContainer;
private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolyData.hxx"
#endif

#endif // itkPolyData_h
