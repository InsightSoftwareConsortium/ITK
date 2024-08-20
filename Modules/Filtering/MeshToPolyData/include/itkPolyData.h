/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
template< typename TPixel, typename TCellPixel = TPixel >
class ITK_TEMPLATE_EXPORT PolyData: public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PolyData);

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
  using CellPixelType = TCellPixel;
  using MeshTraits = DefaultStaticMeshTraits< PixelType, PointDimension, PointDimension, float, float, CellPixelType >;

  /** Convenient type alias obtained from TMeshTraits template parameter. */
  using CoordRepType = typename MeshTraits::CoordRepType;
  using PointIdentifier = typename MeshTraits::PointIdentifier;
  using PointType = typename MeshTraits::PointType;
  using PointsContainer = typename MeshTraits::PointsContainer;
  using PointDataContainer = typename MeshTraits::PointDataContainer;
  using CellIdentifier = typename MeshTraits::CellIdentifier;
  using CellDataContainer = typename MeshTraits::CellDataContainer;
  using CellsContainer = VectorContainer< CellIdentifier, uint32_t >;

  void Initialize() override;

  PointIdentifier GetNumberOfPoints() const;

  /** Define Set/Get access routines for each internal container.
   * Methods also exist to add points, cells, etc. one at a time
   * rather than through an entire container. */
  void SetPoints(PointsContainer *);
  PointsContainer * GetPoints();
  const PointsContainer * GetPoints() const;

  /** Vertices in format [1 pointIndex1 1 pointIndex2 1 pointIndex3 ... ] */
  void SetVertices(CellsContainer *);
  CellsContainer * GetVertices();
  const CellsContainer* GetVertices() const;

  /** Lines in format [nPointsLine1 pointIndex1 pointIndex2 nPointsLine2 pointIndex1 pointIndex2 ... ] */
  void SetLines(CellsContainer *);
  CellsContainer * GetLines();
  const CellsContainer* GetLines() const;

  /** Polygons in format [nPointsPolygon1 pointIndex1 pointIndex2 nPointsPolygon2 pointIndex1 pointIndex2 ... ] */
  void SetPolygons(CellsContainer *);
  CellsContainer * GetPolygons();
  const CellsContainer* GetPolygons() const;

  /** TriangleStrips in format [nPointsTriangleStrip1 pointIndex1 pointIndex2 nPointsTriangleStrip2 pointIndex1 pointIndex2 ... ] */
  void SetTriangleStrips(CellsContainer *);
  CellsContainer * GetTriangleStrips();
  const CellsContainer* GetTriangleStrips() const;

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
  void SetCellData(CellIdentifier, CellPixelType);
  bool GetCellData(CellIdentifier, CellPixelType *) const;

protected:
  PolyData();
  ~PolyData() override = default;

  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** An object containing points used by the mesh.  Individual points are
   * accessed through point identifiers. */
  typename PointsContainer::Pointer m_PointsContainer;

  typename CellsContainer::Pointer m_VerticesContainer;
  typename CellsContainer::Pointer m_LinesContainer;
  typename CellsContainer::Pointer m_PolygonsContainer;
  typename CellsContainer::Pointer m_TriangleStripsContainer;

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
