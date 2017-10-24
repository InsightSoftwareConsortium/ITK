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
#ifndef itkNormalQuadEdgeMeshFilter_h
#define itkNormalQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshPolygonCell.h"
#include "itkTriangleHelper.h"

namespace itk
{
/** \class NormalQuadEdgeMeshFilter
 *
 * \brief Filter which computes normals to faces and vertices and store it in
 * the output mesh.
 * Normals to face are first computed, then normals to vertices are computed
 * as linear combination of neighbor face normals, i.e.
 * \f[
 * n_v = \frac{\sum_{i=0}^{N_f} \omega_i \cdot n_i}{\| \sum_{k=0}^{N_f} \omega_x \cdot n_k\|}
 * \f]
 *
 * The difference between each method relies in the definition of the weight
 * \f$ \omega_i \f$ that you can specify by the method SetWeight.
 *
 * <ul>
 *    <li> GOURAUD \f$ \omega_i = 1\f$ [1] </li>
 *    <li> THURMER \f$ \omega_i = \f$ Angle of the considered triangle at the given vertex} [2] </li>
 *    <li> AREA \f$ \omega_i = Area(t_i)\f$ [3] </li>
 * </ul>
 *
 * These weights are defined in the literature:
 * <ul>
 *    <li> [1] Henri Gouraud. Continuous shading of curved surfaces.
 *    IEEE Transaction on Computers, 20(6):623-629, 1971 </li>
 *    <li> [2] Shuangshuang Jin, Robert R. Lewis, and David West.
 *    A comparison of algorithms for vertex normal computation.
 *    The Visual Computer, 21(1-2):71-82, 2005. </li>
 *    <li> [3] Grit Thurmer and Charles A. Wuthrich.
 *    Computing vertex normals from polygonal facets.
 *    Journal of Graphic Tools, 3(1):43-46, 1998. </li>
 * </ul>
 *
 * \note By default the weight is set to the TURMER weight.
 *
 * \todo Fix run-time issues regarding the difference between the Traits of
 * TInputMesh and the one of TOutputMesh. Right now, it only works if
 * TInputMesh::MeshTraits == TOutputMesh::MeshTraits
 * (and of course it requires that the output have some itk::Vector for point
 * data and cell data.
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT NormalQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef NormalQuadEdgeMeshFilter                                    Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  itkNewMacro (Self);

  itkTypeMacro (NormalQuadEdgeMeshFilter,
                QuadEdgeMeshToQuadEdgeMeshFilter);

  typedef TInputMesh                              InputMeshType;
  typedef typename InputMeshType::Pointer         InputMeshPointer;
  typedef typename InputMeshType::PointIdentifier InputPointIdentifier;
  typedef typename InputMeshType::PointType       InputPointType;
  typedef typename InputMeshType::VectorType      InputVectorType;
  typedef typename InputMeshType::QEType          InputQEType;

  typedef TOutputMesh                                          OutputMeshType;
  typedef typename OutputMeshType::Pointer                     OutputMeshPointer;
  typedef typename OutputMeshType::PointType                   OutputPointType;
  typedef typename OutputPointType::VectorType                 OutputVectorType;
  typedef typename OutputMeshType::QEType                      OutputQEType;
  typedef typename OutputMeshType::PointIdentifier             OutputPointIdentifier;
  typedef typename OutputMeshType::PointIdIterator             OutputPointIdIterator;
  typedef typename OutputMeshType::PointsContainerPointer      OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator     OutputPointsContainerIterator;
  typedef typename OutputMeshType::CellType                    OutputCellType;
  typedef typename OutputMeshType::CellIdentifier              OutputCellIdentifier;
  typedef typename OutputMeshType::CellAutoPointer             OutputCellAutoPointer;
  typedef typename OutputMeshType::CellsContainerConstIterator OutputCellsContainerPointer;
  typedef typename OutputMeshType::CellsContainerConstIterator OutputCellsContainerConstIterator;

  typedef TriangleHelper< OutputPointType > TriangleType;

  typedef QuadEdgeMeshPolygonCell< OutputCellType >   OutputPolygonType;
  typedef typename OutputPolygonType::SelfAutoPointer OutputPolygonAutoPointer;

  typedef typename OutputMeshType::CellDataContainer  OutputCellDataContainer;
  typedef typename OutputMeshType::PointDataContainer OutputPointDataContainer;

  typedef typename OutputMeshType::MeshTraits        OutputMeshTraits;
  typedef typename OutputMeshTraits::PixelType       OutputVertexNormalType;
  typedef typename OutputVertexNormalType::ValueType OutputVertexNormalComponentType;

  typedef typename OutputMeshTraits::CellPixelType OutputFaceNormalType;
  typedef typename OutputFaceNormalType::ValueType OutputFaceNormalComponentType;

  enum WeightType {
    GOURAUD = 0, // Uniform weights
    THURMER,     // Angle on a triangle at the given vertex
    AREA
    };

  itkSetMacro (Weight, WeightType);
  itkGetConstMacro (Weight, WeightType);

protected:
  NormalQuadEdgeMeshFilter();
  ~NormalQuadEdgeMeshFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  WeightType m_Weight;

  /** \brief Compute the normal to a face iPoly. It assumes that iPoly != 0
  * and
  * iPoly is a Triangle, i.e. 3 points only.
  * \note The normal computation itself can be further improved by making
  * possible to cast a CellType into a TriangleType.
  */
  OutputFaceNormalType ComputeFaceNormal(OutputPolygonType *iPoly);

  /** \brief Compute the normal to all faces on the mesh.
  * \note This method should be implemented in a multi-thread way in order
  * to reduce the processing time.
  */
  void ComputeAllFaceNormals();

  /** \brief Compute the normal to all vertices on the mesh.
  * \note This method should be implemented in a multi-thread way in order
  * to reduce the processing time.
  */
  void ComputeAllVertexNormals();

  /** \brief Compute the normal to one vertex by a weighted sum of the faces
  * normal in the 0-ring.
  * \note The weight is chosen by the member m_Weight.
  */
  OutputVertexNormalType ComputeVertexNormal(const OutputPointIdentifier & iId, OutputMeshType *outputMesh);

  /** \brief Definition of the weight in the 0-ring used for the vertex
  * normal computation. By default m_Weight = THURMER;
  */
  OutputVertexNormalComponentType Weight(const OutputPointIdentifier & iPId,
                                         const OutputCellIdentifier & iCId,
                                         OutputMeshType *outputMesh);

  /** \note Calling Superclass::GenerateData( ) is the longest part in the
  * filter! Something must be done in the class
  * itkQuadEdgeMeshToQuadEdgeMeshFilter.
  */
  void GenerateData() ITK_OVERRIDE;

private:
  NormalQuadEdgeMeshFilter (const Self &);
  void operator=(const Self &);
};
}

#include "itkNormalQuadEdgeMeshFilter.hxx"
#endif
