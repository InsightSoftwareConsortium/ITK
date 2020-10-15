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
#ifndef itkParametricSpaceToImageSpaceMeshFilter_h
#define itkParametricSpaceToImageSpaceMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
/** \class ParametricSpaceToImageSpaceMeshFilter
 * \brief
 *
 * ParametricSpaceToImageSpaceMeshFilter takes an itk::Mesh on which
 * the point Data is expected to contain itk::Index of itk::Image pixels
 * associated with each point of the Mesh, and construct with them a new
 * mesh whose points are in the coordinates of those pixels.
 *
 * The input mesh is assumed to represent pixels in some parametric space.
 * The output mesh is suitable to be superimposed as an overlay to the
 * associated image specified by the user.
 *
 * The additional content of the mesh is passed untouched, including the
 * connectivity and the additional information contained on cells and points.
 *
 * \warning This filter also assumes that the Output Mesh has as type
 * for the PointDataType the PointType of the input mesh.
 *
 * \ingroup MeshFilters
 * \ingroup ITKMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ParametricSpaceToImageSpaceMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ParametricSpaceToImageSpaceMeshFilter);

  /** Standard class type aliases. */
  using Self = ParametricSpaceToImageSpaceMeshFilter;
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type for representing coordinates. */
  using CoordRepType = typename TInputMesh::CoordRepType;

  using InputMeshType = TInputMesh;
  using OutputMeshType = TOutputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParametricSpaceToImageSpaceMeshFilter, MeshToMeshFilter);

protected:
  ParametricSpaceToImageSpaceMeshFilter();
  ~ParametricSpaceToImageSpaceMeshFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate requested data. */
  void
  GenerateData() override;

  /** Generate additional information in the output  */
  void
  GenerateOutputInformation() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkParametricSpaceToImageSpaceMeshFilter.hxx"
#endif

#endif
