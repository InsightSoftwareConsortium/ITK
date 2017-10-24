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
#ifndef itkWarpMeshFilter_h
#define itkWarpMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
/** \class WarpMeshFilter
 * \brief
 *
 * WarpMeshFilter applies a deformation field to all the points of a mesh.
 * The deformation field is represented as an image of Vectors.
 *
 * The additional content of the mesh is passed untouched. Including the
 * connectivity and the additional information contained on cells and points.
 *
 * Meshes that have added information like normal vector on the points, will
 * have to take care of transforming this data by other means.
 *
 * \ingroup MeshFilters
 * \sa TransformMeshFilter
 * \ingroup ITKMesh
 */
template< typename TInputMesh, typename TOutputMesh, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT WarpMeshFilter:
  public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef WarpMeshFilter                              Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutputMesh                      OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType CoordRepType;

  /** Deformation field typedef support. */
  typedef TDisplacementField                           DisplacementFieldType;
  typedef typename DisplacementFieldType::ConstPointer DisplacementFieldPointer;
  typedef typename DisplacementFieldType::PixelType    DisplacementType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(WarpMeshFilter, MeshToMeshFilter);

  /** Set the deformation field. */
  void SetDisplacementField(const DisplacementFieldType *field);

  /** Get a pointer the deformation field. */
  const DisplacementFieldType * GetDisplacementField() const;

protected:
  WarpMeshFilter();
  ~WarpMeshFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Requested Data */
  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WarpMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWarpMeshFilter.hxx"
#endif

#endif
