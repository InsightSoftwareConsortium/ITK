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
#ifndef itkTransformMeshFilter_h
#define itkTransformMeshFilter_h

#include "itkMeshToMeshFilter.h"
#include "itkTransform.h"

namespace itk
{
/** \class TransformMeshFilter
 * \brief
 *
 * TransformMeshFilter applies a transform to all the points
 * of a mesh.
 *
 * The additional content of the mesh is passed untouched. Including the
 * connectivity and the additional information contained on cells and points.
 *
 * Meshes that have added information like normal vector on the points, will
 * have to take care of transforming this data by other means.
 *
 * \ingroup MeshFilters
 * \ingroup ITKMesh
 */
template< typename TInputMesh, typename TOutputMesh, typename TTransform >
class ITK_TEMPLATE_EXPORT TransformMeshFilter:
  public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef TransformMeshFilter                         Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  typedef TInputMesh                       InputMeshType;
  typedef TOutputMesh                      OutputMeshType;
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType CoordRepType;

  /** Type of the transform. */
  typedef TTransform TransformType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformMeshFilter, MeshToMeshFilter);

  /** Get/Set transform. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetModifiableObjectMacro(Transform, TransformType);

protected:
  TransformMeshFilter();
  ~TransformMeshFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Requested Data */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Transform to apply to all the mesh points. */
  typename TransformType::Pointer m_Transform;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TransformMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformMeshFilter.hxx"
#endif

#endif
