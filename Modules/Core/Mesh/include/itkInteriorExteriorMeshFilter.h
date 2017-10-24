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
#ifndef itkInteriorExteriorMeshFilter_h
#define itkInteriorExteriorMeshFilter_h

#include "itkMeshToMeshFilter.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class InteriorExteriorMeshFilter
 * \brief
 *
 * InteriorExteriorMeshFilter takes an itk::Mesh and extracts from it a Sub-Mesh
 * such that all its points Evaluate > 0 in an itk::SpatialFunction provided
 * by the user.
 *
 * This filter is templated over the Input Mesh type, the Output Mesh type
 * and the SpatialFunctionType. However the only requirement for the Spatial
 * function is to support SmartPointers and to provide an Execute() method,
 * along with a typedef for OutputType (for the type returned by Execute() ).
 *
 * The additional content of the mesh is passed untouched. Including the
 * connectivity and the additional information contained on cells and points.
 * However, attention should be paid to the cells because some of their points
 * could not exist in the output mesh, if they did not satisfy the criterion
 * imposed by the spatial function.
 *
 * \ingroup MeshFilters
 * \ingroup ITKMesh
 */
template< typename TInputMesh, typename TOutputMesh, typename TSpatialFunction >
class ITK_TEMPLATE_EXPORT InteriorExteriorMeshFilter:
  public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef InteriorExteriorMeshFilter                  Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  typedef TInputMesh                       InputMeshType;
  typedef TOutputMesh                      OutputMeshType;
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType CoordRepType;

  /** Type of the  transform. */
  typedef TSpatialFunction SpatialFunctionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(InteriorExteriorMeshFilter, MeshToMeshFilter);

  /** Get/Set the spatial function. */
  itkSetObjectMacro(SpatialFunction, SpatialFunctionType);
  itkGetModifiableObjectMacro(SpatialFunction, SpatialFunctionType);

  typedef DataObjectDecorator< SpatialFunctionType >
  SpatialFunctionDataObjectType;
  typedef typename SpatialFunctionDataObjectType::Pointer
  SpatialFunctionDataObjectPointer;

protected:
  InteriorExteriorMeshFilter();
  ~InteriorExteriorMeshFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate requested data. */
  virtual void GenerateData(void) ITK_OVERRIDE;

  /** Transform applied to all the mesh points. */
  typename SpatialFunctionType::Pointer m_SpatialFunction;

private:
  InteriorExteriorMeshFilter(const InteriorExteriorMeshFilter &); //purposely
                                                                  // not
                                                                  // implemented
  void operator=(const InteriorExteriorMeshFilter &);             //purposely

  // not
  // implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInteriorExteriorMeshFilter.hxx"
#endif

#endif
