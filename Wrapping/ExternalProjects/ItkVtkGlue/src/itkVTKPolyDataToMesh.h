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

#ifndef __itkVTKPolyDataToMesh_h
#define __itkVTKPolyDataToMesh_h

#include "vtkPoints.h"
#include "vtkCellArray.h"
#include "vtkPolyData.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkObject.h"


namespace itk
{

/**
  \class VTKPolyDataToMesh
  \brief
    \warning
  \sa
  */

template <class TMesh >
class VTKPolyDataToMesh : public Object
{

 public:

  /** Standard class typedefs. */
  typedef VTKPolyDataToMesh         Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataToMesh, Object);

  typedef TMesh                                 TriangleMeshType;
  typedef typename TriangleMeshType::MeshTraits TriangleMeshTraits;

  /**
  The SetInput method provides pointer to the vtkPolyData
  */
  void SetInput( vtkPolyData * polydata);
  vtkPolyData *  GetInput();

  TriangleMeshType * GetOutput();

  void Update();

 private:

  VTKPolyDataToMesh( void );
  virtual ~VTKPolyDataToMesh( void );
  VTKPolyDataToMesh(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename TriangleMeshType::Pointer  m_ItkMesh;
  vtkPolyData                       * m_PolyData;


};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKPolyDataToMesh.txx"
#endif

#endif
