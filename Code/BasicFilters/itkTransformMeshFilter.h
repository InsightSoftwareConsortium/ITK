/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformMeshFilter_h
#define __itkTransformMeshFilter_h

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
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
class ITK_EXPORT TransformMeshFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef TransformMeshFilter  Self;
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  
  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  /** Type of the transform. */
  typedef TTransform  TransformType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformMeshFilter,MeshToMeshFilter);

  /** Set transform. */
  itkSetObjectMacro(Transform, TransformType); 

  /** Get transform. */
  itkGetObjectMacro(Transform,TransformType);

protected:
  TransformMeshFilter();
  ~TransformMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate Requested Data */
  virtual void GenerateData( void );

 /** Transform to apply to all the mesh points. */
  typename TransformType::Pointer   m_Transform;

private:
  TransformMeshFilter(const TransformMeshFilter&); //purposely not implemented
  void operator=(const TransformMeshFilter&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformMeshFilter.txx"
#endif

#endif
