/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInteriorExteriorMeshFilter_h
#define __itkInteriorExteriorMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{

/** \class InteriorExteriorMeshFilter
 * \brief 
 *
 * InteriorExteriorMeshFilter takes an itk::Mesh and extract from it a Sub-Mesh
 * such that all its points Evaluate > 0 in a itk::SpatialFunction provided 
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
 * could not exist in the output mesh, if they did not satisfay the criterion
 * imposed by the spatial function.
 * 
 * \ingroup MeshFilters
 */
template <class TInputMesh, class TOutputMesh, class TSpatialFunction >
class ITK_EXPORT InteriorExteriorMeshFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef InteriorExteriorMeshFilter  Self;
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  
  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  /** Type of the  transform. */
  typedef TSpatialFunction  SpatialFunctionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(InteriorExteriorMeshFilter,MeshToMeshFilter);

  /** Set the spatial function. */
  itkSetObjectMacro( SpatialFunction, SpatialFunctionType ); 

  /** Get the spatial function. */
  itkGetObjectMacro( SpatialFunction, SpatialFunctionType ); 

protected:
  InteriorExteriorMeshFilter();
  ~InteriorExteriorMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate requested data. */
  virtual void GenerateData( void );

  /** Transform applied to all the mesh points. */
  typename SpatialFunctionType::Pointer   m_SpatialFunction;

private:
  InteriorExteriorMeshFilter(const InteriorExteriorMeshFilter&); //purposely not implemented
  void operator=(const InteriorExteriorMeshFilter&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInteriorExteriorMeshFilter.txx"
#endif

#endif
