/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 *
 */
template <class TInputMesh, class TOutputMesh, class TSpatialFunction >
class ITK_EXPORT InteriorExteriorMeshFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef InteriorExteriorMeshFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Type for representing coordinates
   */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  /** 
   * Type of the  Transform
   */
  typedef TSpatialFunction  SpatialFunctionType;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(InteriorExteriorMeshFilter,MeshToMeshFilter);

  /** 
   * Set Spatial Function.
   */
  itkSetObjectMacro( SpatialFunction, SpatialFunctionType ); 

  /** 
   * Get Spatial Function.
   */
  itkGetObjectMacro( SpatialFunction, SpatialFunctionType ); 

protected:
  InteriorExteriorMeshFilter();
  ~InteriorExteriorMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** 
   * Generate Requested Data
   */
  virtual void GenerateData( void );

 /**
  *   transform to apply to all the mesh points
  */
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
