/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableMesh.h
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
#ifndef __itkdeformablemesh_h
#define __itkdeformablemesh_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{

/** \class DeformableMesh
 * \brief 
 *
 * DeformableMesh is a class that define a deformable model structure
 * in the form of Mesh. It is based on the itkMesh structure.
 * All nodes on the model will be calculated and stored in the 
 * pointscontainer. User can change the parameters such as the 
 * resolution and scale, etc to decide the initial forms and property
 * of the model.
 * The connectness of nodes ( the cells make up the surface of the model )
 * is stored in the cellscontainer.
 * The model have both global and local deforming ability.
 *
 * \ingroup MeshFilters
 * \ingroup MeshSegmentation
 */
template <typename TPixelType/*, 
  typename TMeshTraits = DefaultStaticMeshTraits< TPixelType >*/>
class ITK_EXPORT DeformableMesh : public Mesh<TPixelType/*, TMeshTraits*/>
{
public:
  /** Standard class typedefs. */
  typedef DeformableMesh  Self;
  typedef Mesh<TPixelType/*, TMeshTraits*/> Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableMesh,Mesh);

  /** Standard template parameter typedef. */
  typedef TPixelType PixelType;

  /** Typedefs are not inherited.
   * Get typedef from superclass. */
  typedef typename Superclass::CellTraits CellTraits;
  
  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  typedef TriangleCell<PixelType, CellTraits>     TriCell;
  typedef typename TriCell::Pointer TriCellPointer;

  /** All these parameter setting function are public temporarily to make
   * the test easier. */
  void SetResolution(int a, int b);
  void SetCenter(int a, int b, int c);
  void SetScale(float a, float b, float c);
  void SetDefault();
  void Allocate();

protected:
  DeformableMesh();
  ~DeformableMesh() {}

  /** Model center. */
  int m_Center[3]; 

  /** Model resolutions. */
  int m_Resolution[2];

  /** Model scales. */
  float m_Scale[3];
  
private:
  DeformableMesh(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMesh.txx"
#endif
#endif
