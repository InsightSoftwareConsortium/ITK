/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSource.h
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
#ifndef __itkSphereSource_h
#define __itkSphereSource_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{

/** \class itkSphere Source
 * \brief 
 *
 * Input the center and resolutions in 2 direction(verizon and horizon)
 * to create a sphere-like deformable model. The cell on the surface is
 * in the shape of triangular. 
 * More parameters are added to make the sphere mesh has global and local
 * deform ability.
 */
template <typename TOutputMesh>
class ITK_EXPORT SphereSource : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef SphereSource         Self;
  typedef MeshSource<TOutputMesh>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef typename OutputMeshType::MeshTraits   OMeshTraits;
  typedef typename OutputMeshType::PointType    OPointType;
  typedef typename OMeshTraits::PixelType       OPixelType;  

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits CellTraits;
  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer   PointsContainer;
  typedef typename OutputMeshType::PointType     PointType;
  
  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  typedef itk::TriangleCell<OPixelType, CellTraits> TriCell;
  typedef typename TriCell::Pointer TriCellPointer;

  /** All these parameter setting function are public temporarily to make the
   * test easier */
  itkSetMacro(ResolutionX, int);
  itkSetMacro(ResolutionY, int);
  itkSetMacro(Center, OPointType);
  itkSetMacro(Scale,  OPointType);
  itkSetMacro(Squareness1, double);
  itkSetMacro(Squareness2, double);

protected:
  SphereSource();
  ~SphereSource() {};

  void GenerateData();

  /** model center */
  OPointType m_Center; 

  /** model resolutions */
  int m_ResolutionX;

  int m_ResolutionY;

  /** model scales */
  OPointType m_Scale;
  
  double m_Squareness1;
  double m_Squareness2;

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereSource.txx"
#endif
#endif
