/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereMeshSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSphereMeshSource_h
#define __itkSphereMeshSource_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{

/** \class SphereMeshSource
 * \brief 
 *
 * Input the center and resolutions in 2 direction(verizon and horizon)
 * to create a sphere-like deformable model. The cell on the surface is
 * in the shape of triangular. 
 * More parameters are added to make the sphere mesh has global and local
 * deform ability.
 */
template <class TOutputMesh>
class ITK_EXPORT SphereMeshSource : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef SphereMeshSource         Self;
  typedef MeshSource<TOutputMesh>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereMeshSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh OutputMeshType;
  typedef typename OutputMeshType::MeshTraits   OMeshTraits;
  typedef typename OutputMeshType::PointType    OPointType;
  typedef typename OMeshTraits::PixelType       OPixelType;  

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits CellTraits;
  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer   PointsContainer;
  
  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  typedef CellInterface<OPixelType, CellTraits>   CellInterfaceType;
  typedef TriangleCell<CellInterfaceType>         TriCellType;
  typedef typename TriCellType::SelfAutoPointer       TriCellAutoPointer;
  typedef typename TriCellType::CellAutoPointer       CellAutoPointer;

  /** All these parameter setting function are public temporarily to make the
   * test easier */
  itkSetMacro(ResolutionX, unsigned int);
  itkSetMacro(ResolutionY, unsigned int);

  itkSetMacro(Center, OPointType);
  itkSetMacro(Scale,  OPointType);

  itkSetMacro(Squareness1, double);
  itkSetMacro(Squareness2, double);

protected:
  SphereMeshSource();
  ~SphereMeshSource() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

  /** model center */
  OPointType m_Center; 

  /** model resolutions */
  unsigned int m_ResolutionX;
  unsigned int m_ResolutionY;

  /** model scales */
  OPointType m_Scale;
  
  /** model squareness */
  double m_Squareness1;
  double m_Squareness2;

private:
  SphereMeshSource(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereMeshSource.txx"
#endif
#endif
