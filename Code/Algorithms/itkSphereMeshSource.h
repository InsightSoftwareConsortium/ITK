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

/** \class itkSphereMesh Source
 * \brief 
 *
 * Input parameters are:
 * (1) The Center of the SphereMesh
 * (2) The resolutions of the spatial sampling on the SphereMesh surface in both 
 *     verizon and horizon directions.
 * The cell surface is triangulated. 
 * The scale in the x, y, z directions can be reset.
 * Squearness1 and Squearness2 control the shape of the SphereMesh, 
 * when considered as a quadric surface. */
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
  typedef typename OutputMeshType::CellsContainerPointer CellsContainerPointer;
  typedef typename OutputMeshType::CellsContainer   CellsContainer;
  typedef typename OutputMeshType::CellDataContainerPointer CellDataContainerPointer;
  typedef typename OutputMeshType::CellDataContainer   CellDataContainer;
  
  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  typedef CellInterface<OPixelType, CellTraits>   CellInterfaceType;
  typedef TriangleCell<CellInterfaceType>         TriCellType;
  typedef typename TriCellType::CellAutoPointer   TriCellPointer;

  itkSetMacro(ResolutionX, unsigned int);
  itkSetMacro(ResolutionY, unsigned int);

  itkSetMacro(Center, OPointType);
  itkSetMacro(Scale,  OPointType);

  itkSetMacro(Squareness1, double);
  itkSetMacro(Squareness2, double);

protected:
  SphereMeshSource();
  ~SphereMeshSource() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  SphereMeshSource(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** model center. */
  OPointType m_Center; 

  /** model resolutions. */
  unsigned int m_ResolutionX;

  unsigned int m_ResolutionY;

  /** model scales. */
  OPointType m_Scale;
  
  /** model squareness. */
  double m_Squareness1;
  double m_Squareness2;

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereMeshSource.txx"
#endif
#endif
