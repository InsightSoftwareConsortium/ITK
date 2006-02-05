/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularSphereMeshSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegularSphereMeshSource_h
#define __itkRegularSphereMeshSource_h

#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTriangleCell.h"
#include "itkMapContainer.h"

namespace itk
{

/** \class RegularSphereMeshSource
 * \brief 
 * Inputs are the center of the mesh, the scale (radius in each dimension) of the mesh
 * and a resolution parameter, which corresponds to the recursion 
 * depth whlie creating a spherical triangle mesh.
 *
 * Don't use recursion depths larger than 5, because mesh generation gets very slow. 
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 */
template <class TOutputMesh>
class RegularSphereMeshSource : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef RegularSphereMeshSource         Self;
  typedef itk::MeshSource<TOutputMesh>  Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularSphereMeshSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh OutputMeshType;
  typedef typename OutputMeshType::MeshTraits   MeshTraits;
  typedef typename OutputMeshType::PointType    PointType;
  typedef typename MeshTraits::PixelType        PixelType;  

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits CellTraits;
  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer   PointsContainer;
  
  /** Define the triangular cell types which form the surface  */
  typedef itk::CellInterface<PixelType, CellTraits>   CellInterfaceType;
  typedef itk::TriangleCell<CellInterfaceType>         TriCellType;
  typedef typename TriCellType::SelfAutoPointer       TriCellAutoPointer;
  typedef typename TriCellType::CellAutoPointer       CellAutoPointer;

  typedef std::pair<unsigned long,unsigned long> IndexPairType;
  typedef itk::MapContainer<IndexPairType, unsigned long> PointMapType;
  typedef typename PointType::VectorType VectorType;
    

  /** Set the resolution level to be used for generating cells in the Sphere.
   *  High values of this parameter will produce sphere with more triangles. */
  itkSetMacro(Resolution, unsigned int);
  itkGetMacro(Resolution, unsigned int);

  /** Set/Get Coordinates of the Sphere center. */
  itkSetMacro( Center, PointType  );
  itkGetMacro( Center, PointType  );

  /** Set/Get scales of the Sphere. This is a vector of values that can
   * actually be used for generating ellipsoids aligned with the coordinate
   * axis. */
  itkSetMacro( Scale,  VectorType );
  itkGetMacro( Scale,  VectorType );

protected:
  RegularSphereMeshSource();
  ~RegularSphereMeshSource() {}
  void PrintSelf(std::ostream& os, itk::Indent indent) const;

  void GenerateData();

  PointType Divide( const PointType & p1, const PointType & p2) const;

  void AddCell( OutputMeshType * mesh, const unsigned long * pointIds, unsigned long idx);

  /** model center */
  PointType m_Center; 

  /** models resolution */
  unsigned int m_Resolution;

  /** model scales */
  VectorType m_Scale;
  

private:
  RegularSphereMeshSource(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};



} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularSphereMeshSource.txx"
#endif

#endif //_itkRegularSphereMeshSource_h
