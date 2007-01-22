/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVTKPolyDataReader_h
#define __itkVTKPolyDataReader_h

#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTriangleCell.h"
#include "itkMapContainer.h"

namespace itk
{

/** \class VTKPolyDataReader
 * \brief
 * Reads a vtkPolyData file and create an itkMesh.
 *
 * Caveat: itkVTKPolyDataReader can only read triangle meshes.
 *         Use vtkTriangleFilter to convert your mesh to a triangle mesh.
 */
template <class TOutputMesh>
class VTKPolyDataReader : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef VTKPolyDataReader         Self;
  typedef MeshSource<TOutputMesh>   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataReader, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh                          OutputMeshType;
  typedef typename OutputMeshType::MeshTraits  MeshTraits;
  typedef typename OutputMeshType::PointType   PointType;
  typedef typename MeshTraits::PixelType       PixelType;

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer     OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits  CellTraits;

  typedef typename OutputMeshType::PointsContainerPointer
    PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer
    PointsContainer;

  /** Define the triangular cell types which form the surface  */
  typedef CellInterface<PixelType, CellTraits> CellInterfaceType;
  typedef TriangleCell<CellInterfaceType>      TriangleCellType;

  typedef typename TriangleCellType::SelfAutoPointer
    TriangleCellAutoPointer;
  typedef typename TriangleCellType::CellAutoPointer
    CellAutoPointer;

  typedef std::pair<unsigned long,unsigned long>     IndexPairType;
  typedef MapContainer<IndexPairType, unsigned long> PointMapType;
  typedef typename PointType::VectorType             VectorType;

  /** Set the resolution level to be used for generating cells in the
   * Sphere. High values of this parameter will produce sphere with more
   * triangles. */
  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

protected:
  VTKPolyDataReader();
  ~VTKPolyDataReader() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Reads the file */
  void GenerateData();

  /** Filename to read */
  std::string m_FileName;

private:
  VTKPolyDataReader(const Self&); // purposely not implemented
  void operator=(const Self&); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKPolyDataReader.txx"
#endif

#endif //_itkVTKPolyDataReader_h
