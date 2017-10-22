/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkVTKTetrahedralMeshReader.h,v $
  Language:  C++
  Date:      $Date: 2011-10-05 18:01:00 $
  Version:   $Revision: 1.9 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or https://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itkVTKTetrahedralMeshReader_h
#define itkVTKTetrahedralMeshReader_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTetrahedronCell.h"


namespace itk
{

/** \class VTKTetrahedralMeshReader
 * \brief
 * Reads a VTKUnstructuredGrid file and create an itkMesh.
 *
 * Caveat1: itkVTKTetrahedralMeshReader can only read tetrahedral meshes.
 *          Use vtkTriangleFilter to convert your mesh to a triangle mesh.
 * Caviet2: itkVTKTetrahedralMeshReader can only read vtk legacy files.
 * Caveat3: itkVTKTetrahedralMeshReader cannot read binary vtk files.
 */
template <typename TOutputMesh>
class ITK_TEMPLATE_EXPORT VTKTetrahedralMeshReader : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef VTKTetrahedralMeshReader  Self;
  typedef MeshSource<TOutputMesh>   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKTetrahedralMeshReader, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh                          OutputMeshType;
  typedef typename OutputMeshType::MeshTraits  MeshTraits;
  typedef typename OutputMeshType::PointType   PointType;
  typedef typename MeshTraits::PixelType       PixelType;

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer         OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits      CellTraits;
  typedef typename OutputMeshType::CellIdentifier  CellIdentifier;
  typedef typename OutputMeshType::CellType        CellType;
  typedef typename OutputMeshType::CellAutoPointer CellAutoPointer;
  typedef typename OutputMeshType::PointIdentifier PointIdentifier;
  typedef typename CellTraits::PointIdIterator     PointIdIterator;

  typedef typename OutputMeshType::PointsContainerPointer
    PointsContainerPointer;

  typedef typename OutputMeshType::PointsContainer
    PointsContainer;

  /** Define the tetrahedron cell types which form the volume  */
  typedef TetrahedronCell<CellType>   TetrahedronCellType;

  typedef typename TetrahedronCellType::SelfAutoPointer
    TetrahedronCellAutoPointer;


  /** Set the resolution level to be used for generating cells in the
   * Sphere. High values of this parameter will produce sphere with more
   * triangles. */
  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Get the file version line */
  itkGetStringMacro(Version);

  /** Get the file header line */
  itkGetStringMacro(Header);

protected:
  VTKTetrahedralMeshReader();
  ~VTKTetrahedralMeshReader() ITK_OVERRIDE;
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Reads the file */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKTetrahedralMeshReader);

  /** Filename to read */
  std::string m_FileName;
  std::string m_Header;
  std::string m_Version;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKTetrahedralMeshReader.hxx"
#endif

#endif //_itkVTKTetrahedralMeshReader_h
