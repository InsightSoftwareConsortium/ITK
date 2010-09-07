/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWarpMeshFilter_h
#define __itkWarpMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
/** \class WarpMeshFilter
 * \brief
 *
 * WarpMeshFilter applies a deformation field to all the points of a mesh.
 * The deformation field is represented as an image of Vectors.
 *
 * The additional content of the mesh is passed untouched. Including the
 * connectivity and the additional information contained on cells and points.
 *
 * Meshes that have added information like normal vector on the points, will
 * have to take care of transforming this data by other means.
 *
 * \ingroup MeshFilters
 * \sa TransformMeshFilter
 */
template< class TInputMesh, class TOutputMesh, class TDeformationField >
class ITK_EXPORT WarpMeshFilter:
  public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef WarpMeshFilter                              Self;
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutputMesh                      OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType CoordRepType;

  /** Deformation field typedef support. */
  typedef TDeformationField                           DeformationFieldType;
  typedef typename DeformationFieldType::ConstPointer DeformationFieldPointer;
  typedef typename DeformationFieldType::PixelType    DisplacementType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(WarpMeshFilter, MeshToMeshFilter);

  /** Set the deformation field. */
  void SetDeformationField(const DeformationFieldType *field);

  /** Get a pointer the deformation field. */
  const DeformationFieldType * GetDeformationField(void) const;

protected:
  WarpMeshFilter();
  ~WarpMeshFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Generate Requested Data */
  virtual void GenerateData(void);

private:
  WarpMeshFilter(const WarpMeshFilter &); //purposely not implemented
  void operator=(const WarpMeshFilter &); //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWarpMeshFilter.txx"
#endif

#endif
