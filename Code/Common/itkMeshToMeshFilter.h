/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshToMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeshToMeshFilter_h
#define __itkMeshToMeshFilter_h

#include "itkMeshSource.h"

namespace itk
{
/** \class MeshToMeshFilter
 * \brief
 *
 * MeshToMeshFilter is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup MeshFilters
 *
 */
template< class TInputMesh, class TOutputMesh >
class ITK_EXPORT MeshToMeshFilter:public MeshSource< TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef MeshToMeshFilter           Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshToMeshFilter, MeshSource);

  /** Some convenient typedefs. */
  typedef TInputMesh                       InputMeshType;
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef TOutputMesh                      OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Set the mesh input of this process object.  */
  void SetInput(const InputMeshType *input);

  /** Get the mesh input of this process object.  */
  const InputMeshType * GetInput(void) const;

  const InputMeshType * GetInput(unsigned int idx) const;

protected:
  MeshToMeshFilter();
  ~MeshToMeshFilter() {}

  void CopyInputMeshToOutputMeshPoints();

  void CopyInputMeshToOutputMeshPointData();

  void CopyInputMeshToOutputMeshCellLinks();

  void CopyInputMeshToOutputMeshCells();

  void CopyInputMeshToOutputMeshCellData();

private:
  MeshToMeshFilter(const Self &); //purposely not implemented
  void operator=(const Self &);   //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshToMeshFilter.txx"
#endif

#endif
