/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterMeshToMesh.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterMeshToMesh_h
#define __itkFilterMeshToMesh_h

#include "itkMeshSource.h"

namespace itk
{

/** \class FilterMeshToMesh
 * \brief 
 *
 * FilterMeshToMesh is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 */
template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT FilterMeshToMesh : public MeshSource<TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterMeshToMesh  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshSource<TOutputMesh> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterMeshToMesh,MeshSource);

  /** 
   * Some typedefs.
   */
  typedef TInputMesh InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  /** 
   * Set the mesh input of this process object. 
   */
  void SetInput(InputMeshType *input);

  /** 
   * Get the mesh input of this process object. 
   */
  InputMeshPointer GetInput();
  InputMeshPointer GetInput(unsigned int idx);

protected:
  FilterMeshToMesh();
  ~FilterMeshToMesh() {};
  FilterMeshToMesh(const FilterMeshToMesh&) {};
  void operator=(const FilterMeshToMesh&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterMeshToMesh.txx"
#endif

#endif
