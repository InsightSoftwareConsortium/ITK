/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMeshSource_h
#define __itkMeshSource_h

#include "itkProcessObject.h"

namespace itk
{

/** \class MeshSource
 *  \brief Base class for all process objects that output mesh data.
 *
 * MeshSource is the base class for all process objects that output
 * mesh data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output mesh. The class also defines
 * some internal private data memebers that are used to manage streaming
 * of data.
 */
template <class TOutputMesh>
class ITK_EXPORT MeshSource : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MeshSource         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(MeshSource,ProcessObject);

  /** 
   * Some typedefs.
   */
  typedef TOutputMesh OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** 
   * Get the mesh output of this process object. 
   */
  OutputMeshPointer GetOutput();
  OutputMeshPointer GetOutput(unsigned int idx);

  /** 
   * Set the mesh output of this process object. 
   */
  void SetOutput(TOutputMesh *output);

protected:
  MeshSource();
  virtual ~MeshSource() {}
  MeshSource(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  /**
   * Requested region of Mesh is specified as i of N unstructured regions.
   * Since all DataObjects should be able to set the requested region in 
   * unstructured form, just copy output->RequestedRegion all inputs.
   */
  void GenerateInputRequestedRegion();
  
private:
  /**
   * Used by streaming: The requested region of the output being processed
   * by the execute method. Set in the GenerateInputRequestedRegion method.
   */
  int m_GenerateDataRegion;
  int m_GenerateDataNumberOfRegions;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshSource.txx"
#endif

#endif
  
