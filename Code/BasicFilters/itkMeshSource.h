/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSource.h
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
 * some internal private data members that are used to manage streaming
 * of data.
 *
 * \ingroup DataSources
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
  void PrintSelf(std::ostream& os, Indent indent) const;
  
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
  
