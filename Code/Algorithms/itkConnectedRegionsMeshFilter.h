/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedRegionsMeshFilter.h
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
#ifndef __itkConnectedRegionsMeshFilter_h
#define __itkConnectedRegionsMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
  
/** \class ConnectedRegionsMeshFilter
 * \brief Extract portions of a mesh that are connected at vertices.
 *
 * ConnectedRegionsMeshFilter will extract portions of a mesh that
 * are connected at vertices. (Such connected portions of the mesh
 * are referred to as a region.) Options exist to extract the largest
 * region, a particular region, a region containing a specified
 * point, or a region containing a specified cell.
 */

template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT ConnectedRegionsMeshFilter :
    public MeshToMeshFilter<TInputMesh,TOutputMesh> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ConnectedRegionsMeshFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh>   Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Different modes of operation. Use these to specify
   * how to extract the regions.
   */
  enum { PointSeededRegions = 0,
         CellSeededRegions = 1,
         SpecifiedRegions = 2,
         LargestRegion = 3,
         AllRegions = 4,
         ClosestPointRegion = 5 };
  
  /**
   * Methods specify mode of operation for the filter. Note that
   * some modes require additional information. For example, 
   * SetExtractionModeToClosestPointRegion() also requires that
   * a point be defined.
   */
  itkSetMacro(ExtractionMode,int);
  itkGetMacro(ExtractionMode,int);
  void SetExtractionModeToPointSeededRegions ()
    {this->SetExtractionMode(Self::PointSeededRegions);}
  void SetExtractionModeToCellSeededRegions ()
    {this->SetExtractionMode(Self::CellSeededRegions);}
  void SetExtractionModeToSpecifiedRegions ()
    {this->SetExtractionMode(Self::SpecifiedRegions);}
  void SetExtractionModeToLargestRegion ()
    {this->SetExtractionMode(Self::LargestRegion);}
  void SetExtractionModeToAllRegions ()
    {this->SetExtractionMode(Self::AllRegions);}
  void SetExtractionModeToClosestPointRegion ()
    {this->SetExtractionMode(Self::ClosestPointRegion);}

protected:
  ConnectedRegionsMeshFilter();
  virtual ~ConnectedRegionsMeshFilter() {};
  ConnectedRegionsMeshFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  virtual void GenerateData();

private:  
  int m_ExtractionMode;
  

}; // class declaration

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedRegionsMeshFilter.txx"
#endif

#endif
