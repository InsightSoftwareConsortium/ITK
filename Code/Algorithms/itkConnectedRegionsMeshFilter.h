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
   * Special typedefs for this filter
   */
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  enum {PointDimension = TInputMesh::PointDimension};
  typedef typename TInputMesh::PointType PointType;

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

  /**
   * Initialize list of point ids/cell ids used to seed regions.
   */
  void InitializeSeedList()
    {this->Modified(); m_SeedList.clear();}

  /**
   * Add a seed id (point or cell id). Note: ids are 0-offset.
   */
  void AddSeed(unsigned long id)
    {this->Modified(); m_SeedList.push_back(id);}

  /**
   * Delete a seed id (point or cell id). Note: ids are 0-offset.
   */
  void DeleteSeed(unsigned long id);

  /**
   * Initialize list of region ids to extract.
   */
  void InitializeSpecifiedRegionList()
    {this->Modified(); m_RegionList.clear();}

  /**
   * Add a region id to extract. Note: ids are 0-offset.
   */
  void AddSpecifiedRegion(unsigned long id)
    {this->Modified(); m_RegionList.push_back(id);}

  /**
   * Delete a region id to extract. Note: ids are 0-offset.
   */
  void DeleteSpecifiedRegion(unsigned long id);

  /**
   * Use to specify x-y-z point coordinates when extracting the region 
   * closest to a specified point.
   */
  void SetClosestPoint(PointType& p)
    {
      if ( m_ClosestPoint != p )
        {
        m_ClosestPoint = p;
        this->Modified();
        }
    }
  PointType& GetClosestPoint(PointType& p) const
    {return m_ClosestPoint;}

  /**
   * Obtain the number of connected regions.
   */
  unsigned long GetNumberOfExtractedRegions();

protected:
  ConnectedRegionsMeshFilter();
  virtual ~ConnectedRegionsMeshFilter() {};
  ConnectedRegionsMeshFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:  
  int                        m_ExtractionMode;
  PointType                  m_ClosestPoint;
  std::vector<unsigned long> m_SeedList;
  std::vector<unsigned long> m_RegionList;
  std::vector<unsigned long> m_RegionSizes;
  
}; // class declaration

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedRegionsMeshFilter.txx"
#endif

#endif
