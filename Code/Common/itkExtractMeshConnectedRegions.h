/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractMeshConnectedRegions.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkExtractMeshConnectedRegions_h
#define __itkExtractMeshConnectedRegions_h

#include "itkFilterMeshToMesh.h"

namespace itk
{
  
/** \class ExtractMeshConnectedRegions
 * \brief Extract portions of a mesh that are connected at vertices.
 *
 * ExtractMeshConnectedRegions will extract portions of a mesh that
 * are connected at vertices. (Such connected portions of the mesh
 * are referred to as a region.) Options exist to extract the largest
 * region, a particular region, a region containing a specified
 * point, or a region containing a specified cell.
 */

template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT ExtractMeshConnectedRegions :
    public FilterMeshToMesh<TInputMesh,TOutputMesh> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ExtractMeshConnectedRegions  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterMeshToMesh<TInputMesh,TOutputMesh>   Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;

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
  ExtractMeshConnectedRegions();
  virtual ~ExtractMeshConnectedRegions() {};
  ExtractMeshConnectedRegions(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  virtual void GenerateData();

private:  
  int m_ExtractionMode;
  

}; // class declaration

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractMeshConnectedRegions.txx"
#endif

#endif
