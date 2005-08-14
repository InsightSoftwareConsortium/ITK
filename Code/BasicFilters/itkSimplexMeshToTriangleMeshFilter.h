/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshToTriangleMeshFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __SimplexMeshToTriangleMeshFilter_h
#define __SimplexMeshToTriangleMeshFilter_h

#include <itkMesh.h>
#include <itkLineCell.h>
#include <itkPolygonCell.h>
#include <itkVertexCell.h>
#include <itkMapContainer.h>

#include "itkSimplexMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"
#include "itkMapContainer.h"
#include "itkAutomaticTopologyMeshSource.h"

namespace itk
  {


  /**  \class SimplexMeshToTriangleMeshFilter
  * \brief This filter converts a 2-simplex mesh into a triangle mesh
  * 
  * Convert a simplex mesh into a triangle mesh. Therefore the center of each 
  * simplex cell is computed. These centers are taken as the points for the 
  * triangle mesh then the points are connected.
  *
  *
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  *
  */
  template <class TInputMesh, class TOutputMesh>
class SimplexMeshToTriangleMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
  {

  public:
    /** Standard "Self" typedef. */
    typedef SimplexMeshToTriangleMeshFilter  Self;

    /** Standard "Superclass" typedef. */
    typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

    /** Smart pointer typedef support */
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method of creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(SimplexMeshToTriangleMeshFilter,MeshToMeshFilter);


    typedef TInputMesh                                              InputMeshType;
    typedef typename InputMeshType::Pointer                         InputMeshPointer;
    typedef typename InputMeshType::PointType                       InputPointType;
    typedef typename InputMeshType::PixelType                       InputPixelType;
    typedef typename InputMeshType::MeshTraits::CellTraits          InputCellTraitsType;

    typedef typename InputMeshType::PointsContainer                 InputPointsContainer;
    typedef typename InputPointsContainer::Pointer                  InputPointsContainerPointer;
    typedef typename InputPointsContainer::Iterator                 InputPointsContainerIterator;

    typedef typename InputMeshType::NeighborListType                InputNeighbors;
    typedef typename InputMeshType::NeighborListType::iterator      InputNeighborsIterator;


    typedef          itk::AutomaticTopologyMeshSource<TOutputMesh>  AutoMeshSourceType;

    typedef typename InputMeshType::CellType                        SimplexCellType;
    typedef          itk::PolygonCell<SimplexCellType>              SimplexPolygonType;

    // stores the center for each simplex mesh cell, key is the point id
    typedef          itk::MapContainer<unsigned long, InputPointType> PointMapType;
    typedef typename PointMapType::Pointer                            PointMapPointer;


    /** 
    * This class provides methods for visiting 
    * each simplex cell of a simplex mesh
    * It computes the center of each visited cell.
    */
    class SimplexCellVisitor
      {

      public:

        /** 
        * default constructor
        */
        SimplexCellVisitor()
          {
          m_CenterMap = PointMapType::New();
          }

        /** 
        * \brief visits all polygon cells and compute the cell centers 
        */
        void Visit(unsigned long cellId, SimplexPolygonType * poly)
          {
          typedef typename SimplexPolygonType::PointIdIterator   PointIdIterator;
          PointIdIterator  it =  poly->PointIdsBegin();
          InputPointType center,p;
          center.Fill(0);

          while ( it != poly->PointIdsEnd() )
            {
            this->m_Mesh->GetPoint(*it, &p);
            center += p.GetVectorFromOrigin();
            it++;
            }

          center[0] /= poly->GetNumberOfPoints();
          center[1] /= poly->GetNumberOfPoints();
          center[2] /= poly->GetNumberOfPoints();

          m_CenterMap->InsertElement(cellId, center);


          //std::cout << "cellId: " << cellId << "  center = " << center << std::endl; 
          }

        PointMapPointer GetCenterMap()
          {
          return m_CenterMap;
          }

        void SetMesh(InputMeshPointer mesh)
          {
          this->m_Mesh = mesh;
          }

      protected:
        InputMeshPointer m_Mesh;
        PointMapPointer m_CenterMap; 
      };

    typedef itk::CellInterfaceVisitorImplementation<InputPixelType,
      InputCellTraitsType,
      SimplexPolygonType,
      SimplexCellVisitor>
      SimplexVisitorInterfaceType;

    typedef typename SimplexVisitorInterfaceType::Pointer  SimplexVisitorInterfacePointer;
    typedef typename SimplexCellType::MultiVisitor         CellMultiVisitorType;
    typedef typename CellMultiVisitorType::Pointer         CellMultiVisitorPointer;


  protected:

    SimplexMeshToTriangleMeshFilter();

    ~SimplexMeshToTriangleMeshFilter();

    SimplexMeshToTriangleMeshFilter(const Self&) {}

  void operator=(const Self&) {}

  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

  void Initialize();

  /** creates dual triangles for all simplex cells */
  void CreateTriangles();

  /** part of algorithm */
  unsigned long FindCellId(unsigned long id1, unsigned long id2, unsigned long id3);

  /** attribute stores the result of the simplex cell visitor */
  PointMapPointer m_Centers;

};

} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshToTriangleMeshFilter.txx"
#endif

#endif //__SimplexMeshToTriangleMeshFilter_h
