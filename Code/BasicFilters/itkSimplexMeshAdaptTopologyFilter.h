/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshAdaptTopologyFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimplexMeshAdaptTopologyFilter_h
#define __itkSimplexMeshAdaptTopologyFilter_h

#include "itkMesh.h"
#include "itkPolygonCell.h"
#include "itkMapContainer.h"
#include "itkCellInterfaceVisitor.h"

#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL > 20040406
# include <vnl/vnl_cross.h>
# define itk_cross_3d vnl_cross_3d
#else
# define itk_cross_3d cross_3d
#endif

namespace itk
  {
  /** \class SimplexMeshAdaptTopologyFilter
  *  \brief This filter changes the topology of a 2-simplex mesh
  * 
  * Currently only one transformation for inserting new cells into a mesh is implemented.
  * For insertion several criteria are compute, e.g. the curvature in a mesh point. The user 
  * can set a threshold value to control how many cells will be manipulated.
  *
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  * 
  */
  template <class TInputMesh, class TOutputMesh>
class SimplexMeshAdaptTopologyFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
  {

  public:
    /** Standard "Self" typedef. */
    typedef SimplexMeshAdaptTopologyFilter Self;

    /** Standard "Superclass" typedef. */
    typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

    /** Smart pointer typedef support */
    typedef SmartPointer<Self>  Pointer;
    /** Smart pointer typedef support */
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method of creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(SimplexMeshAdaptTopologyFilter,MeshToMeshFilter);

    typedef TInputMesh                                              InputMeshType;
    typedef typename InputMeshType::Pointer                         InputMeshPointer;
    typedef typename InputMeshType::PointType                       InputPointType;
    typedef typename InputMeshType::VectorType                      InputVectorType;
    typedef typename InputMeshType::PixelType                       InputPixelType;
    typedef typename InputMeshType::MeshTraits::CellTraits          InputCellTraitsType;
    typedef typename InputMeshType::CellType                        InputCellType;
    typedef typename InputCellType::PointIdIterator                 InputCellPointIdIterator;
    typedef typename InputCellType::CellAutoPointer                 InputCellAutoPointer;
    typedef typename InputMeshType::CellAutoPointer                 CellAutoPointer;
    typedef          itk::PolygonCell<InputCellType>                InputPolygonType;
    typedef typename InputPolygonType::PointIdIterator              InputPolygonPointIdIterator;


    typedef  TOutputMesh                                            OutputMeshType;
    typedef typename OutputMeshType::Pointer                        OutputMeshPointer;
    typedef typename OutputMeshType::CellType                       OutputCellType;
    typedef          itk::PolygonCell<OutputCellType>               OutputPolygonType;

    typedef          itk::MapContainer<unsigned long, double>       DoubleValueMapType;
    typedef typename DoubleValueMapType::Iterator                   DoubleContainerIterator;


    /** 
    * class for visiting all polygonal cells. 
    * The visitor computes the area and curvature
    * of each cell and stores them in the area
    * map.
    */
    class SimplexCellVisitor
      {

      public:
        InputMeshPointer mesh;
        double totalArea;
        double totalCurvature;
        double minCellSize;
        double maxCellSize;
        DoubleValueMapType::Pointer areaMap;
        DoubleValueMapType::Pointer curvatureMap;

        double minCurvature;
        double maxCurvature;

        SimplexCellVisitor()
          {
          areaMap = DoubleValueMapType::New();
          curvatureMap = DoubleValueMapType::New();
          totalArea = 0;
          totalCurvature = 0;
          minCellSize = NumericTraits<double>::max();
          maxCellSize = 0;
          minCurvature = NumericTraits<double>::max();
          maxCurvature = 0;
          }


        /** \brief visits all polygon cells and computes the area, 
        *  NOTE: works for convex polygons only!!!
        */
        void Visit(unsigned long cellId, InputPolygonType * poly)
          {
          typename InputPolygonType::PointIdIterator it =  poly->PointIdsBegin();

          double meanCurvature = 0;
          unsigned long refPoint = *it;
          double val = mesh->GetMeanCurvature(*it++);
          meanCurvature += vcl_abs(val);

          unsigned long id1 = *it;
          val = mesh->GetMeanCurvature(*it++);
          meanCurvature += vcl_abs(val);

          unsigned long id2;

          double area = 0;

          int cnt = 0; 

          while ( it != poly->PointIdsEnd() )
            {
            id2 = *it;
            area += ComputeArea(refPoint,id1,id2);
            id1 = id2;
            val = mesh->GetMeanCurvature(*it);
            meanCurvature += vcl_abs(val);
            cnt++;
            it++;
            }

          meanCurvature /= (double)cnt;
          totalArea += area;
          totalCurvature += meanCurvature;

          areaMap->InsertElement(cellId, area);
          curvatureMap->InsertElement(cellId, meanCurvature);

          if (area > maxCellSize ) maxCellSize = area;
          if (area < minCellSize ) minCellSize = area;
          if (meanCurvature > maxCurvature ) maxCurvature = meanCurvature;
          if (meanCurvature < minCurvature ) minCurvature = meanCurvature;
          }

        double ComputeArea(unsigned long p1,unsigned long p2, unsigned long p3)
          {
          InputPointType v1,v2,v3;
          mesh->GetPoint(p1, &v1);
          mesh->GetPoint(p2, &v2);
          mesh->GetPoint(p3, &v3);
          return vcl_abs (itk_cross_3d((v2-v1).GetVnlVector(), (v3-v1).GetVnlVector()).two_norm() /2.0);
          }

        DoubleValueMapType::Pointer GetAreaMap()
          {
          return areaMap;
          }

        DoubleValueMapType::Pointer GetCurvatureMap()
          {
          return curvatureMap;
          }

        double GetTotalMeshArea()
          {
          return totalArea;
          }

        double GetTotalMeanCurvature()
          {
          return totalCurvature/(curvatureMap->Size());
          }

        double GetMaximumCellSize()
          {
          return maxCellSize;
          }

        double GetMinimumCellSize()
          {
          return minCellSize;
          }
        double GetMaximumCurvature()
          {
          return maxCurvature;
          }

        double GetMinimumCurvature()
          {
          return minCurvature;
          }

      };

    // cell visitor stuff
    typedef itk::CellInterfaceVisitorImplementation<InputPixelType,
      InputCellTraitsType,
      InputPolygonType,
      SimplexCellVisitor>
      SimplexVisitorInterfaceType;

    typedef typename SimplexVisitorInterfaceType::Pointer   SimplexVisitorInterfacePointer;
    typedef typename InputCellType::MultiVisitor            CellMultiVisitorType;
    typedef typename CellMultiVisitorType::Pointer          CellMultiVisitorPointer;


    itkSetMacro(Threshold, double);
    itkGetMacro(Threshold, double);

    itkSetMacro(SelectionMethod, int);
    itkGetMacro(SelectionMethod, int);

    itkGetMacro(ModifiedCount, int);


  protected:

    SimplexMeshAdaptTopologyFilter();

    ~SimplexMeshAdaptTopologyFilter();

    SimplexMeshAdaptTopologyFilter(const Self&) 
      {
      }

    void operator=(const Self&)
      {
      }

    void PrintSelf(std::ostream& os, Indent indent) const;

    virtual void GenerateData();


    /**
    * Initialize this filters containers
    */
    void Initialize();

    /**
    * Method computes and evaluates cell propeties, 
    * like area and curvature and determines whether 
    * a cell should be refined or not.
    */ 
    void ComputeCellParameters();

    /**
    * Create new cells 
    */
    void InsertNewCells();

    /**
    * Update topology neighbor relations for all cells
    * which are were influenced by he insertion of new
    * points. 
    */
    void ModifyNeighborCells(unsigned long id1, unsigned long id2, unsigned long insertPointId);


    /**
    * Compute the center of a cell
    */
    InputPointType ComputeCellCenter(InputCellAutoPointer &simplexCell);


    /**
    * class member stoing cell id offset
    */
    unsigned long m_IdOffset;


    /**
    * threshold controls the percentage of cells
    * to satify the selection criteria
    */
    double m_Threshold;

    /**
    * different criteria for cell refinement selection
    */
    int  m_SelectionMethod;

    /** 
    * atttribute contains the number of cells
    * which were modified during the last Update()
    */
    int m_ModifiedCount;

    /**
    * \brief member for accessing the filter result during
    *  creation
    */
    OutputMeshPointer m_Output;

    InputCellAutoPointer  NewSimplexCellPointer;

  };

  } //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshAdaptTopologyFilter.txx"
#endif

#endif // __itkSimplexMeshAdaptTopologyFilter_h

