/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshVolumeCalculator.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __SimplexMeshVolumeCalculator_h
#define __SimplexMeshVolumeCalculator_h

#include <itkMesh.h>
#include <itkLineCell.h>
#include <itkPolygonCell.h>
#include <itkVertexCell.h>
#include <itkMapContainer.h>
#include "itkCovariantVector.h"
#include "itkVector.h"
#include "itkSimplexMesh.h"
#include "itkVectorContainer.h"

namespace itk
  {


  /**  \class SimplexMeshVolumeCalculator
  * \brief
  * 
  * Adapted from itkSimplexMeshToTriangleFilter to calculate the volume of
  * a simplex mesh using the barycenters and normals.
  * call Compute() to caluclate the volume and GetVolume() to get the
  * value. For an example see itkDeformableSimplexMesh3DFilter.cxx
  * (Thomas Boettger. Division Medical and Biological Informatics, 
  *  German Cancer Research Center, Heidelberg.)
  * \author Leila Baghdadi MICe, Hospital for Sick Children, Toronto, Canada.
  *
  */
template <class TInputMesh>
class ITK_EXPORT SimplexMeshVolumeCalculator : public Object
  {

  public:
    /** Standard "Self" typedef. */
    typedef SimplexMeshVolumeCalculator  Self;

    /** Standard "Superclass" typedef. */
    typedef Object Superclass;

    /** Smart pointer typedef support */
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method of creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(Self, Object);

    typedef TInputMesh                                              InputMeshType;
    typedef typename InputMeshType::Pointer                         InputMeshPointer;
    typedef typename InputMeshType::ConstPointer                    InputMeshConstPointer;
 
    typedef typename InputMeshType::PointType                       InputPointType;
    typedef typename InputMeshType::PixelType                       InputPixelType;
    typedef typename InputMeshType::MeshTraits::CellTraits          InputCellTraitsType;

    typedef typename InputMeshType::PointsContainer                 InputPointsContainer;
    typedef typename InputPointsContainer::ConstPointer             InputPointsContainerPointer;
    typedef typename InputPointsContainer::ConstIterator            InputPointsContainerIterator;

    typedef typename InputMeshType::NeighborListType                InputNeighbors;
    typedef typename InputMeshType::NeighborListType::iterator      InputNeighborsIterator;

    typedef typename InputMeshType::CellType                        SimplexCellType;
    typedef          itk::PolygonCell<SimplexCellType>              SimplexPolygonType;

    // stores the center for each simplex mesh cell, key is the point id
    typedef          itk::MapContainer<unsigned long, InputPointType> PointMapType;
    typedef typename PointMapType::Pointer                            PointMapPointer;

    typedef typename InputPointType::VectorType                VectorType; 
    typedef CovariantVector< 
                    typename VectorType::ValueType, 3 >   CovariantVectorType;
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
       m_Mesh->GetPoint(*it, &p);
        //m_SimplexMesh->GetPoint(*it, &p);
            center += p.GetVectorFromOrigin();
            it++;
            }

          center[0] /= poly->GetNumberOfPoints();
          center[1] /= poly->GetNumberOfPoints();
          center[2] /= poly->GetNumberOfPoints();

          m_CenterMap->InsertElement(cellId, center);


          std::cout << "cellId: " << cellId << "  center = " << center << std::endl; 
          }

        PointMapPointer GetCenterMap()
          {
          return m_CenterMap;
          }
  
        void SetMesh(InputMeshPointer mesh)
          {
          m_Mesh = mesh;
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
    
    /** Set the input mesh. */
    itkSetObjectMacro(SimplexMesh, InputMeshType);

    /** Compute the volume of the entire simplex mesh. */
    void Compute(void);
    
    /** Return the minimum intensity value. */
    itkGetMacro(Volume, double);

    

  protected:
    SimplexMeshVolumeCalculator();
    virtual ~SimplexMeshVolumeCalculator();
    void PrintSelf(std::ostream& os, Indent indent) const;

    void Initialize();

    /** creates dual triangles for all simplex cells */
    void CreateTriangles();

    void CalculateTriangleVolume(double normal_z,InputPointType p1, InputPointType p2, InputPointType p3);

    /** part of algorithm */
    unsigned long FindCellId(unsigned long id1, unsigned long id2, unsigned long id3);

    /** attribute stores the result of the simplex cell visitor */
    PointMapPointer m_Centers;

    double m_Volume;

  private:
    SimplexMeshVolumeCalculator(const Self&); //purposely not implemented
    void operator=(const Self&); //purposely not implemented
    
    InputMeshPointer  m_SimplexMesh;

};

} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshVolumeCalculator.txx"
#endif

#endif /* __SimplexMeshVolumeCalculator_h */
