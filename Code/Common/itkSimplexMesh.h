/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMesh.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimplexMesh_h
#define __itkSimplexMesh_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkVertexCell.h"
#include "itkTriangleCell.h"
#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include "itkFixedArray.h"
#include "itkNumericTraits.h"
#include <itkCovariantVector.h>
#include <vector>
#include <algorithm>
#include <set>

namespace itk
  {
  /** \class SimplexMesh
  * \brief The class represents a 2-simplex mesh. 
  *
  * A simplex mesh can be used for deformable model segmentation of 3D image data.
  * To create a simplex mesh one needs a triangle mesh, which can be converted 
  * to using the class itkTriangleMeshToSimplexMeshFilter. The back filtering 
  * (from simplex to trinagle mesh)is done through a itkSimplexMeshToTriangleMeshFilter.
  * 
  *
  *
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  */
  template <typename TPixelType, unsigned int VDimension = 3,
    typename TMeshTraits = DefaultStaticMeshTraits< TPixelType , VDimension, VDimension, TPixelType , TPixelType , TPixelType >
  >
class SimplexMesh : public Mesh<TPixelType, VDimension, TMeshTraits>
  {
  public:
    /** Standard typedefs. */
    typedef SimplexMesh                Self;

    /** Standard typedefs. */
    typedef Mesh<TPixelType, VDimension, TMeshTraits>  Superclass;

    /** Standard typedefs. */
    typedef SmartPointer<Self>  Pointer;

    /** Standard typedefs. */
    typedef SmartPointer<const Self>  ConstPointer;

    /** definition for array of indices*/
    typedef typename SimplexMeshGeometry::IndexArray            IndexArray;

    /** definition for a set of neighbor indices */
    typedef std::set<unsigned long>                             NeighborSetType;

    /** */
    typedef typename NeighborSetType::iterator                  NeighborSetIterator;

    /** */
    typedef std::vector<unsigned long>                          NeighborListType;

    /** */
    typedef typename TMeshTraits::PointType                     PointType;

    /** */
    typedef typename TMeshTraits::PointIdentifier               PointIdentifier;

    /** */
    typedef typename PointType::VectorType VectorType;

    /** */ 
    typedef CovariantVector<typename VectorType::ValueType, 3>  CovariantVectorType;


    /** */
    typedef typename Superclass::CellType                       CellType;

    /** */
    typedef typename CellType::CellAutoPointer                  CellAutoPointer;
    /** */
    typedef itk::LineCell<CellType>                             LineType;

    /** map containing a SimplexMeshGeometry data object for each mesh point*/
    typedef itk::MapContainer<unsigned long, SimplexMeshGeometry *>   GeometryMapType;

    /** smartpointer def for the geometry map */
    typedef typename GeometryMapType::Pointer GeometryMapPointer;

    /** iterator definition for iterating over a geometry map */
    typedef typename GeometryMapType::Iterator GeometryMapIterator;


    /** Method for creation through the object factory. */
    itkNewMacro(Self);

    /** Standard part of every itk Object. */
    itkTypeMacro(SimplexMesh, Mesh);

    /** Hold on to the type information specified by the template parameters. */
    typedef TMeshTraits                                          MeshTraits;
    typedef typename MeshTraits::PixelType                       PixelType;  
    typedef typename MeshTraits::PointsContainer                 PointsContainer;
    typedef typename Superclass::PointsContainerPointer          PointsContainerPointer;
    typedef typename Superclass::PointsContainer::Iterator       PointsContainerIterator;
    typedef typename Superclass::PointsContainerConstIterator    PointsContainerConstIterator;
    typedef typename Superclass::CellsContainerPointer           CellsContainerPointer;
    typedef typename Superclass::CellsContainerIterator          CellsContainerIterator;
    /** set the map of geometrydata to the new pointer */
    itkSetMacro(GeometryData, GeometryMapPointer );

    /** returns the current map of geometrydata */
    itkGetConstReferenceMacro(GeometryData, GeometryMapPointer );

    /** Get the first free id for new cells*/
    itkSetMacro(LastCellId, unsigned long);

    /** Set the id value valid for new cells */
    itkGetMacro(LastCellId, unsigned long);

    /**
    * copy all necessary information from passed object
    * to the mesh
    */
    virtual void CopyInformation(const DataObject *data);


    /**
    * Add a new edge to the simplex mesh by specifying the ids of the start 
    * and end point of the edge
    * Note: This can destroy the simplex mesh structure! Better use the 
    * simplex mesh modification or creation filters
    */
    unsigned long AddEdge(unsigned long startPointId, unsigned long endPointId);


    /**
    * Add a new simplex mesh cell to the mesh by passing an AutoPointer of a 
    * previously created simplex mesh cell
    * 
    * Note: This can destroy the simplex mesh structure! You should use the 
    * simplex mesh modification or creation filters.
    */
    unsigned long AddFace(CellAutoPointer &cellPointer);

    /**
    * Replaces the cell specified by replaceIndex with the new cell passed by its
    * AutoPopinter
    */
    unsigned long ReplaceFace(unsigned long replaceIndex, CellAutoPointer &cellPointer);

    /** 
    * Get the three direct neighbors of a point 
    */  
    IndexArray GetNeighbors(unsigned long pointId) const;

    /** 
    * Get all neighbor points with a specified radius  
    */  
    NeighborListType* GetNeighbors(unsigned long pointId, unsigned int radius, NeighborListType* list = NULL) const;

    /** 
    * Add a neighbor to a point. 
    * Note: This can destroy the simplex mesh topology! 
    * Better use te simplex mesh creation filters.
    */  
    void AddNeighbor(unsigned long pointId, unsigned long neighborId);

    /** 
    * Replace a neighbor of a specific point by a new one     
    */  
    void ReplaceNeighbor(unsigned long pointId, unsigned long oldNeighborId,unsigned long newNeighborIdx);

    /** 
    * Swap the order of two neighbors  
    */
    void SwapNeighbors(unsigned long pointId, unsigned long firstNeighborId,unsigned long secondNeighborId);

    /**
    * Set the geometry data for a specified point
    */
    void SetGeometryData(unsigned long pointId, SimplexMeshGeometry*);

    /**
    * Set the geometry data for a specified point
    */
    void SetBarycentricCoordinates(unsigned long idx, PointType values);

    /**
    * Set the barycentric coordinates for a specified point
    */
    PointType GetBarycentricCoordinates(unsigned long idx) const;

    /**
    * Set the reference metrics for a specified point
    */
    void SetReferenceMetrics(unsigned long idx, PointType values);

    /**
    *  Return the reference metrics for the specified point
    */
    PointType GetReferenceMetrics(unsigned long idx) const;

    /**
    * Set the simplex angle for the specified point
    */
    void SetPhi(unsigned long idx, double values);

    /**
    * Get the simplex angle for the specified point
    */
    double GetPhi(unsigned long idx) const;

    /**
    * Set the mean curvature for the specified point
    */
    void SetMeanCurvature(unsigned long idx, double values);

    /**
    * Get the mean curvature for the specified point
    */
    double GetMeanCurvature(unsigned long idx) const;

    /**
    * Set the circum circles radius for the specified point
    */
    void SetRadius(unsigned long idx, double values);

    /**
    * Get the circum circles radius for the specified point
    */
    double GetRadius(unsigned long idx) const;

    /**
    * Set the distance to the foot point for the specified point
    */
    void SetDistance(unsigned long idx, double values);

    /**
    * Get the distance to the foot point for the specified point
    */
    double GetDistance(unsigned long idx) const;

    /** compute the normal vector in the specified mesh point */
    CovariantVectorType ComputeNormal(unsigned long idx ) const;

  protected:
    //  /** Constructor for use by New() method. */
    SimplexMesh();
    virtual ~SimplexMesh();
    void PrintSelf(std::ostream& os, Indent indent) const;

    /** 
    * The map stores a SimplexMeshGeometry object for each mesh point
    */
    GeometryMapPointer m_GeometryData;

    /*  */
    /**
    * The last cell id is the index which is used for insertion of new
    * cells. It increases during mesh creation. This is done because
    * one cannot rely on the size of the map or the highest index when 
    * cells are removed.
    */
    unsigned long m_LastCellId;


  private:
    SimplexMesh(const Self&); //purposely not implemented
    //  void operator=(const Self&); //purposely not implemented



  }; // End Class:  SimplexMesh

  } // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMesh.txx"
#endif

#endif
