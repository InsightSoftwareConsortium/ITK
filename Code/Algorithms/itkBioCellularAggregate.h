/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCellularAggregate.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioCellularAggregate_h
#define __itkBioCellularAggregate_h

#include "itkBioCellularAggregateBase.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkImage.h"
#include "itkBioCell.h"
#include "itkPolygonCell.h"

#include <iostream>
#include <vector>


namespace itk {

namespace bio {

/** \class CellularAggregate
 * \brief This class represent an aggregation of bio::Cell objects
 * This class is the base for different types of cellular groups
 * including bacterial colonies and pluricellular organisms 
 */
template<unsigned int NSpaceDimension=3>
class CellularAggregate : public CellularAggregateBase
{
public:
  /** Standard class typedefs. */
  typedef CellularAggregate      Self;
  typedef CellularAggregateBase  Superclass;
  typedef itk::SmartPointer<Self>        Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;

  /*** Run-time type information (and related methods). */
  itkTypeMacro(CellularAggregate, CellularAggregateBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  itkStaticConstMacro( SpaceDimension, unsigned int, NSpaceDimension);

  /*** Type to be used for data associated with each point in the mesh. */
  typedef    Cell<NSpaceDimension>      BioCellType;
  typedef    BioCellType *              PointPixelType;
  typedef    double                     CellPixelType;


  /** Mesh Traits */
  typedef itk::DefaultDynamicMeshTraits<  
              PointPixelType,           // PixelType
              NSpaceDimension,           // Points Dimension
              NSpaceDimension,           // Max.Topological Dimension
              double,                   // Type for coordinates
              double,                   // Type for interpolation 
              CellPixelType             // Type for values in the cells  
              >  MeshTraits;
  
  /** Mesh Traits */
  typedef itk::Mesh<  PointPixelType,
                      NSpaceDimension,
                      MeshTraits  >               MeshType;

  /** Mesh Associated types */
  typedef typename MeshType::Pointer                       MeshPointer;
  typedef typename MeshType::ConstPointer                  MeshConstPointer;
  typedef typename MeshType::PointType                     PointType;
  typedef typename BioCellType::VectorType                 VectorType;


  typedef typename MeshType::PointsContainer               PointsContainer;
  typedef typename MeshType::PointDataContainer            PointDataContainer;
  typedef typename MeshType::CellsContainer                VoronoiRegionsContainer;
  typedef typename PointsContainer::Iterator               PointsIterator;
  typedef typename PointDataContainer::Iterator            CellsIterator;
  typedef typename VoronoiRegionsContainer::Iterator       VoronoiIterator;
  typedef typename PointsContainer::ConstIterator          PointsConstIterator;
  typedef typename PointDataContainer::ConstIterator       CellsConstIterator;
  typedef typename VoronoiRegionsContainer::ConstIterator  VoronoiConstIterator;
  typedef typename MeshType::PointIdentifier               IdentifierType;
  typedef typename MeshType::CellAutoPointer               CellAutoPointer;

  /**   Voronoi region around a bio::Cell */
  typedef itk::CellInterface<  
                     typename MeshType::CellPixelType, 
                     typename MeshType::CellTraits >      CellInterfaceType;
  typedef itk::PolygonCell<  CellInterfaceType >          VoronoiRegionType;
  typedef typename VoronoiRegionType::SelfAutoPointer     VoronoiRegionAutoPointer;

  /** Convenient typedefs. */
  typedef float                                        ImagePixelType;
  typedef itk::Image<ImagePixelType, NSpaceDimension > SubstrateType;
  typedef typename SubstrateType::Pointer              SubstratePointer;
  typedef ImagePixelType                               SubstrateValueType;
  typedef std::vector< SubstratePointer >              SubstratesVector;

public:
  unsigned int GetNumberOfCells(void) const;
 
  static unsigned int GetDimension() { return SpaceDimension; }
    
  void SetGrowthRadiusLimit( double value );
  void SetGrowthRadiusIncrement( double value );
  
  itkGetObjectMacro( Mesh, MeshType );
  itkGetConstObjectMacro( Mesh, MeshType );

  virtual void AdvanceTimeStep(void);

  virtual void SetEgg( BioCellType * cell, const PointType & position );
  virtual void Add( CellBase * cell );
  virtual void Add( CellBase * cell, const VectorType & perturbation );
  virtual void Add( CellBase * cellA, CellBase * cellB, double perturbationLength );
  virtual void Remove( CellBase * cell );
  
  virtual void GetVoronoi( unsigned long int cellId, VoronoiRegionAutoPointer & ) const;

  void DumpContent( std::ostream & os ) const;

  virtual void AddSubstrate( SubstrateType * substrate );
  virtual SubstratesVector & GetSubstrates( void );
  virtual SubstrateValueType GetSubstrateValue( unsigned long int cellId,
                                                unsigned int substrateId ) const;

  virtual void KillAll(void);


protected:
  CellularAggregate();
  virtual ~CellularAggregate();
  CellularAggregate( const Self & );
  void operator=(const Self&);
  void PrintSelf(std::ostream& os, itk::Indent indent) const;

  virtual void ComputeForces(void);
  virtual void UpdatePositions(void);
  virtual void ComputeClosestPoints(void);
  virtual void ClearForces(void);
  
private:
 
  MeshPointer           m_Mesh;
  SubstratesVector      m_Substrates;
  double                m_FrictionForce;
  unsigned long         m_Iteration;
  unsigned long         m_ClosestPointComputationInterval;

};

} // end namespace bio

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBioCellularAggregate.txx"
#endif

#endif

