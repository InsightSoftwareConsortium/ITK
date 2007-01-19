/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkConformalFlatteningFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConformalFlatteningFilter_h
#define __itkConformalFlatteningFilter_h

// ITK headers
#include "itkMeshToMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkConceptChecking.h"
#include "itkMesh.h"

// Standard headers
#include <assert.h>
#include <vector>

// vnl headers
#include <vcl_iostream.h>
#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_conjugate_gradient.h>


namespace itk
{

  /** \class ConformalFlatteningFilter
   * \brief 
   *
   * 
   * \ingroup MeshFilters
   */
  template <class TInputMesh, class TOutputMesh>
  class ITK_EXPORT ConformalFlatteningFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
  {
  public:
    /** Standard class typedefs. */
    typedef ConformalFlatteningFilter  Self;
    typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;
  
    typedef TInputMesh InputMeshType;
    typedef TOutputMesh OutputMeshType;
    typedef typename InputMeshType::Pointer InputMeshPointer;
    typedef typename OutputMeshType::Pointer OutputMeshPointer;

    /** Type for representing coordinates. */
    typedef typename TInputMesh::CoordRepType  CoordRepType;

    /** Method for creation through the object factory. */
    itkNewMacro(Self);
  
    /** Run-time type information (and related methods). */
    itkTypeMacro(ConformalFlatteningFilter, MeshToMeshFilter);

    /** Convenient constants obtained from TMeshTraits template parameter. */
    itkStaticConstMacro(PointDimension, unsigned int,
       ::itk::GetMeshDimension< TInputMesh >::PointDimension );

    ///////////////////////
    typedef typename InputMeshType::PointsContainer           PointsContainer;
    typedef typename InputMeshType::CellsContainer            CellsContainer;
    typedef typename PointsContainer::ConstIterator           PointIterator;
    typedef typename CellsContainer::ConstIterator            CellIterator;  
    typedef typename InputMeshType::CellType                  CellType;
    typedef typename CellType::PointIdIterator                PointIdIterator;
    typedef typename InputMeshType::PointType                 PointType;

    typedef vnl_vector< CoordRepType >                        Tvnl_vector;
    
    void setPointP( int );
    // The point P used to define the mapping is put according to the input of this function.
    
    void setScale( double );
    
    
    void mapToSphere( void );
    // Output a sphere, default choice.
    
    void mapToPlane( void );
    // Output a plane, i.e., no stereographic projection step.
    
#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(DimensionShouldBe3,
    (Concept::SameDimension<itkGetStaticConstMacro(PointDimension),3>));
  /** End concept checking */
#endif
 
  protected:
    ConformalFlatteningFilter();
    ~ConformalFlatteningFilter() {};
    void PrintSelf(std::ostream& os, Indent indent) const;
  
    /** Generate Requested Data */
    virtual void GenerateData( void );

  private:
    ConformalFlatteningFilter(const ConformalFlatteningFilter&); //purposely not implemented
    void operator=(const ConformalFlatteningFilter&); //purposely not implemented

    void mapping( InputMeshPointer inputMesh, OutputMeshPointer outputMesh);
    void stereographicProject( vnl_vector<CoordRepType> const& x,
                               vnl_vector<CoordRepType> const& y,
                               OutputMeshPointer outputMesh);
    void getDb(OutputMeshPointer mesh, 
               vnl_sparse_matrix<CoordRepType> &D,
               vnl_vector<CoordRepType> &bR,
               vnl_vector<CoordRepType> &bI);  

    vnl_vector<CoordRepType> solveLinearEq(vnl_sparse_matrix<CoordRepType> const& A, 
                                           vnl_vector<CoordRepType> const& b);
                                           
    unsigned int _cellHavePntP;
    // Store the cell Id in which the point P, which is used to define the mapping, lies in.
    
    bool _mapToSphere;
    // Whether the result is sphere or plane.  
    
    double _mapScale;
    // The scale when mapping to the plane. Determines how far the farthest point goes.           
  };

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConformalFlatteningFilter.txx"
#endif

#endif
