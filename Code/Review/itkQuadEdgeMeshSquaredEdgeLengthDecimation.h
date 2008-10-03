/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshSquaredEdgeLengthDecimation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshSquaredEdgeLengthDecimation_h
#define __itkQuadEdgeMeshSquaredEdgeLengthDecimation_h

#include "itkQuadEdgeMeshEdgeMergeDecimationFilter.h"

namespace itk
{
/**
 * \class QuadEdgeMeshSquaredEdgeLengthDecimation
 * \brief
 */
template< class TInput, class TOutput, class TCriterion >
class QuadEdgeMeshSquaredEdgeLengthDecimation :
  public QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >
{
public:
  typedef QuadEdgeMeshSquaredEdgeLengthDecimation         Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef QuadEdgeMeshEdgeMergeDecimationFilter< 
    TInput, TOutput, TCriterion >                         Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshSquaredEdgeLengthDecimation, QuadEdgeMeshEdgeMergeDecimationFilter );

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  typedef TInput                                            InputMeshType;
  typedef typename InputMeshType::Pointer                   InputMeshPointer;

  typedef TOutput                                           OutputMeshType;
  typedef typename OutputMeshType::Pointer                  OutputMeshPointer;
  typedef typename OutputMeshType::PointIdentifier          OutputPointIdentifier;
  typedef typename OutputMeshType::PointType                OutputPointType;
  typedef typename OutputMeshType::QEType                   OutputQEType;
  typedef typename OutputMeshType::EdgeCellType             OutputEdgeCellType;
  typedef typename OutputMeshType::CellsContainerIterator   OutputCellsContainerIterator;

  typedef TCriterion                                        CriterionType;
  typedef typename CriterionType::MeasureType               MeasureType;

  typedef typename Superclass::PriorityType                 PriorityType;
  typedef typename Superclass::PriorityQueueItemType        PriorityQueueItemType;
  typedef typename Superclass::PriorityQueueType            PriorityQueueType;
  typedef typename Superclass::PriorityQueuePointer         PriorityQueuePointer;

  typedef typename Superclass::QueueMapType                 QueueMapType;
  typedef typename Superclass::QueueMapIterator             QueueMapIterator;

  typedef typename Superclass::OperatorType                 OperatorType;
  typedef typename Superclass::OperatorPointer              OperatorPointer;

protected:

  QuadEdgeMeshSquaredEdgeLengthDecimation();
  virtual ~QuadEdgeMeshSquaredEdgeLengthDecimation();

  /**
   * \brief Compute the measure value for iEdge
   * \param[in] iEdge
   * \return measure value, here the squared edge length
   */
  inline MeasureType MeasureEdge( OutputQEType* iEdge )
    {
    OutputMeshPointer output = this->GetOutput();

    OutputPointIdentifier id_org = iEdge->GetOrigin();
    OutputPointIdentifier id_dest = iEdge->GetDestination();

    OutputPointType org = output->GetPoint( id_org );
    OutputPointType dest = output->GetPoint( id_dest );

    return static_cast< MeasureType >( org.SquaredEuclideanDistanceTo( dest ) );
    }

  /**
   * \brief
   * \param[in]
   * \return
   */
  OutputPointType Relocate( OutputQEType* iEdge );

private:
  QuadEdgeMeshSquaredEdgeLengthDecimation( const Self& );
  void operator = ( const Self& );

};

}

#include "itkQuadEdgeMeshSquaredEdgeLengthDecimation.txx"
#endif
