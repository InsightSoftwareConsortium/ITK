/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDecimationFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshDecimationFilter_h
#define __itkQuadEdgeMeshDecimationFilter_h

#include <itkQuadEdgeMeshToQuadEdgeMeshFilter.h>

namespace itk
{
/**
 * \class QuadEdgeMeshDecimationFilter
 * \brief
 */
template< class TInput, class TOutput, class TCriterion >
class QuadEdgeMeshDecimationFilter :
      public QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput >
{
public:
  typedef QuadEdgeMeshDecimationFilter                          Self;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput >   Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshDecimationFilter, QuadEdgeMeshToQuadEdgeMeshFilter );

  typedef TInput                                                InputMeshType;
  typedef typename InputMeshType::Pointer                       InputMeshPointer;

  typedef TOutput                                               OutputMeshType;
  typedef typename OutputMeshType::Pointer                      OutputMeshPointer;

  typedef TCriterion                                            CriterionType;
  typedef typename CriterionType::Pointer                       CriterionPointer;
  typedef typename CriterionType::MeasureType                   MeasureType;
  typedef typename CriterionType::PriorityType                  PriorityType;
  typedef typename CriterionType::PriorityQueueWrapperType      PriorityQueueItemType;

  itkSetObjectMacro( Criterion, CriterionType );

protected:
  QuadEdgeMeshDecimationFilter()
    {
    this->m_Iteration = 0;
    }

  ~QuadEdgeMeshDecimationFilter() {}

  CriterionPointer      m_Criterion;
  unsigned long         m_Iteration;

  void GenerateData()
    {
    this->CopyInputMeshToOutputMesh();

    Initialize();
    FillPriorityQueue();
    m_Iteration = 0;

    do
      {
      this->Extract();

      if ( ProcessWithTopologicalGuarantee() )
        {
        return;
        }

      ++m_Iteration;
      } while ( !IsCriterionSatisfied() );

    this->GetOutput()->SqueezePointsIds( );
    }

  virtual void Initialize() {}
  virtual void FillPriorityQueue() = 0;
  virtual void Extract() = 0;
  virtual bool ProcessWithoutAnyTopologicalGuarantee() = 0;
  virtual bool ProcessWithTopologicalGuarantee() = 0;
  virtual bool IsCriterionSatisfied() = 0;

  void PrintSelf( std::ostream& os, Indent indent ) const
    {
    this->Superclass::PrintSelf( os, indent );
    }

private:
  QuadEdgeMeshDecimationFilter( const Self& );
  void operator = ( const Self& );
};
}

#endif
