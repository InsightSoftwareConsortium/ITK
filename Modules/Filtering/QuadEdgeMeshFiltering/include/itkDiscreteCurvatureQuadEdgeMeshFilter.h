/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkDiscreteCurvatureQuadEdgeMeshFilter_h
#define __itkDiscreteCurvatureQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkConceptChecking.h"
#include "itkTriangleHelper.h"

namespace itk
{
/**
 * \class DiscreteCurvatureQuadEdgeMeshFilter
 *
 * \brief FIXME
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< class TInputMesh, class TOutputMesh=TInputMesh >
class ITK_EXPORT DiscreteCurvatureQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef DiscreteCurvatureQuadEdgeMeshFilter                         Self;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh > Superclass;

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutputMesh                                      OutputMeshType;
  typedef typename OutputMeshType::Pointer                 OutputMeshPointer;
  typedef typename OutputMeshType::PointsContainerPointer  OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator OutputPointsContainerIterator;
  typedef typename OutputMeshType::PointType               OutputPointType;
  typedef typename OutputPointType::CoordRepType           OutputCoordType;
  typedef typename OutputMeshType::PointIdentifier         OutputPointIdentifier;
  typedef typename OutputMeshType::CellIdentifier          OutputCellIdentifier;
  typedef typename OutputMeshType::QEType                  OutputQEType;
  typedef typename OutputMeshType::MeshTraits              OutputMeshTraits;
  typedef typename OutputMeshTraits::PixelType             OutputCurvatureType;

  typedef TriangleHelper< OutputPointType > TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteCurvatureQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputCurvatureType > ) );
  /** End concept checking */
#endif

protected:
  DiscreteCurvatureQuadEdgeMeshFilter() {}
  ~DiscreteCurvatureQuadEdgeMeshFilter() {}

  virtual OutputCurvatureType EstimateCurvature(const OutputPointType & iP) = 0;

  OutputCurvatureType ComputeMixedArea(OutputQEType *iQE1, OutputQEType *iQE2)
  {
    OutputMeshPointer output = this->GetOutput();

    OutputPointIdentifier id[3];

    id[0] = iQE1->GetOrigin();
    id[1] = iQE1->GetDestination();
    id[2] = iQE2->GetDestination();

    OutputPointType p[3];

    for ( int i = 0; i < 3; i++ )
      {
      p[i] = output->GetPoint(id[i]);
      }

    if ( !TriangleType::IsObtuse(p[0], p[1], p[2]) )
      {
      OutputCurvatureType sq_d01 =
        static_cast< OutputCurvatureType >(
          p[0].SquaredEuclideanDistanceTo(p[1]) );
      OutputCurvatureType sq_d02 =
        static_cast< OutputCurvatureType >(
          p[0].SquaredEuclideanDistanceTo(p[2]) );

      OutputCurvatureType cot_theta_210 =
        TriangleType::Cotangent(p[2], p[1], p[0]);
      OutputCurvatureType cot_theta_021 =
        TriangleType::Cotangent(p[0], p[2], p[1]);

      return 0.125 * ( sq_d02 * cot_theta_210 + sq_d01 * cot_theta_021 );
      }
    else
      {
      OutputCurvatureType area =
        static_cast< OutputCurvatureType >(
          TriangleType::ComputeArea(p[0], p[1], p[2]) );
      if ( ( p[1] - p[0] ) * ( p[2] - p[0] ) < 0. )
        {
        return 0.5 * area;
        }
      else
        {
        return 0.25 * area;
        }
      }
  }

  virtual void GenerateData()
  {
    this->CopyInputMeshToOutputMesh();

    OutputMeshPointer output = this->GetOutput();

    OutputPointsContainerPointer  points = output->GetPoints();
    OutputPointsContainerIterator p_it = points->Begin();

    OutputCurvatureType curvature;

    while ( p_it != points->End() )
      {
      curvature = EstimateCurvature( p_it->Value() );
      output->SetPointData(p_it->Index(), curvature);
      ++p_it;
      }
  }

private:
  DiscreteCurvatureQuadEdgeMeshFilter(const Self &); // purposely not
                                                        // implemented
  void operator=(const Self &);                         // purposely not
                                                        // implemented
};
} // end namespace itk

#endif
