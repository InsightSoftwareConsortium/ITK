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
#ifndef itkDiscreteCurvatureTensorQuadEdgeMeshFilter_h
#define itkDiscreteCurvatureTensorQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class DiscreteCurvatureTensorQuadEdgeMeshFilter
 *
 * \brief FIXME Add documentation here
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TInputMesh, typename TOutputMesh=TInputMesh >
class DiscreteCurvatureTensorQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef DiscreteCurvatureTensorQuadEdgeMeshFilter     Self;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
                                                        Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteCurvatureTensorQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
//  itkConceptMacro( OutputIsFloatingPointCheck,
//                   ( Concept::IsFloatingPoint< OutputCurvatureType > ) );
  // End concept checking
#endif

protected:
  DiscreteCurvatureTensorQuadEdgeMeshFilter() {}
  ~DiscreteCurvatureTensorQuadEdgeMeshFilter() {}

  ///TODO to be implemented
  virtual void GenerateData()
  {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DiscreteCurvatureTensorQuadEdgeMeshFilter);
};
}

#endif
