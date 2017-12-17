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
#ifndef itkAdditiveGaussianNoiseQuadEdgeMeshFilter_h
#define itkAdditiveGaussianNoiseQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
/** \class AdditiveGaussianNoiseQuadEdgeMeshFilter
 * \brief Add Gaussian noise to the points defining an itkQuadEdgeMesh.
 *
 * \author Davis Vigneault
 *
 * This class adds Gaussian noise with a specified mean and standard
 * deviation to the coordinates of the points defining an itkMesh.
 * Mesh topology and other data is passed unaltered.  This may be
 * useful in testing the robustness of an algorithm to small changes
 * in the input mesh, augmenting datasets for machine learning, and
 * counteracting deleterious effects which highly regular regions
 * of a mesh may occassionally have on mesh processing.
 *
 * \ingroup DVMeshNoise
 */
template< typename TInputMesh, typename TOutputMesh = TInputMesh >
class AdditiveGaussianNoiseQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef AdditiveGaussianNoiseQuadEdgeMeshFilter                     Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType CoordRepType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AdditiveGaussianNoiseQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** Mean of noise. */
  itkGetConstMacro(Mean, CoordRepType);
  itkSetMacro(Mean, CoordRepType);

  /** Variance of noise. */
  itkGetConstMacro(Sigma, CoordRepType);
  itkSetMacro(Sigma, CoordRepType);

  /** Initialization seed. */
  itkGetConstMacro(Seed, int);
  itkSetMacro(Seed, int);

protected:

  AdditiveGaussianNoiseQuadEdgeMeshFilter();
  ~AdditiveGaussianNoiseQuadEdgeMeshFilter() ITK_OVERRIDE{}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Requested Data */
  void GenerateData(void) ITK_OVERRIDE;

  CoordRepType m_Mean;
  CoordRepType m_Sigma;
  int          m_Seed;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(AdditiveGaussianNoiseQuadEdgeMeshFilter);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAdditiveGaussianNoiseQuadEdgeMeshFilter.hxx"
#endif

#endif
