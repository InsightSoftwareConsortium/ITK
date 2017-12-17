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
#ifndef itkAdditiveGaussianNoiseMeshFilter_h
#define itkAdditiveGaussianNoiseMeshFilter_h

#include <itkMeshToMeshFilter.h>

namespace itk
{
/** \class AdditiveGaussianNoiseMeshFilter
 * \brief Add Gaussian noise to the points defining an itkMesh.
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
template< typename TInput, typename TOutput = TInput >
class AdditiveGaussianNoiseMeshFilter:
public MeshToMeshFilter< TInput, TOutput >
{

public:

  /** Standard class typedefs. */
  typedef AdditiveGaussianNoiseMeshFilter     Self;
  typedef MeshToMeshFilter< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  typedef TInput                           InputMeshType;
  typedef TOutput                          OutputMeshType;
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Type for representing coordinates. */
  typedef typename TInput::CoordRepType CoordRepType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AdditiveGaussianNoiseMeshFilter, MeshToMeshFilter);

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

  AdditiveGaussianNoiseMeshFilter();
  ~AdditiveGaussianNoiseMeshFilter() override{}

  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Requested Data */
  void GenerateData() override;

  CoordRepType m_Mean;
  CoordRepType m_Sigma;
  int          m_Seed;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(AdditiveGaussianNoiseMeshFilter);

};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAdditiveGaussianNoiseMeshFilter.hxx"
#endif

#endif
