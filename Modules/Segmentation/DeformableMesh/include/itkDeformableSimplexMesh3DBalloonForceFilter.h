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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkDeformableSimplexMesh3DBalloonForceFilter_h
#define itkDeformableSimplexMesh3DBalloonForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkCovariantVector.h"

#include <set>

namespace itk
{
/** \class DeformableSimplexMesh3DBalloonForceFilter
  * \brief
  * Additional to its superclass this model adds an balloon force component to the
  * internal forces.
  *
  * The balloon force can be scaled, by setting the parameter kappa.
  *
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  *
  * \ingroup ITKDeformableMesh
  */
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT DeformableSimplexMesh3DBalloonForceFilter:public DeformableSimplexMesh3DFilter< TInputMesh,
                                                                                                 TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef DeformableSimplexMesh3DBalloonForceFilter Self;

  /** Standard "Superclass" typedef. */
  typedef DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableSimplexMesh3DBalloonForceFilter, DeformableSimplexMesh3DFilter);

  /** Some typedefs. */
  typedef TInputMesh                                  InputMeshType;
  typedef TOutputMesh                                 OutputMeshType;
  typedef typename Superclass::PointType              PointType;
  typedef typename Superclass::GradientIndexType      GradientIndexType;
  typedef typename Superclass::GradientIndexValueType GradientIndexValueType;
  typedef typename Superclass::GradientImageType      GradientImageType;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  typedef typename InputMeshType::PixelType PixelType;

  typedef Image< PixelType, 3 >                        GradientIntensityImageType;
  typedef typename GradientIntensityImageType::Pointer GradientIntensityImagePointer;

  itkSetMacro(Kappa, double);
  itkGetConstMacro(Kappa, double);

protected:
  DeformableSimplexMesh3DBalloonForceFilter();
  ~DeformableSimplexMesh3DBalloonForceFilter() ITK_OVERRIDE;
  DeformableSimplexMesh3DBalloonForceFilter(const Self &)
  {}

  void operator=(const Self &)
  {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Compute the external force component
   */
  virtual void ComputeExternalForce(SimplexMeshGeometry *data,const GradientImageType *gradientImage) ITK_OVERRIDE;

  /** Parameters definitions. */

  /**
   * scalar for balloon force
   */
  double m_Kappa;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DBalloonForceFilter.hxx"
#endif

#endif // itkDeformableSimplexMesh3DBalloonForceFilter_h
