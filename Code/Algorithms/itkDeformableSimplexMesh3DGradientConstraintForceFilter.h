/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableSimplexMesh3DGradientConstraintForceFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _DeformableSimplexMesh3DGradientConstraintForceFilter_h
#define _DeformableSimplexMesh3DGradientConstraintForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkImage.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkCovariantVector.h"

#include <set>

namespace itk
{

/** \class DeformableSimplexMesh3DGradientConstraintForceFilter
 * \brief
 * Additional to its superclass this class reimplemets the external forces 
 * in which the highest gradient is found in the direction of the normal to eac vertex.
 *
 * \author Leila Baghdadi. Mouse Imaging Centre, Hospital for Sick Children, Toronto, Ontario,Canada.
 */

template <class TInputMesh, class TOutputMesh>
class DeformableSimplexMesh3DGradientConstraintForceFilter : public DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
{
public:
  /** Standard "Self" typedefs. */
  typedef DeformableSimplexMesh3DGradientConstraintForceFilter Self;

  /** Standard "Superclass" typedef. */
  typedef  DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableSimplexMesh3DGradientConstraintForceFilter, DeformableSimplexMesh3DFilter); 

  /** Some typedefs. */
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename Superclass::PointType                  PointType;
  typedef typename Superclass::GradientIndexType          GradientIndexType;
  typedef typename Superclass::GradientIndexValueType     GradientIndexValueType;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer     InputMeshPointer;
  typedef typename OutputMeshType::Pointer    OutputMeshPointer;

  typedef typename InputMeshType::PixelType         PixelType;

  typedef Image<PixelType, 3>                                   GradientIntensityImageType;
  typedef typename GradientIntensityImageType::Pointer          GradientIntensityImagePointer;
  /** control the range of search for Bresenham at normal line */
  itkSetMacro(Range, int);
  itkGetMacro(Range, int);


protected:
  DeformableSimplexMesh3DGradientConstraintForceFilter();
  ~DeformableSimplexMesh3DGradientConstraintForceFilter();
  DeformableSimplexMesh3DGradientConstraintForceFilter(const Self&) 
      {
      }
  void operator=(const Self&){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Compute the external force component
   */
  virtual void ComputeExternalForce(SimplexMeshGeometry* data);  

  

  /** 
   * Range of search for Bresenham algorithm (normal line at each vertex)
   */
  int m_Range;

 private:
/**
   *
   */
  GradientIndexType BresenhamLine(GradientIndexType a,GradientIndexType b);

}; // end of class

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.txx"
#endif

#endif /* __DeformableSimplexMesh3DGradientConstraintForceFilter_h */
