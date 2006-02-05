/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableSimplexMesh3DBalloonForceFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableSimplexMesh3DBalloonForceFilter_h
#define __itkDeformableSimplexMesh3DBalloonForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkImage.h"
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
  */
  template <class TInputMesh, class TOutputMesh>
class DeformableSimplexMesh3DBalloonForceFilter : public DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
  {
  public:
    /** Standard "Self" typedef. */
    typedef DeformableSimplexMesh3DBalloonForceFilter  Self;

    /** Standard "Superclass" typedef. */
    typedef DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh> Superclass;

    /** Smart pointer typedef support */
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method of creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(DeformableSimplexMesh3DBalloonForceFilter,DeformableSimplexMesh3DFilter);

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

    itkSetMacro(Kappa, double);
    itkGetMacro(Kappa, double);


  protected:
    DeformableSimplexMesh3DBalloonForceFilter();
    ~DeformableSimplexMesh3DBalloonForceFilter();
    DeformableSimplexMesh3DBalloonForceFilter(const Self&) 
      {
      }
    void operator=(const Self&) 
      {
      }
    void PrintSelf(std::ostream& os, Indent indent) const;


    /**
    * Compute the external force component
    */
    virtual void ComputeExternalForce(SimplexMeshGeometry * data);

    /** Parameters definitions. */

    /**
    * scalar for balloon force 
    */
    double    m_Kappa;

  }; // end of class


  } // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DBalloonForceFilter.txx"
#endif

#endif //__itkDeformableSimplexMesh3DBalloonForceFilter_H
