/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDemonsRegistrationFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDemonsRegistrationFilter_h_
#define _itkDemonsRegistrationFilter_h_

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkDemonsRegistrationFunction.h"

namespace itk {

/** \class DemonsRegistrationFilter
 * \brief Deformably register two images using the demons algorithm.
 *
 * DemonsRegistrationFilter implements the demons deformable algorithm that 
 * register two images by computing the deformation field which will map a 
 * reference image onto a target image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the target image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the Reference image type, Target image type
 * and the deformation field type.
 *
 * The input reference and target images are set via methods SetReference
 * and SetTarget respectively. An initial deformation field maybe set via
 * SetInitialDeformationField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The algorithm has one parameters: the number of iteration to be performed.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDeformationField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in DemonsRegistrationFunction.
 *
 * \warning This filter assumes that the reference type, target type
 * and deformation field type all have the same number of dimensions.
 * 
 * \sa DemonsRegistrationFunction 
 * \ingroup DeformableImageRegistration MultiThreaded
 */
template<class TReference, class TTarget, class TDeformationField>
class ITK_EXPORT DemonsRegistrationFilter : 
  public PDEDeformableRegistrationFilter< TReference, TTarget,
    TDeformationField>
{
public:
  /** Standard class typedefs. */
  typedef DemonsRegistrationFilter    Self;
  typedef PDEDeformableRegistrationFilter<
    TReference, TTarget,TDeformationField>    Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( DemonsRegistrationFilter, 
    PDEDeformableRegistrationFilter );

  /** Reference image type. */
  typedef typename Superclass::ReferenceType   ReferenceType;
  typedef typename Superclass::ReferencePointer  ReferencePointer;

  /** Target image type. */
  typedef typename Superclass::TargetType    TargetType;
  typedef typename Superclass::TargetPointer  TargetPointer;
  
  /** Deformation field type. */
  typedef typename Superclass::DeformationFieldType 
    DeformationFieldType;
  typedef typename Superclass::DeformationFieldPointer  
    DeformationFieldPointer;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
    FiniteDifferenceFunctionType;

  /** DemonsRegistrationFilterFunction type. */
  typedef DemonsRegistrationFunction<ReferenceType,TargetType,
    DeformationFieldType>  DemonsRegistrationFunctionType;

protected:
  DemonsRegistrationFilter();
  ~DemonsRegistrationFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }

  /** Initialize the state of filter and equation before each iteration. */
  virtual void InitializeIteration();

private:
  DemonsRegistrationFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDemonsRegistrationFilter.txx"
#endif

#endif
