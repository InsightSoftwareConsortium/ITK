/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProcrustesRegistrationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProcrustesRegistrationMetric_h
#define __itkProcrustesRegistrationMetric_h

#include "itkSimilarityRegistrationMetric.h"
#include "itkRegistrationMapperProcrustes.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
  
/** \class ProcrustesRegistrationMetric
 * \brief Procrustes distance between sets of points
 *
 * This class receives two sets of N-D points and register
 * them by minimizing the sum of square distances between 
 * point pairs
 *
 * \ingroup RegistrationMetrics
 */
template <class TTransform, unsigned int NDimension>
class ITK_EXPORT ProcrustesRegistrationMetric : 
      public SimilarityRegistrationMetric<
        VectorContainer< unsigned long, Point<double,NDimension> >,
        RegistrationMapperProcrustes<TTransform,NDimension>,
        vnl_vector_fixed< double, NDimension >,
        vnl_matrix_fixed< double, NDimension, NDimension > > 
{
public:
  /** Standard class typedefs. */
  typedef ProcrustesRegistrationMetric  Self;
  typedef SimilarityRegistrationMetric<
  VectorContainer< unsigned long, Point<double,NDimension> >,
    RegistrationMapperProcrustes<TTransform,NDimension>,
    vnl_vector< double >,vnl_matrix< double > >       Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ProcrustesRegistrationMetric, 
               SimilarityRegistrationMetric );

  /**  Type of the reference. */
  typedef VectorContainer< unsigned long, Point<double,NDimension> > ReferenceType;

  /**  Type of the target. */
  typedef VectorContainer< unsigned long, Point<double,NDimension> > TargetType;

  /**  Type of the mapper. */
  typedef RegistrationMapperProcrustes<TTransform,NDimension>       MapperType;
  
  /**  Type of the transform. */
  typedef TTransform                             TransformationType;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType        MeasureType;

  /**  Type of the measure derivatives. */
  typedef typename Superclass::DerivativeType     DerivativeType;

  /**  Pointer type for the reference.  */
  typedef typename ReferenceType::ConstPointer         ReferenceConstPointer;

  /**  Pointer type for the target.  */
  typedef typename TargetType::ConstPointer            TargetConstPointer;

  /**  Pointer type for the mapper. */
  typedef typename MapperType::Pointer            MapperPointer;

  /** Method for execute the algorithm. */
  virtual void Compute(void);

protected:
  ProcrustesRegistrationMetric();
  virtual ~ProcrustesRegistrationMetric() {};

private:
  ProcrustesRegistrationMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProcrustesRegistrationMetric.txx"
#endif

#endif



