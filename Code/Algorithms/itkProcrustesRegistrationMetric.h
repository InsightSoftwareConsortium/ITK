/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProcrustesRegistrationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
  /**
   * Standard "Self" typedef.
   */
  typedef ProcrustesRegistrationMetric  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef SimilarityRegistrationMetric<
        VectorContainer< unsigned long, Point<double,NDimension> >,
        RegistrationMapperProcrustes<TTransform,NDimension>,
        vnl_vector< double >,
        vnl_matrix< double > >             Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Reference
   */
   typedef VectorContainer< unsigned long, Point<double,NDimension> > ReferenceType;


  /**
   *  Type of the Target
   */
   typedef VectorContainer< unsigned long, Point<double,NDimension> > TargetType;
 

  /**
   *  Type of the Mapper
   */
  typedef RegistrationMapperProcrustes<TTransform,NDimension>       MapperType;
  
  
  /**
   *  Type of the Transform
   */
  typedef TTransform                             TransformationType;
 

  /**
   *  Type of the measure
   */
  typedef typename Superclass::MeasureType        MeasureType;
 

  /**
   *  Type of the measure derivatives
   */
  typedef typename Superclass::DerivativeType     DerivativeType;


  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::Pointer         ReferencePointer;


  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::Pointer            TargetPointer;


  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer            MapperPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ProcrustesRegistrationMetric, 
               SimilarityRegistrationMetric );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for execute the algorithm
   */
   virtual void Compute(void);
  

protected:

  ProcrustesRegistrationMetric();
  virtual ~ProcrustesRegistrationMetric() {};
  ProcrustesRegistrationMetric(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProcrustesRegistrationMetric.txx"
#endif

#endif



