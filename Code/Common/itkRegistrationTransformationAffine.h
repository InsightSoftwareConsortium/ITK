/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransformationAffine.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationTransformationAffine_h
#define __itkRegistrationTransformationAffine_h

#include "itkObject.h"
#include "itkRegistrationTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkVectorContainer.h"
#include "itkAffineTransform.h"


namespace itk
{
  
/** \class RegistrationTransformationAffine
 * \brief Generic Affine Transformation for a registration method
 *
 * This Class define the generic interface for an Affine Transformation 
 * It provides Transform() and InverseTransform() methods that works 
 * over Points and Vectors.
 *
 */

template <unsigned int NDimensions>
class ITK_EXPORT  RegistrationTransformationAffine : 
        public RegistrationTransformation<
                  VectorContainer<unsigned int,double> >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationTransformationAffine  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef RegistrationTransformation< 
                       VectorContainer< unsigned int, double> > Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the input parameters
   */
  typedef    VectorContainer<unsigned int, double>   ParametersType;


 /** 
   * Pointer Type to input parameters
   */
  typedef    typename ParametersType::Pointer  ParametersPointer;


 /** 
   * Affine Transform Type
   */
  typedef  AffineTransform<double,NDimensions>      AffineTransformationType;


 /** 
   * Point Type
   */
  typedef  typename AffineTransformationType::PointType     PointType;


 /** 
   * Vector Type
   */
  typedef  typename AffineTransformationType::VectorType   VectorType;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationTransformationAffine, RegistrationTransformation);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Affine transformation
   */
   PointType Transform( const PointType & point );


  /**
   * Transform a Vector using the Affine transformation
   */
   VectorType Transform( const VectorType & point );


  /**
   * Inverse Transform a Point using the Affine transformation
   */
   PointType InverseTransform( const PointType & point );


  /**
   * Inverse Transform a Vector using the Affine transformation
   */
   VectorType InverseTransform( const VectorType & point );


  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
   void SetParameters(const ParametersType *);



protected:

  RegistrationTransformationAffine();
  virtual ~RegistrationTransformationAffine() {};
  RegistrationTransformationAffine(const Self&);
  const Self & operator=(const Self&);


private:

  AffineTransformationType      m_AffineTransform;
  ParametersType::Pointer       m_Parameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationTransformationAffine.txx"
#endif

#endif



