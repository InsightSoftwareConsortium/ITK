/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAffineTransform_h
#define __itkAffineTransform_h

#include "itkObject.h"
#include "itkTransform.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkVectorContainer.h"
#include "itkAffineTransform.h"


namespace itk
{
  
/** \class AffineTransform
 * \brief Generic Affine Transformation for a registration method
 *
 * This Class define the generic interface for an Affine Transformation 
 * It provides Transform() and InverseTransform() methods that works 
 * over Points and Vectors.
 *
 */

template <unsigned int NDimensions>
class ITK_EXPORT  AffineTransform : 
        public Transform<
                  VectorContainer<unsigned int,double> >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AffineTransform  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transform< 
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
  itkTypeMacro(AffineTransform, Transform);


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

  AffineTransform();
  virtual ~AffineTransform() {};
  AffineTransform(const Self&);
  const Self & operator=(const Self&);


private:

  AffineTransformationType      m_AffineTransform;
  ParametersType::Pointer       m_Parameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineTransform.txx"
#endif

#endif



