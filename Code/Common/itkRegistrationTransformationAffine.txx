/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransformationAffine.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/


namespace itk
{

/**
 * Constructor
 */
template <class TParameters, unsigned int NDimensions>
RegistrationTransformationAffine<TParameters,NDimensions>
::RegistrationTransformationAffine()
{
}


/**
 * Constructor
 */
template <class TParameters,unsigned int NDimensions>
RegistrationTransformationAffine<TParameters,NDimensions>
::RegistrationTransformationAffine( const Self & other )
{
}


/**
 * Assignment Operator
 */
template <class TParameters,unsigned int NDimensions>
const RegistrationTransformationAffine<TParameters,NDimensions> &
RegistrationTransformationAffine<TParameters,NDimensions>
::operator=( const Self & other )
{
  return *this;
}


/**
 * Transform a Point
 */
template <class TParameters,unsigned int NDimensions>
Point<NDimensions,double>
RegistrationTransformationAffine<TParameters,NDimensions>
::Transform(const Point<NDimensions,double> & point )
{
  return point;
}




/**
 * Transform a Point
 */
template <class TParameters,unsigned int NDimensions>
Vector<double,NDimensions>
RegistrationTransformationAffine<TParameters,NDimensions>
::Transform(const Vector<double,NDimensions> & vector )
{
  return vector;
}


/**
 * Inverse Transform a Point
 */
template <class TParameters,unsigned int NDimensions>
Point<NDimensions,double>
RegistrationTransformationAffine<TParameters,NDimensions>
::InverseTransform(const Point<NDimensions,double> & point )
{
  return point;
}




/**
 * Inverse Transform a Point
 */
template <class TParameters,unsigned int NDimensions>
Vector<double,NDimensions>
RegistrationTransformationAffine<TParameters,NDimensions>
::InverseTransform(const Vector<double,NDimensions> & vector )
{
  return vector;
}




} // end namespace itk
