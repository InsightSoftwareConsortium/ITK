/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIdentityTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIdentityTransform_h
#define __itkIdentityTransform_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkArray.h"
#include "itkArray2D.h"
#include "itkTransform.h"

#include "itkObjectFactory.h"


namespace itk
{
  
/** \class IdentityTransform
 * \brief Implementation of an Identity Transform.
 *
 * This Class define the generic interface for an Identity Transform.
 * 
 * It will map every point to itself, every vector to itself and
 * every covariant vector to itself.
 * 
 * This class is intended to be used primarily as a default Transform
 * for initializing those classes supporting a generic Transform.
 *
 * This class is templated over the Representation type for coordinates
 * (that is the type used for representing the components of points and
 * vectors) and over the dimension of the space. In this case the Input
 * and Output spaces are the same so only one dimension is required.
 *
 * \ingroup Transforms
 *
 */
template <class TScalarType,
          unsigned int NDimensions=3>
class ITK_EXPORT  IdentityTransform  : public Transform<TScalarType,NDimensions,NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef IdentityTransform  Self;
  typedef Transform<TScalarType,NDimensions,NDimensions> Superclass;
  typedef SmartPointer< Self >   Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  
  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( IdentityTransform, Transform );

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NDimensions);
  
  /** Type of the input parameters. */
  typedef  TScalarType     ScalarType;

  /** Type of the input parameters. */
  typedef  typename Superclass::ParametersType                 ParametersType;

  /** Type of the Jacobian matrix. */
  typedef  typename Superclass::JacobianType                   JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TScalarType,
                itkGetStaticConstMacro(InputSpaceDimension)>  InputVectorType;
  typedef Vector<TScalarType,
                itkGetStaticConstMacro(OutputSpaceDimension)> OutputVectorType;
  
  /** Standard covariant vector type for this class */
  typedef CovariantVector<TScalarType,
                          itkGetStaticConstMacro(InputSpaceDimension)>  InputCovariantVectorType;
  typedef CovariantVector<TScalarType,
                          itkGetStaticConstMacro(OutputSpaceDimension)> OutputCovariantVectorType;
  
  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TScalarType,
                           itkGetStaticConstMacro(InputSpaceDimension)>  InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType,
                           itkGetStaticConstMacro(OutputSpaceDimension)> OutputVnlVectorType;
  
  /** Standard coordinate point type for this class */
  typedef Point<TScalarType,
                itkGetStaticConstMacro(InputSpaceDimension)> InputPointType;
  typedef Point<TScalarType,
                itkGetStaticConstMacro(OutputSpaceDimension)> OutputPointType;
  
  /**  Method to transform a point. */
  virtual OutputPointType TransformPoint(const InputPointType  &point ) const
    { return point; }

  /**  Method to transform a vector. */
  virtual OutputVectorType TransformVector(const InputVectorType &vector) const
    { return vector; }

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &vector) const
    { return vector; }

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &vector) const
    { return vector; }

  /** Set the Transformation Parameters
   * and update the internal transformation. */
  virtual void SetParameters(const ParametersType &) {};

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point.
   *
   * The Jacobian can be expressed as a set of partial derivatives of the
   * output point components with respect to the parameters that defined
   * the transform:
   *
   * \f[
   *
      J=\left[ \begin{array}{cccc}
      \frac{\partial x_{1}}{\partial p_{1}} & 
      \frac{\partial x_{2}}{\partial p_{1}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{1}}\\
      \frac{\partial x_{1}}{\partial p_{2}} & 
      \frac{\partial x_{2}}{\partial p_{2}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{2}}\\
      \vdots  & \vdots  & \ddots  & \vdots \\
      \frac{\partial x_{1}}{\partial p_{m}} & 
      \frac{\partial x_{2}}{\partial p_{m}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{m}}
      \end{array}\right] 
   *
   * \f]
   * **/
  virtual const JacobianType & GetJacobian(const InputPointType  & ) const
    { 
    m_Jacobian = JacobianType(NDimensions,1); 
    m_Jacobian.Fill(0.0); 
    return m_Jacobian;
    }



protected:
  IdentityTransform():Transform<TScalarType,NDimensions,NDimensions>(NDimensions,1) {}; 
  virtual ~IdentityTransform() {};


private:
  IdentityTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif



