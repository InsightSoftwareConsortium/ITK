/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransform_h
#define __itkTransform_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkArray.h"
#include "itkArray2D.h"

#include "itkObjectFactory.h"


namespace itk
{
  
/** \class Transform
 * \brief Transform points and vector from an input space to an output space.
 *
 * This abstract class define the generic interface for a geometrical 
 * transformation from one space to another. The class provides methods
 * for mapping points, vectors and covariant vectors from the input space 
 * to the output space. 
 *
 * Given that transformation are not necesarily invertible, this basic
 * class does not provide the methods for back transfromation. Back transform
 * methods are implemented in derived classes where appropriate.
 * 
 * \par Registration Framework Support
 * Typically a Transform class have several methods for setting its 
 * parameters. For use in the registration framework, the parameters must
 * also be represented by an array of doubles to allow communication
 * with generic optimizers. The Array of transformation parameters is set using
 * the SetParameters() method.
 *
 * Another requirement of the registration framework is the computation
 * of the transform Jacobian. In general, a ImageToImageMetric requires
 * the knowledge of the Jacobian in order to compute the metric derivatives.
 * The Jacobian is a matrix whose element are the partial derivatives
 * of the output point with respect to the array of parameters that defines
 * the transform.
 *
 * \ingroup Transforms
 *
 */
template <class TScalarType,
          unsigned int NInputDimensions=3, 
          unsigned int NOutputDimensions=3>
class ITK_EXPORT  Transform  : public Object
{
public:
  /** Standard class typedefs. */
  typedef Transform  Self;
  typedef Object Superclass;
  typedef SmartPointer< Self >   Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  
  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( Transform, Object );

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NInputDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NOutputDimensions);

  /** Type of the scalar representing coordinate and vector elements. */
  typedef  TScalarType     ScalarType;

  /** Type of the input parameters. */
  typedef  Array< double >                           ParametersType;

  /** Type of the Jacobian matrix. */
  typedef  Array2D< double >                           JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TScalarType, NInputDimensions>  InputVectorType;
  typedef Vector<TScalarType, NOutputDimensions> OutputVectorType;
  
  /** Standard covariant vector type for this class */
  typedef CovariantVector<TScalarType, NInputDimensions>  InputCovariantVectorType;
  typedef CovariantVector<TScalarType, NOutputDimensions> OutputCovariantVectorType;
  
  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TScalarType, NInputDimensions>  InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType, NOutputDimensions> OutputVnlVectorType;
  
  /** Standard coordinate point type for this class */
  typedef Point<TScalarType, NInputDimensions> InputPointType;
  typedef Point<TScalarType, NOutputDimensions> OutputPointType;
  
  /**  Method to transform a point. */
  virtual OutputPointType TransformPoint(const InputPointType  & ) const
    { return OutputPointType(); } 

  /**  Method to transform a vector. */
  virtual OutputVectorType    TransformVector(const InputVectorType &) const
    { return OutputVectorType(); }

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const
    { return OutputVnlVectorType(); }

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &) const
    { return OutputCovariantVectorType(); } 

  /** Set the transformation parameters and update internal transformation. */
  virtual void SetParameters( const ParametersType & ) 
    { itkExceptionMacro( << "Subclasses should override this method" ) };

  /** Get the Transformation Parameters. */
  virtual const ParametersType& GetParameters(void) const
    { itkExceptionMacro( << "Subclasses should override this method" );
      return m_Parameters; };

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation
   * at a given input point. The rank of the Jacobian will also indicate 
   * if the transform is invertible at this point.
   *
   * The Jacobian is be expressed as a matrix of partial derivatives of the
   * output point components with respect to the parameters that defined
   * the transform:
   *
   * \f[
   *
      J=\left[ \begin{array}{cccc}
      \frac{\partial x_{1}}{\partial p_{1}} & 
      \frac{\partial x_{1}}{\partial p_{2}} & 
      \cdots  & \frac{\partial x_{1}}{\partial p_{m}}\\
      \frac{\partial x_{2}}{\partial p_{1}} & 
      \frac{\partial x_{2}}{\partial p_{2}} & 
      \cdots  & \frac{\partial x_{2}}{\partial p_{m}}\\
      \vdots  & \vdots  & \ddots  & \vdots \\
      \frac{\partial x_{n}}{\partial p_{1}} & 
      \frac{\partial x_{n}}{\partial p_{2}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{m}}
      \end{array}\right] 
   *
   * \f]
   * **/
  virtual const JacobianType & GetJacobian(const InputPointType  &) const
    { itkExceptionMacro( << "Subclass should override this method" );
      return m_Jacobian; }; 


  /** Return the number of parameters that completely define the Transfom  */
  unsigned int virtual GetNumberOfParameters(void) const 
                      { return m_Parameters.Size(); }


protected:
  Transform(); 
  Transform(unsigned int Dimension, unsigned int NumberOfParameters);
  virtual ~Transform() {};


  mutable ParametersType     m_Parameters;
  mutable JacobianType       m_Jacobian;

private:
  Transform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransform.txx"
#endif

#endif



