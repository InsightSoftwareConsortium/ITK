/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMINCTransformAdapter_h
#define itkMINCTransformAdapter_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_det.h"
#include "vnl/vnl_vector_fixed_ref.h"
#include "vnl/vnl_vector.h"
#include "itkTransform.h"
#include "itkObjectFactory.h"

//minc header
#include "itk_minc2.h"

namespace itk
{

/** \class MINCTransformAdapter
  * \ingroup  ITKIOTransformMINC
  * \brief ITK wrapper around MINC general transform functions, supports all the transformations that MINC XFM supports
  *
  * \author Vladimir S. FONOV
  *         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
  * \ingroup ITKIOTransformMINC
  */
template<typename TParametersValueType=double, unsigned int NInputDimensions=3,unsigned int NOutputDimensions=3>
  class MINCTransformAdapter : public Transform<TParametersValueType, NInputDimensions, NOutputDimensions>
{
public:
  /** Standard class typedefs. */
  typedef MINCTransformAdapter  Self;

  typedef Transform<TParametersValueType, NInputDimensions, NOutputDimensions> Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::NumberOfParametersType  NumberOfParametersType;

  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( MINCTransformAdapter, Transform );

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NInputDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NOutputDimensions);

  /** Type of the input parameters. */
  typedef  double ScalarType;

  /** Type of the input parameters. */
  typedef typename Superclass::ParametersType      ParametersType;
  typedef typename Superclass::FixedParametersType FixedParametersType;

  /** Type of the Jacobian matrix. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType, itkGetStaticConstMacro(InputSpaceDimension)>  InputVectorType;
  typedef Vector<TParametersValueType, itkGetStaticConstMacro(OutputSpaceDimension)> OutputVectorType;

  /** Standard variable length vector type for this class
  *  this provides an interface for the VectorImage class */
  typedef VariableLengthVector<TParametersValueType> InputVectorPixelType;
  typedef VariableLengthVector<TParametersValueType> OutputVectorPixelType;

  /** Standard covariant vector type for this class */
  typedef CovariantVector<TParametersValueType, itkGetStaticConstMacro(InputSpaceDimension)>  InputCovariantVectorType;

  typedef CovariantVector<TParametersValueType, itkGetStaticConstMacro(OutputSpaceDimension)> OutputCovariantVectorType;

  /** Standard coordinate point type for this class */
  typedef Point<TParametersValueType,NInputDimensions > InputPointType;
  typedef Point<TParametersValueType,NInputDimensions > OutputPointType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType, NInputDimensions>  InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType, NOutputDimensions> OutputVnlVectorType;

  /**  Method to transform a point. */
  virtual OutputPointType TransformPoint(const InputPointType  &point ) const ITK_OVERRIDE
  {
    if(!m_Initialized)
      {
      return point;
      }

    if(m_Invert && !m_Initialized_invert)
      {
      return point;
      }

    OutputPointType pnt;
    //works only for 3D->3D transforms
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), point[0], point[1], point[2], &pnt[0], &pnt[1], &pnt[2]);

    return pnt;
  }

  //! use finate element difference to estimate local jacobian
  void estimate_local_jacobian(const InputPointType  &orig, vnl_matrix_fixed< double, 3, 3 > &m)
  {
    double u1,v1,w1;
    double u2,v2,w2;
    const double delta=1e-4;

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm) , orig[0]-delta, orig[1], orig[2],&u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm) , orig[0]+delta, orig[1], orig[2],&u2, &v2, &w2);
    m(0,0)=(u2-u1)/(2*delta);
    m(0,1)=(v2-v1)/(2*delta);
    m(0,2)=(w2-w1)/(2*delta);

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm) , orig[0], orig[1]-delta, orig[2],&u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm) , orig[0], orig[1]+delta, orig[2],&u2, &v2, &w2);
    m(1,0)=(u2-u1)/(2*delta);
    m(1,1)=(v2-v1)/(2*delta);
    m(1,2)=(w2-w1)/(2*delta);

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1], orig[2]-delta,&u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1], orig[2]+delta,&u2, &v2, &w2);
    m(2,0)=(u2-u1)/(2*delta);
    m(2,1)=(v2-v1)/(2*delta);
    m(2,2)=(w2-w1)/(2*delta);
  }

  /**  Method to transform a vector. */
  OutputVectorType TransformVector( const InputVectorType& vector, const InputPointType &  ) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return vector;
  }

  /**  Method to transform a vector. */
  OutputVnlVectorType TransformVector( const InputVnlVectorType& vector, const InputPointType & ) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return vector;
  }

  /**  Method to transform a vector. */
  OutputVectorType TransformVector( const InputVectorType& vector) const ITK_OVERRIDE
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVnlVectorType TransformVector( const InputVnlVectorType& vector) const ITK_OVERRIDE
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVectorPixelType TransformVector( const InputVectorPixelType& vector) const ITK_OVERRIDE
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVectorPixelType TransformVector(
    const InputVectorPixelType& vector,
    const InputPointType & ) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return vector;
  }

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &vector
  , const InputPointType & ) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return vector;
  }

/**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &vector) const ITK_OVERRIDE
  {
    return Superclass::TransformCovariantVector(vector);
  }

/**  Method to transform a CovariantVector. */
  virtual OutputVectorPixelType TransformCovariantVector(
    const InputVectorPixelType &vector) const ITK_OVERRIDE
  {
    return Superclass::TransformCovariantVector(vector);
  }

  /**  Method to transform a CovariantVector. */
  virtual OutputVectorPixelType TransformCovariantVector(
    const InputVectorPixelType &vector, const InputPointType & ) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return vector;
  }

  /** Set the transformation to an Identity
    */
  virtual void SetIdentity( void )
  {
    cleanup();
  }

  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
  }

  virtual void ComputeJacobianWithRespectToParameters(
              const InputPointType &,
              JacobianType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
  }

  virtual NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    //this transform is defined by XFM file
    itkExceptionMacro( << "Not Defined" );
    return 0;
  }

  /** Set the Transformation Parameters
    * and update the internal transformation. */
  virtual void  SetParameters(const ParametersType &) ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
  }

  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "Not Implemented" );
    return m_Parameters;
  }

  void OpenXfm(const char *xfm)
  {
    cleanup();
    if(input_transform_file(xfm, &m_Xfm) != VIO_OK)
      itkExceptionMacro( << "Error reading XFM:" << xfm );
    m_Initialized=true;
  }

  void Invert(void)
  {
    if(!m_Initialized)
      itkExceptionMacro( << "XFM not initialized" );
    if(!m_Initialized_invert)
      {
      create_inverse_general_transform(&m_Xfm,&m_Xfm_inv);
      m_Initialized_invert=true;
      }
    m_Invert= !m_Invert;
  }

protected:
  MINCTransformAdapter():
    Transform<TParametersValueType, NInputDimensions, NOutputDimensions>(0),
    m_Invert(false),
    m_Initialized(false),
    m_Initialized_invert(false)
  {
    if(NInputDimensions!=3 || NOutputDimensions!=3)
      itkExceptionMacro(<< "Sorry, only 3D to 3d minc xfm transform is currently implemented");
  }

  virtual ~MINCTransformAdapter() ITK_OVERRIDE
  {
    cleanup();
  }

  void cleanup(void)
  {
    if(m_Initialized)
      {
      delete_general_transform(&m_Xfm);
      }
    if(m_Initialized_invert)
      {
      delete_general_transform(&m_Xfm_inv);
      }
    m_Initialized=false;
    m_Initialized_invert=false;
  }

  ParametersType m_Parameters;

  mutable VIO_General_transform m_Xfm;
  mutable VIO_General_transform m_Xfm_inv;

  bool m_Invert;
  bool m_Initialized;
  bool m_Initialized_invert;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MINCTransformAdapter );
};

}
#endif //itkMINCTransformAdapter_h
