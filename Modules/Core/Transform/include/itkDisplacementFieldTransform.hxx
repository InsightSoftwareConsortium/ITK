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
#ifndef __itkDisplacementFieldTransform_hxx
#define __itkDisplacementFieldTransform_hxx

#include "itkDisplacementFieldTransform.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{

/* Forward-declaration to avoid including the header file that would
 * introduce a circular dependency in the Transform module. */
template< class TInputImage, class TCoordRep >
class VectorLinearInterpolateImageFunction;

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
DisplacementFieldTransform<TScalar, NDimensions>::
DisplacementFieldTransform() : Superclass( NDimensions, 0 )
{
  this->m_DisplacementField = NULL;
  this->m_InverseDisplacementField = NULL;

  //Setup and assign default interpolator
  typedef VectorLinearInterpolateImageFunction<
                                          DisplacementFieldType,
                                          ScalarType>
                                                      DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer
                                interpolator = DefaultInterpolatorType::New();
  this->m_Interpolator = interpolator;

  //Setup and assign parameter helper. This will hold the displacement field
  // for access through the common TransformParameters interface.
  TransformParametersHelperType* helper = new TransformParametersHelperType;
  //After assigning this, m_Parametes will manage this,
  // deleting when appropriate.
  this->m_Parameters.SetHelper( helper );

  m_DisplacementFieldSetTime = 0;

  /* Initialize the identity jacobian. */
  m_IdentityJacobian.SetSize( NDimensions, NDimensions );
  m_IdentityJacobian.Fill(0.0);
  for( unsigned int dim=0; dim < NDimensions; dim++ )
    {
    m_IdentityJacobian[dim][dim] = 1.0;
     }
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
DisplacementFieldTransform<TScalar, NDimensions>::
~DisplacementFieldTransform()
{
}

/**
 * Transform point
 */
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputPointType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformPoint( const InputPointType& inputPoint ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  typename InterpolatorType::ContinuousIndexType cidx;
  typename InterpolatorType::PointType point;
  point.CastFrom( inputPoint );

  OutputPointType outputPoint;
  outputPoint.CastFrom( inputPoint );

  if( this->m_Interpolator->IsInsideBuffer( point ) )
    {
    this->m_DisplacementField->
      TransformPhysicalPointToContinuousIndex( point, cidx );
    typename InterpolatorType::OutputType displacement =
      this->m_Interpolator->EvaluateAtContinuousIndex( cidx );
    outputPoint += displacement;
    }
  //else
  // simply return inputPoint

  return outputPoint;
}

/**
 * Transform covariant vector
 */

template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputCovariantVectorType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformCovariantVector( const InputCovariantVectorType& vector,
                            const InputPointType & point) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  JacobianType jacobian;
  /* Get the inverse Jacobian directly for efficiency. It means we don't have
   * to compute an SVD inverse. */
  this->GetInverseJacobianOfForwardFieldWithRespectToPosition( point, jacobian );

  OutputCovariantVectorType result;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    result[i] = NumericTraits< ScalarType >::Zero;
    for ( unsigned int j = 0; j < NDimensions; j++ )
      {
      result[i] += jacobian[j][i] * vector[j]; // Inverse
                                               // transposed
      }
    }

  return result;
}

template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputVectorPixelType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformCovariantVector( const InputVectorPixelType& vector,
                            const InputPointType & point) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  JacobianType jacobian;
  /* Get the inverse Jacobian directly for efficiency. It means we don't have
   * to compute an SVD inverse. */
  this->GetInverseJacobianOfForwardFieldWithRespectToPosition( point, jacobian );

  const unsigned int numberOfComponents = NumericTraits< InputVectorPixelType >::GetLength( vector );

  OutputVectorPixelType result;     // Converted vector
  result.SetSize( numberOfComponents );

  JacobianType dataJacobian;
  dataJacobian.SetSize( numberOfComponents, numberOfComponents );

  for ( unsigned int i = 0; i < numberOfComponents; i++ )
    {
    if ( i < NDimensions )
      {
      result[i] = NumericTraits< ScalarType >::Zero;
      for ( unsigned int j = 0; j < NDimensions; j++ )
        {
        result[i] += jacobian[j][i] * vector[j];
        }
      }
    else
      {
      result[i] = vector[i];
      }
    }

  return result;
}


/**
 * Transform vector
 */
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputVectorType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformVector( const InputVectorType& vector, const InputPointType & point ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  JacobianType jacobian;
  this->GetJacobianWithRespectToPosition( point, jacobian );
  OutputVectorType result;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    result[i] = NumericTraits< ScalarType >::Zero;
    for ( unsigned int j = 0; j < NDimensions; j++ )
      {
      result[i] += jacobian[i][j] * vector[j];
      }
    }

  return result;
}


/**
 * Transform vector
 */
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputVnlVectorType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformVector( const InputVnlVectorType& vector, const InputPointType & point ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  JacobianType jacobian;
  this->GetJacobianWithRespectToPosition( point, jacobian );
  OutputVnlVectorType result;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    result[i] = NumericTraits< ScalarType >::Zero;
    for ( unsigned int j = 0; j < NDimensions; j++ )
      {
      result[i] += jacobian[i][j] * vector[j];
      }
    }

  return result;
}

/**
 * Transform vector
 */
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputVectorPixelType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformVector( const InputVectorPixelType& vector, const InputPointType & point ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  const unsigned int numberOfComponents = NumericTraits< InputVectorPixelType >::GetLength( vector );

  JacobianType jacobian;
  this->GetJacobianWithRespectToPosition( point, jacobian );
  OutputVectorPixelType result;
  result.SetSize( numberOfComponents );

  for ( unsigned int i = 0; i < numberOfComponents; i++ )
    {
    if ( i < NDimensions )
      {
      result[i] = NumericTraits< ScalarType >::Zero;
      for ( unsigned int j = 0; j < NDimensions; j++ )
        {
        result[i] += jacobian[i][j] * vector[j];
        }
      }
    else
      {
      result[i] = vector[i];
      }
    }

  return result;
}

/**
 * Transform tensor
 */
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputDiffusionTensor3DType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformDiffusionTensor( const InputDiffusionTensor3DType& inputTensor, const InputPointType & point ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  //JacobianType jacobian;
  //this->GetJacobianWithRespectToPosition( point, jacobian );

  //Get Tensor-space version of local transform (i.e. always 3D)
  typedef MatrixOffsetTransformBase<ScalarType, InputDiffusionTensor3DType::Dimension, InputDiffusionTensor3DType::Dimension> EigenVectorTransformType;
  typename  EigenVectorTransformType::MatrixType matrix;
  typename  EigenVectorTransformType::MatrixType dMatrix;
  matrix.Fill(0.0);
  dMatrix.Fill(0.0);
  for (unsigned int i=0; i<InputDiffusionTensor3DType::Dimension; i++)
    {
    matrix(i,i) = 1.0;
    dMatrix(i,i) = 1.0;
    }

  JacobianType invJacobian;
  this->GetInverseJacobianOfForwardFieldWithRespectToPosition( point, invJacobian );

  for (unsigned int i=0; i<NDimensions; i++)
    {
    for (unsigned int j=0; j<NDimensions; j++)
      {
      if ( (i < InputDiffusionTensor3DType::Dimension) && (j < InputDiffusionTensor3DType::Dimension))
        {
        matrix(i,j) = invJacobian(i,j);
        dMatrix(i,j) = this->GetDirectionChangeMatrix()(i,j);
        }
      }
    }

  typename InputDiffusionTensor3DType::EigenValuesArrayType eigenValues;
  typename InputDiffusionTensor3DType::EigenVectorsMatrixType eigenVectors;
  inputTensor.ComputeEigenAnalysis( eigenValues, eigenVectors );

  InputTensorEigenVectorType ev1;
  InputTensorEigenVectorType ev2;
  InputTensorEigenVectorType ev3;

  for (unsigned int i=0; i<InputDiffusionTensor3DType::Dimension; i++)
    {
    ev1[i] = eigenVectors(2,i);
    ev2[i] = eigenVectors(1,i);
    }

  // Account for image direction changes between moving and fixed spaces
  ev1 = matrix * dMatrix * ev1;
  ev1.Normalize();

  // Get aspect of rotated e2 that is perpendicular to rotated e1
  ev2 = matrix * dMatrix * ev2;
  double dp = ev2 * ev1;
  if ( dp < 0 )
    {
    ev2 = ev2*(-1.0);
    dp = dp*(-1.0);
    }
  ev2 = ev2 - dp*ev1;
  ev2.Normalize();

  itk::CrossHelper<InputTensorEigenVectorType> vectorCross;
  ev3 = vectorCross( ev1, ev2 );

  // Outer product matrices
  typename EigenVectorTransformType::MatrixType e1;
  typename EigenVectorTransformType::MatrixType e2;
  typename EigenVectorTransformType::MatrixType e3;
  for (unsigned int i=0; i<InputDiffusionTensor3DType::Dimension; i++)
    {
    for (unsigned int j=0; j<InputDiffusionTensor3DType::Dimension; j++)
      {
      e1(i,j) = eigenValues[2] * ev1[i]*ev1[j];
      e2(i,j) = eigenValues[1] * ev2[i]*ev2[j];
      e3(i,j) = eigenValues[0] * ev3[i]*ev3[j];
      }
    }

  typename EigenVectorTransformType::MatrixType rotated = e1 + e2 + e3;

  OutputDiffusionTensor3DType result;     // Converted vector
  result[0] = rotated(0,0);
  result[1] = rotated(0,1);
  result[2] = rotated(0,2);
  result[3] = rotated(1,1);
  result[4] = rotated(1,2);
  result[5] = rotated(2,2);

  return result;
}

template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::OutputVectorPixelType
DisplacementFieldTransform<TScalar, NDimensions>
::TransformDiffusionTensor( const InputVectorPixelType& inputTensor, const InputPointType & point ) const
{
  if( !this->m_DisplacementField )
    {
    itkExceptionMacro( "No displacement field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  OutputVectorPixelType result( InputDiffusionTensor3DType::InternalDimension );     // Converted tensor
  result.Fill( 0.0 );

  InputDiffusionTensor3DType dt(0.0);
  const unsigned int tDim = inputTensor.Size();
  for (unsigned int i=0; i<tDim; i++)
    {
    dt[i] = inputTensor[i];
    }

  OutputDiffusionTensor3DType outDT = this->TransformDiffusionTensor( dt, point );

  for (unsigned int i=0; i<InputDiffusionTensor3DType::InternalDimension; i++)
    {
    result[i] = outDT[i];
    }

  return result;
}


/**
 * return an inverse transformation
 */
template<class TScalar, unsigned int NDimensions>
bool DisplacementFieldTransform<TScalar, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_InverseDisplacementField )
    {
    return false;
    }
  else
    {
    inverse->SetDisplacementField( this->m_InverseDisplacementField );
    inverse->SetInverseDisplacementField( this->m_DisplacementField );
    inverse->SetInterpolator( this->m_Interpolator );

    return true;
    }
}

// Return an inverse of this transform
template<class TScalar, unsigned int NDimensions>
typename DisplacementFieldTransform<TScalar, NDimensions>::InverseTransformBasePointer
DisplacementFieldTransform<TScalar, NDimensions>
::GetInverseTransform() const
{
  Pointer inverseTransform = New();
  if( this->GetInverse( inverseTransform ) )
    {
    return inverseTransform.GetPointer();
    }
  else
    {
    return NULL;
    }
}

/*
 * GetJacobian methods
 */

template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::GetJacobianWithRespectToPosition( const InputPointType & point,
                                      JacobianType & jacobian )
                                                                          const
{
  IndexType idx;
  this->m_DisplacementField->TransformPhysicalPointToIndex( point, idx );
  this->GetJacobianWithRespectToPosition( idx, jacobian );
}

template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::GetJacobianWithRespectToPosition( const IndexType & index,
                                      JacobianType & jacobian )
                                                                          const
{
  this->GetJacobianWithRespectToPositionInternal( index, jacobian, false );
}

template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::GetInverseJacobianOfForwardFieldWithRespectToPosition(
                                      const InputPointType & point,
                                      JacobianType & jacobian,
                                      bool useSVD )
                                                                          const
{
  IndexType idx;
  this->m_DisplacementField->TransformPhysicalPointToIndex( point, idx );
  this->GetInverseJacobianOfForwardFieldWithRespectToPosition( idx, jacobian,
                                                               useSVD );
}

template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::GetInverseJacobianOfForwardFieldWithRespectToPosition(
                                      const IndexType & index,
                                      JacobianType & jacobian,
                                      bool useSVD )
                                                                          const
{
  if (useSVD)
    {
    this->GetJacobianWithRespectToPositionInternal( index, jacobian, false );
    vnl_svd< typename JacobianType::ValueType > svd( jacobian );

    for (unsigned int i=0; i<jacobian.rows(); i++)
      for (unsigned int j=0; j<jacobian.cols(); j++)
        jacobian(i,j) = svd.inverse()(i,j);
    }
  else
    {
    this->GetJacobianWithRespectToPositionInternal( index, jacobian, true );
    }
}

/*
 * GetJacobianWithRespectToPositionInternal. Worker method.
 */
template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::GetJacobianWithRespectToPositionInternal( const IndexType & index,
                                      JacobianType & jacobian,
                                      bool doInverseJacobian )
                                                                          const
{
  jacobian.SetSize(NDimensions,NDimensions);
  //This may not be necessary. Double-check below.
  // jacobian.Fill(0.0);

  typename DisplacementFieldType::SizeType size =
                this->m_DisplacementField->GetLargestPossibleRegion().GetSize();
  typename DisplacementFieldType::SpacingType spacing =
                                        this->m_DisplacementField->GetSpacing();

  IndexType ddrindex;
  IndexType ddlindex;
  IndexType difIndex[NDimensions][2];

  // index offset
  unsigned int posoff = NumericTraits<unsigned int>::One;

  // space between indices
  TScalar space = NumericTraits<TScalar>::One;

  // minimum distance between neighbors
  TScalar mindist = NumericTraits<TScalar>::One;

  // flag indicating a valid location for jacobian calculation
  bool oktosample = true;

  // multiplier for getting inverse jacobian
  TScalar dPixSign = NumericTraits<TScalar>::One;
  dPixSign = doInverseJacobian ? -dPixSign : dPixSign;

  for (unsigned int row=0; row<NDimensions; row++)
    {
    TScalar dist = fabs((float)index[row]);
    if (dist < mindist)
      {
      oktosample = false;
      }
    dist = fabs((TScalar)size[row] - (TScalar)index[row]);
    if (dist < mindist)
      {
      oktosample = false;
      }
    }

  if ( oktosample )
    {
    OutputVectorType cpix = this->m_DisplacementField->GetPixel(index);
    m_DisplacementField->TransformLocalVectorToPhysicalVector( cpix, cpix );
    //cpix = directionRaw->TransformVector( cpix );

    // itkCentralDifferenceImageFunction does not support 4th order so
    // do manually here
    for(unsigned int row=0; row< NDimensions;row++)
      {
      difIndex[row][0]=index;
      difIndex[row][1]=index;
      ddrindex=index;
      ddlindex=index;
      if ((int) index[row] < (int)(size[row]-2) )
        {
        difIndex[row][0][row] = index[row]+posoff;
        ddrindex[row] = index[row]+posoff*2;
        }
      if (index[row] > 1 )
        {
        difIndex[row][1][row] = index[row]-1;
        ddlindex[row] = index[row]-2;
        }

      OutputVectorType rpix = m_DisplacementField->GetPixel( difIndex[row][1] );
      OutputVectorType lpix = m_DisplacementField->GetPixel( difIndex[row][0] );
      OutputVectorType rrpix = m_DisplacementField->GetPixel( ddrindex );
      OutputVectorType llpix = m_DisplacementField->GetPixel( ddlindex );

      m_DisplacementField->TransformLocalVectorToPhysicalVector( rpix, rpix );
      m_DisplacementField->TransformLocalVectorToPhysicalVector( rrpix, rrpix );
      m_DisplacementField->TransformLocalVectorToPhysicalVector( lpix, lpix );
      m_DisplacementField->TransformLocalVectorToPhysicalVector( llpix, llpix );

      //4th order centered difference
      OutputVectorType dPix =
          ( lpix*8.0 + llpix - rrpix - rpix*8.0 ) * space / (12.0) * dPixSign;

      //typename DisplacementFieldType::PixelType dPix=
      //      ( lpix - rpix )*space/(2.0*h); //2nd order centered difference

      for(unsigned int col=0; col< NDimensions; col++)
        {
        TScalar val = dPix[col] / spacing[col];
        if (row == col)
          {
          val += 1.0;
          }
        jacobian(col,row) = val;
        }
      } // for row
    } //if oktosample

  for (unsigned int jx = 0; jx < NDimensions; jx++)
    {
    for (unsigned int jy = 0; jy < NDimensions; jy++)
      {
      if ( !vnl_math_isfinite(jacobian(jx,jy))  )
        {
        oktosample = false;
        }
      }
    }

  if ( !oktosample )
    {
    jacobian.Fill(0.0);
    for (unsigned int i=0; i<NDimensions; i++)
      {
      jacobian(i,i) = 1.0;
      }
    }
}

template<class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType & update, ScalarType factor)
{
  //This simply adds the values.
  //TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters( update, factor );
}

template<class TScalar, unsigned int NDimensions>
void DisplacementFieldTransform<TScalar, NDimensions>
::SetDisplacementField( DisplacementFieldType* field )
{
  itkDebugMacro("setting DisplacementField to " << field);
  if ( this->m_DisplacementField != field )
    {
    this->m_DisplacementField = field;
    this->Modified();
    /* Store this separately for use in smoothing because we only want
     * to know when the displacement field object has changed, not just
     * its contents. */
    this->m_DisplacementFieldSetTime = this->GetMTime();
    if( ! this->m_Interpolator.IsNull() )
      {
      this->m_Interpolator->SetInputImage( this->m_DisplacementField );
      }
    //Assign to parameters object
    this->m_Parameters.SetParametersObject( this->m_DisplacementField );
    }
}

template<class TScalar, unsigned int NDimensions>
void DisplacementFieldTransform<TScalar, NDimensions>
::SetInterpolator( InterpolatorType* interpolator )
{
  itkDebugMacro("setting Interpolator to " << interpolator);
  if ( this->m_Interpolator != interpolator )
    {
    this->m_Interpolator = interpolator;
    this->Modified();
    if( ! this->m_DisplacementField.IsNull() )
      {
      this->m_Interpolator->SetInputImage( this->m_DisplacementField );
      }
    }
}

template <class TScalar, unsigned int NDimensions>
void
DisplacementFieldTransform<TScalar, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  std::cout << indent << "Interpolator: " << std::endl;
  std::cout << indent << indent << this->m_Interpolator << std::endl;

  if( this->m_DisplacementField )
    {
    std::cout << indent << "Displacement Field: " << std::endl;
    std::cout << indent << indent << this->m_DisplacementField << std::endl;
    }

  if( this->m_InverseDisplacementField )
    {
    std::cout << indent << "Inverse Displacement Field: " << std::endl;
    std::cout << indent << indent << this->m_InverseDisplacementField << std::endl;
    }
}
} // namespace itk

#endif
