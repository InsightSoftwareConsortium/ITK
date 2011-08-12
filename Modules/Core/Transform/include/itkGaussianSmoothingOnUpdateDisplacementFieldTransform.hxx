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
#ifndef __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx
#define __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
  m_GaussianSmoothingSigma = 3;
  m_GaussianSmoothingTempFieldModifiedTime = 0;
  /** These are init'ed when SmoothDeformatFieldGauss is called, either for
   * the first time, or after a new displacement field has been assigned. */
  m_GaussianSmoothingTempField = NULL;
  m_GaussianSmoothingSmoother = NULL;
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
~GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
}

template<class TScalar, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType & update, ScalarType factor)
{
  //This simply adds the values.
  //TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters( update, factor );

  //Now we smooth the result. Not thread safe. Does it's own
  // threading.
  GaussianSmoothDisplacementField();
}

template<class TScalar, unsigned int NDimensions>
void GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::GaussianSmoothDisplacementField()
{
  itkDebugMacro(" enter gauss smooth. m_GaussianSmoothingSigma: "
                << m_GaussianSmoothingSigma);
  if( this->m_GaussianSmoothingSigma <= 0 )
    {
    return;
    }

  typename DisplacementFieldType::Pointer field = this->GetDisplacementField();

  /* Allocate temp field if new displacement field has been set.
   * We only want to allocate this field if this method is used */
  if( this->GetDisplacementFieldSetTime() >
      this->m_GaussianSmoothingTempFieldModifiedTime )
    {
    this->m_GaussianSmoothingTempFieldModifiedTime = this->GetMTime();
    m_GaussianSmoothingTempField = DisplacementFieldType::New();
    m_GaussianSmoothingTempField->SetSpacing( field->GetSpacing() );
    m_GaussianSmoothingTempField->SetOrigin( field->GetOrigin() );
    m_GaussianSmoothingTempField->SetDirection( field->GetDirection() );
    m_GaussianSmoothingTempField->SetLargestPossibleRegion(
                                          field->GetLargestPossibleRegion() );
    m_GaussianSmoothingTempField->SetRequestedRegion(
                                                field->GetRequestedRegion() );
    m_GaussianSmoothingTempField->SetBufferedRegion(
                                                field->GetBufferedRegion() );
    m_GaussianSmoothingTempField->Allocate();

    //This should only be allocated once as well, for efficiency.
    m_GaussianSmoothingSmoother = GaussianSmoothingSmootherType::New();
    }

  if( m_GaussianSmoothingTempField.IsNull() )
    {
    itkExceptionMacro("Expected m_GaussianSmoothingTempField to be allocated.");
    }

  typedef typename DisplacementFieldType::PixelType   VectorType;

  typedef typename DisplacementFieldType::PixelContainerPointer
                                                        PixelContainerPointer;
  // I think we need to keep this as SmartPointer type, to preserve the
  // reference counting so we can assign the swapPtr to the main field and
  // not have to do a memory copy - this happens when image dimensions are odd.
  PixelContainerPointer swapPtr;

  // graft the output field onto the mini-pipeline
  m_GaussianSmoothingSmoother->GraftOutput( m_GaussianSmoothingTempField );

  for( unsigned int j = 0; j < Superclass::Dimension; j++ )
    {
    // smooth along this dimension
    m_GaussianSmoothingOperator.SetDirection( j );
    m_GaussianSmoothingOperator.SetVariance( m_GaussianSmoothingSigma );
    m_GaussianSmoothingOperator.SetMaximumError(0.001 );
    m_GaussianSmoothingOperator.SetMaximumKernelWidth( 256 );
    m_GaussianSmoothingOperator.CreateDirectional();

    // todo: make sure we only smooth within the buffered region
    m_GaussianSmoothingSmoother->SetOperator( m_GaussianSmoothingOperator );
    m_GaussianSmoothingSmoother->SetInput( field );
    try
      {
      m_GaussianSmoothingSmoother->Update();
      }
    catch( ExceptionObject & exc )
      {
      std::string msg("Caught exception: ");
      msg += exc.what();
      itkExceptionMacro( << msg );
      }

    if( j < Superclass::Dimension - 1 )
      {
      // swap the containers
      swapPtr = m_GaussianSmoothingSmoother->GetOutput()->GetPixelContainer();
      m_GaussianSmoothingSmoother->GraftOutput( field );
      // SetPixelContainer does a smartpointer assignment, so the pixel
      // container won't be deleted if field  points to the
      // temporary field upon exiting this method.
      field->SetPixelContainer( swapPtr );
      m_GaussianSmoothingSmoother->Modified();
      }
    }

  if( Superclass::Dimension % 2 == 0 )
    {
    // For even number of dimensions, the final pass writes the output
    // into field's original pixel container, so we just point back to that.
    // And point the temporary field back to its original container for next
    // time through.
    m_GaussianSmoothingTempField->SetPixelContainer(
                                                  field->GetPixelContainer() );
    field->SetPixelContainer(
               m_GaussianSmoothingSmoother->GetOutput()->GetPixelContainer() );
    }

  //make sure boundary does not move
  ScalarType weight = 1.0;
  if (m_GaussianSmoothingSigma < 0.5)
    {
    weight=1.0 - 1.0 * ( this->m_GaussianSmoothingSigma / 0.5);
    }
  ScalarType weight2 = 1.0 - weight;
  typedef ImageRegionIteratorWithIndex<DisplacementFieldType> Iterator;
  typename DisplacementFieldType::SizeType size =
                                field->GetLargestPossibleRegion().GetSize();
  Iterator outIter( field, field->GetLargestPossibleRegion() );
  for( outIter.GoToBegin(); !outIter.IsAtEnd(); ++outIter )
  {
    bool onboundary=false;
    typename DisplacementFieldType::IndexType index= outIter.GetIndex();
    for (int i=0; i < Superclass::Dimension; i++)
      {
      if (index[i] < 1 || index[i] >= static_cast<int>( size[i] )-1 )
        {
        onboundary=true;
        }
      }
    if( onboundary )
      {
      VectorType vec;
      vec.Fill(0.0);
      outIter.Set(vec);
      }
    else
      {
      VectorType
          svec = m_GaussianSmoothingSmoother->GetOutput()->GetPixel( index );
      outIter.Set( svec * weight + outIter.Get() * weight2);
      }
  }

  itkDebugMacro("done gauss smooth ");
}

template <class TScalar, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "Gaussian smoothing parameters: " << std::endl
     << indent << "m_GaussianSmoothingSigma: " << m_GaussianSmoothingSigma
     << std::endl
     << "m_GaussianSmoothingTempFieldModifiedTime: "
     << m_GaussianSmoothingTempFieldModifiedTime
     << std::endl
     << "m_GaussianSmoothingTempField: " << m_GaussianSmoothingTempField
     << std::endl
     << "m_GaussianSmoothingSmoother: "
     << m_GaussianSmoothingSmoother << std::endl;
}
} // namespace itk

#endif
