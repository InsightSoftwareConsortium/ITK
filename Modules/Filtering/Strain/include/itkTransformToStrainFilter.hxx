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
#ifndef itkTransformToStrainFilter_hxx
#define itkTransformToStrainFilter_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkTransformToStrainFilter.h"

namespace itk
{

template <typename TTransform, typename TOperatorValue, typename TOutputValue>
TransformToStrainFilter<TTransform, TOperatorValue, TOutputValue>::TransformToStrainFilter()
  : m_StrainForm(INFINITESIMAL)
{}

template <typename TTransform, typename TOperatorValue, typename TOutputValue>
void
TransformToStrainFilter<TTransform, TOperatorValue, TOutputValue>::BeforeThreadedGenerateData()
{}


template <typename TTransform, typename TOperatorValue, typename TOutputValue>
void
TransformToStrainFilter<TTransform, TOperatorValue, TOutputValue>::ThreadedGenerateData(
  const OutputRegionType & region,
  ThreadIdType             itkNotUsed(threadId))
{
  const TransformType * input = this->GetTransform();

  OutputImageType * output = this->GetOutput();
  output->FillBuffer(NumericTraits<OutputPixelType>::Zero);
  typedef ImageRegionIteratorWithIndex<OutputImageType> ImageIteratorType;
  ImageIteratorType                                     outputIt(output, region);

  // e_ij += 1/2( du_i/dx_j + du_j/dx_i )
  for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
  {
    const typename OutputImageType::IndexType index = outputIt.GetIndex();
    typename OutputImageType::PointType       point;
    output->TransformIndexToPhysicalPoint(index, point);
    typename TransformType::JacobianType jacobian;
    input->ComputeJacobianWithRespectToPosition(point, jacobian);
    typename OutputImageType::PixelType outputPixel = outputIt.Get();
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
      for (unsigned int jj = 0; jj < ImageDimension; ++jj)
      {
        outputPixel(ii, jj) += jacobian(ii, jj) / static_cast<TOutputValue>(2);
      }
    }
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
      outputPixel(ii, ii) += jacobian(ii, ii) / static_cast<TOutputValue>(2) - static_cast<TOutputValue>(1);
    }
    outputIt.Set(outputPixel);
  }
  switch (m_StrainForm)
  {
    case INFINITESIMAL:
      break;
    // e_ij += 1/2 du_m/du_i du_m/du_j
    case GREENLAGRANGIAN:
      for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
      {
        const typename OutputImageType::IndexType index = outputIt.GetIndex();
        typename OutputImageType::PointType       point;
        output->TransformIndexToPhysicalPoint(index, point);
        typename TransformType::JacobianType jacobian;
        input->ComputeJacobianWithRespectToPosition(point, jacobian);
        typename OutputImageType::PixelType outputPixel = outputIt.Get();
        for (unsigned int ii = 0; ii < ImageDimension; ++ii)
        {
          for (unsigned int jj = 0; jj < ImageDimension; ++jj)
          {
            for (unsigned int kk = 0; kk <= jj; ++kk)
            {
              // outputPixel( jj, kk ) += jacobian( ii, jj ) * jacobian( ii, kk ) / static_cast< TOutputValue >( 2 );
            }
          }
        }
        outputIt.Set(outputPixel);
      }
      // for( unsigned int i = 0; i < ImageDimension; ++i )
      //{
      // itk::ImageRegionConstIterator< GradientOutputImageType >
      // gradientIt( reinterpret_cast< GradientOutputImageType* >(
      // dynamic_cast< GradientOutputImageType* >(
      // this->ProcessObject::GetOutput( i + 1 ) ) )
      //, region );
      // for( outputIt.GoToBegin(), gradientIt.GoToBegin();
      //! gradientIt.IsAtEnd();
      //++outputIt, ++gradientIt )
      //{
      // outputPixel = outputIt.Get();
      // gradientPixel = gradientIt.Get();
      // for( j = 0; j < ImageDimension; ++j )
      //{
      // for( k = 0; k <= j; ++k )
      //{
      //// @todo use .Value() here?
      // outputPixel( j, k ) += gradientPixel[j] * gradientPixel[k] / static_cast< TOutputValue >( 2 );
      // }
      //}
      // outputIt.Set( outputPixel );
      // }
      //}
      break;
      // e_ij -= 1/2 du_m/du_i du_m/du_j
      // case EULERIANALMANSI:
      // for( unsigned int i = 0; i < ImageDimension; ++i )
      //{
      // itk::ImageRegionConstIterator< GradientOutputImageType >
      // gradientIt( reinterpret_cast< GradientOutputImageType* >(
      // dynamic_cast< GradientOutputImageType* >(
      // this->ProcessObject::GetOutput( i + 1 ) ) )
      //, region );
      // for( outputIt.GoToBegin(), gradientIt.GoToBegin();
      //! gradientIt.IsAtEnd();
      //++outputIt, ++gradientIt )
      //{
      // outputPixel = outputIt.Get();
      // gradientPixel = gradientIt.Get();
      // for( j = 0; j < ImageDimension; ++j )
      //{
      // for( k = 0; k <= j; ++k )
      //{
      //// @todo use .Value() here?
      // outputPixel( j, k ) -= gradientPixel[j] * gradientPixel[k] / static_cast< TOutputValue >( 2 );
      // }
      //}
      // outputIt.Set( outputPixel );
      // }
      //}
      // break;
    default:
      itkExceptionMacro(<< "Unknown strain form.");
  }
}

} // end namespace itk

#endif
