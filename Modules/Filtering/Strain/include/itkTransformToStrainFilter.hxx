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
#ifndef __itkTransformToStrainFilter_hxx
#define __itkTransformToStrainFilter_hxx

#include "itkTransformToStrainFilter.h"

namespace itk
{

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
TransformToStrainFilter<TInputImage, TOperatorValueType, TOutputValueType>::TransformToStrainFilter()
  : m_StrainForm(INFINITESIMAL)
{}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
void
TransformToStrainFilter<TInputImage, TOperatorValueType, TOutputValueType>::BeforeThreadedGenerateData()
{}


template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
void
TransformToStrainFilter<TInputImage, TOperatorValueType, TOutputValueType>::ThreadedGenerateData(
  const OutputRegionType & region,
  ThreadIdType             itkNotUsed(threadId))
{
  // typename InputImageType::ConstPointer input = this->GetInput();

  // OutputImageType * output = this->GetOutput();

  // ImageRegionIterator< OutputImageType > outputIt( output, region );
  //// First fill the outputs with zeros.  Better way to do this?  FillBuffer does
  //// not seem to work.
  // typename OutputImageType::PixelType outputPixel;
  // outputPixel.Fill( itk::NumericTraits< TOutputValueType >::Zero );
  //// @todo use .Value() here?
  // for( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt )
  // outputIt.Set( outputPixel );
  // unsigned int            j;
  // unsigned int            k;
  // GradientOutputPixelType gradientPixel;

  //// e_ij += 1/2( du_i/dx_j + du_j/dx_i )
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
  // for( j = 0; j < i; ++j )
  //{
  //// @todo use .Value() here?
  // outputPixel( i, j ) += gradientPixel[j] / static_cast< TOutputValueType >( 2 );
  // }
  //// j == i
  // outputPixel( i, i ) += gradientPixel[i];
  // for( j = i + 1; j < ImageDimension; ++j )
  //{
  // outputPixel( i, j ) += gradientPixel[j] / static_cast< TOutputValueType >( 2 );
  // }
  // outputIt.Set( outputPixel );
  // }
  //}
  // switch( m_StrainForm )
  //{
  // case INFINITESIMAL:
  // break;
  //// e_ij += 1/2 du_m/du_i du_m/du_j
  // case GREENLAGRANGIAN:
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
  // outputPixel( j, k ) += gradientPixel[j] * gradientPixel[k] / static_cast< TOutputValueType >( 2 );
  // }
  //}
  // outputIt.Set( outputPixel );
  // }
  //}
  // break;
  //// e_ij -= 1/2 du_m/du_i du_m/du_j
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
  // outputPixel( j, k ) -= gradientPixel[j] * gradientPixel[k] / static_cast< TOutputValueType >( 2 );
  // }
  //}
  // outputIt.Set( outputPixel );
  // }
  //}
  // break;
  // default:
  // itkExceptionMacro( << "Unknown strain form." );
  // }
}

} // end namespace itk

#endif
