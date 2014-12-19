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
#ifndef itkEigenAnalysis2DImageFilter_hxx
#define itkEigenAnalysis2DImageFilter_hxx

#include "itkEigenAnalysis2DImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::EigenAnalysis2DImageFilter()
{
  this->SetNumberOfRequiredInputs(3);
  this->SetNumberOfRequiredOutputs(3);
  this->SetNthOutput( 0, this->MakeOutput(0) );
  this->SetNthOutput( 1, this->MakeOutput(1) );
  this->SetNthOutput( 2, this->MakeOutput(2) );
}

/**
 * Connect one the image containing the [0,0] elements of the input matrix
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
void
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::SetInput1(TInputImage *image)
{
  this->SetNthInput(0, image);
}

/**
 * Connect one the image containing the [0,1] elements of the input matrix
 * this element is the same [1,0] because this filter assumes symmetric
 * matrices
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
void
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::SetInput2(TInputImage *image)
{
  this->SetNthInput(1, image);
}

/**
 * Connect one the image containing the [1,1] elements of the input matrix
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
void
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::SetInput3(TInputImage *image)
{
  this->SetNthInput(2, image);
}

/**
 * Get the largest eigenvalue considering the sign
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
typename EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >::EigenValueImageType *
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::GetMaxEigenValue(void)
{
  return dynamic_cast< EigenValueImageType * >(
           this->ProcessObject::GetOutput(0) );
}

/**
 * Get the smallest eigenvalue considering the sign
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
typename EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >::EigenValueImageType *
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::GetMinEigenValue(void)
{
  return dynamic_cast< EigenValueImageType * >(
           this->ProcessObject::GetOutput(1) );
}

/**
 * Get the eigenvector corresponding to the largest eigenvalue (considering the sign)
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
typename EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >::EigenVectorImageType *
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::GetMaxEigenVector(void)
{
  EigenVectorImageType *eigenVector = dynamic_cast< EigenVectorImageType * >(
    this->ProcessObject::GetOutput(2) );

  if ( eigenVector )
    {
    return eigenVector;
    }
  else
    {
    itkWarningMacro(

      <<
      "EigenAnalysis2DImageFilter::GetMaxEigenVector(): dynamic_cast has failed. A reinterpret_cast is being attempted."
      << std::endl << "Type name is: "
      << typeid( *this->GetOutput(2) ).name() );
    return reinterpret_cast< EigenVectorImageType * >(
             this->ProcessObject::GetOutput(2) );
    }
}

/**
 *   Make Ouput
 * \todo Verify that MakeOutput is createing the right type of objects
 *  this could be the cause of the reinterpret_cast bug in this class
 */
template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
DataObject::Pointer
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::MakeOutput(DataObjectPointerArraySizeType idx)
{
  DataObject::Pointer output;

  switch ( idx )
    {
    case 0:
      output = ( EigenValueImageType::New() ).GetPointer();
      break;
    case 1:
      output = ( EigenValueImageType::New() ).GetPointer();
      break;
    case 2:
      output = ( EigenVectorImageType::New() ).GetPointer();
      break;
    }
  return output.GetPointer();
}

template< typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage >
void
EigenAnalysis2DImageFilter< TInputImage, TEigenValueImage, TEigenVectorImage >
::GenerateData()
{
  typename TInputImage::ConstPointer inputPtr1(
    dynamic_cast< const TInputImage * >(
      ( ProcessObject::GetInput(0) ) ) );

  typename TInputImage::ConstPointer inputPtr2(
    dynamic_cast< const TInputImage * >(
      ( ProcessObject::GetInput(1) ) ) );

  typename TInputImage::ConstPointer inputPtr3(
    dynamic_cast< const TInputImage * >(
      ( ProcessObject::GetInput(2) ) ) );

  EigenValueImagePointer  outputPtr1 = this->GetMaxEigenValue();
  EigenValueImagePointer  outputPtr2 = this->GetMinEigenValue();
  EigenVectorImagePointer outputPtr3 = this->GetMaxEigenVector();

  outputPtr1->SetBufferedRegion( inputPtr1->GetBufferedRegion() );
  outputPtr2->SetBufferedRegion( inputPtr1->GetBufferedRegion() );
  outputPtr3->SetBufferedRegion( inputPtr1->GetBufferedRegion() );

  outputPtr1->Allocate();
  outputPtr2->Allocate();
  outputPtr3->Allocate();

  EigenValueImageRegionType region = outputPtr1->GetRequestedRegion();

  ImageRegionConstIteratorWithIndex< TInputImage > inputIt1(inputPtr1, region);
  ImageRegionConstIteratorWithIndex< TInputImage > inputIt2(inputPtr2, region);
  ImageRegionConstIteratorWithIndex< TInputImage > inputIt3(inputPtr3, region);

  ImageRegionIteratorWithIndex< EigenValueImageType >  outputIt1(outputPtr1, region);
  ImageRegionIteratorWithIndex< EigenValueImageType >  outputIt2(outputPtr2, region);
  ImageRegionIteratorWithIndex< EigenVectorImageType > outputIt3(outputPtr3, region);

  EigenVectorType nullVector;
  nullVector.Fill(0.0);

  // support progress methods/callbacks
  ProgressReporter progress( this, 0, region.GetNumberOfPixels() );

  inputIt1.GoToBegin();
  inputIt2.GoToBegin();
  inputIt3.GoToBegin();

  outputIt1.GoToBegin();
  outputIt2.GoToBegin();
  outputIt3.GoToBegin();

  EigenVectorType eigenVector;

  while ( !inputIt1.IsAtEnd() )
    {
    const double xx = static_cast< double >( inputIt1.Get() );
    const double xy = static_cast< double >( inputIt2.Get() );
    const double yy = static_cast< double >( inputIt3.Get() );

    const double dxy = xx - yy;
    const double sxy = xx + yy;

    const double S = std::sqrt(dxy * dxy + 4.0 * xy * xy);

    const double pp =  ( sxy + S ) / 2.0;
    const double qq =  ( sxy - S ) / 2.0;

    outputIt1.Set(pp);
    outputIt2.Set(qq);

    eigenVector[0] = static_cast< VectorComponentType >( ( -dxy - S ) / 2.0 );
    eigenVector[1] = static_cast< VectorComponentType >( -xy );

    const VectorComponentType norm = eigenVector.GetNorm();
    if ( norm > 1e-30 )
      {
      outputIt3.Set(eigenVector / norm);
      }
    else
      {
      outputIt3.Set(nullVector);
      }

    ++inputIt1;
    ++inputIt2;
    ++inputIt3;

    ++outputIt1;
    ++outputIt2;
    ++outputIt3;

    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif
