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
#ifndef itkHessian3DToVesselnessMeasureImageFilter_hxx
#define itkHessian3DToVesselnessMeasureImageFilter_hxx

#include "itkHessian3DToVesselnessMeasureImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TPixel >
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::Hessian3DToVesselnessMeasureImageFilter()
{
  m_Alpha1 = 0.5;
  m_Alpha2 = 2.0;

  // Hessian( Image ) = Jacobian( Gradient ( Image ) )  is symmetric
  m_SymmetricEigenValueFilter = EigenAnalysisFilterType::New();
  m_SymmetricEigenValueFilter->SetDimension(ImageDimension);
  m_SymmetricEigenValueFilter->OrderEigenValuesBy(
    EigenAnalysisFilterType::FunctorType::OrderByValue);
}

template< typename TPixel >
void
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::GenerateData()
{
  itkDebugMacro(<< "Hessian3DToVesselnessMeasureImageFilter generating data ");

  m_SymmetricEigenValueFilter->SetInput( this->GetInput() );

  typename OutputImageType::Pointer output = this->GetOutput();

  typedef typename EigenAnalysisFilterType::OutputImageType
  EigenValueOutputImageType;

  m_SymmetricEigenValueFilter->Update();

  const typename EigenValueOutputImageType::ConstPointer eigenImage =
    m_SymmetricEigenValueFilter->GetOutput();

  // walk the region of eigen values and get the vesselness measure
  EigenValueArrayType                                   eigenValue;
  ImageRegionConstIterator< EigenValueOutputImageType > it;
  it = ImageRegionConstIterator< EigenValueOutputImageType >(
    eigenImage, eigenImage->GetRequestedRegion() );
  ImageRegionIterator< OutputImageType > oit;
  this->AllocateOutputs();
  oit = ImageRegionIterator< OutputImageType >( output,
                                                output->GetRequestedRegion() );
  oit.GoToBegin();
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    // Get the eigen value
    eigenValue = it.Get();

    // normalizeValue <= 0 for bright line structures
    double normalizeValue = std::min(-1.0 * eigenValue[1], -1.0 * eigenValue[0]);

    // Similarity measure to a line structure
    if ( normalizeValue > 0 )
      {
      double lineMeasure;
      if ( eigenValue[2] <= 0 )
        {
        lineMeasure =
          std::exp( -0.5 * itk::Math::sqr( eigenValue[2] / ( m_Alpha1 * normalizeValue ) ) );
        }
      else
        {
        lineMeasure =
          std::exp( -0.5 * itk::Math::sqr( eigenValue[2] / ( m_Alpha2 * normalizeValue ) ) );
        }

      lineMeasure *= normalizeValue;
      oit.Set( static_cast< OutputPixelType >( lineMeasure ) );
      }
    else
      {
      oit.Set(NumericTraits< OutputPixelType >::ZeroValue());
      }

    ++it;
    ++oit;
    }
}

template< typename TPixel >
void
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
}
} // end namespace itk

#endif
