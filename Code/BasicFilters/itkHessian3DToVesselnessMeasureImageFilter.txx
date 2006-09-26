/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHessian3DToVesselnessMeasureImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHessian3DToVesselnessMeasureImageFilter_txx
#define _itkHessian3DToVesselnessMeasureImageFilter_txx

#include "itkHessian3DToVesselnessMeasureImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Constructor
 */
template < typename TPixel >
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::Hessian3DToVesselnessMeasureImageFilter()
{
  m_Alpha1 = 0.5;
  m_Alpha2 = 2.0;

  // Hessian( Image ) = Jacobian( Gradient ( Image ) )  is symmetric
  m_SymmetricEigenValueFilter = EigenAnalysisFilterType::New();
  m_SymmetricEigenValueFilter->SetDimension( ImageDimension );
  m_SymmetricEigenValueFilter->OrderEigenValuesBy( 
      EigenAnalysisFilterType::FunctorType::OrderByValue );
  
}


template < typename TPixel >
void 
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::GenerateData()
{
  itkDebugMacro(<< "Hessian3DToVesselnessMeasureImageFilter generating data ");

  m_SymmetricEigenValueFilter->SetInput( this->GetInput() );
  
  typename OutputImageType::Pointer output = this->GetOutput();

  typedef typename EigenAnalysisFilterType::OutputImageType
                                                    EigenValueImageType;

  m_SymmetricEigenValueFilter->Update();
  
  const typename EigenValueImageType::ConstPointer eigenImage = 
                    m_SymmetricEigenValueFilter->GetOutput();
  
  // walk the region of eigen values and get the vesselness measure
  EigenValueArrayType eigenValue;
  ImageRegionConstIterator<EigenValueImageType> it;
  it = ImageRegionConstIterator<EigenValueImageType>(
      eigenImage, eigenImage->GetRequestedRegion());
  ImageRegionIterator<OutputImageType> oit;
  this->AllocateOutputs();
  oit = ImageRegionIterator<OutputImageType>(output,
                                             output->GetRequestedRegion());
  oit.GoToBegin();
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    // Get the eigen value
    eigenValue = it.Get();
    
    // normalizeValue <= 0 for bright line structures
    double normalizeValue = vnl_math_min( -1.0 * eigenValue[1], -1.0 * eigenValue[0]);

    // Similarity measure to a line structure
    if( normalizeValue > 0 )
      {
      double lineMeasure;
      if( eigenValue[2] <= 0 )
        {
        lineMeasure = 
          vcl_exp(-0.5 * vnl_math_sqr( eigenValue[2] / (m_Alpha1 * normalizeValue)));
        }
      else
        {
        lineMeasure = 
          vcl_exp(-0.5 * vnl_math_sqr( eigenValue[2] / (m_Alpha2 * normalizeValue)));
        }
      
      lineMeasure *= normalizeValue;
      oit.Set( static_cast< OutputPixelType >(lineMeasure) );
      }
    else
      {
      oit.Set( NumericTraits< OutputPixelType >::Zero );
      }    
    

    ++it;
    ++oit;
    }
    
}

template < typename TPixel >
void
Hessian3DToVesselnessMeasureImageFilter< TPixel >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
}


} // end namespace itk
  
#endif
