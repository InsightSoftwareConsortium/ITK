/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiffusionTensor3DReconstructionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDiffusionTensor3DReconstructionImageFilter_txx
#define __itkDiffusionTensor3DReconstructionImageFilter_txx

#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkArray.h"
#include "vnl/vnl_vector.h"


namespace itk {

template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::DiffusionTensor3DReconstructionImageFilter()
{
  this->SetNumberOfRequiredInputs( 6 ); // At least 6 inputs are necessary
  m_GradientDirectionContainer = GradientDirectionContainerType::New();
  m_NumberOfGradientDirections = 0;
  m_Threshold = NumericTraits< ReferencePixelType >::min();
}


template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::BeforeThreadedGenerateData()
{
  this->ComputeTensorBasis();
}


// POTENTIAL WARNING:
//
// There are several static variable definitions in dsvdc, that is called
// by vnl_svd. (used to compute the psudo-inverse to find the dual tensor basis).
// Until we find a solution, we will need to set the number of threads to this
// filter to 1.
template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int ) 
{
  typename OutputImageType::Pointer outputImage = 
            static_cast< OutputImageType * >(this->ProcessObject::GetOutput(0));
  
  ImageRegionIterator< OutputImageType > oit(outputImage, outputRegionForThread);
  ImageRegionConstIterator< ReferenceImageType > 
                  it(static_cast< ReferenceImageType * >(this->ProcessObject::GetInput(0)), 
                      outputRegionForThread);
  typedef ImageRegionConstIterator< GradientImageType > GradientIteratorType;
  
  std::vector< GradientIteratorType * > gradientItContainer;
  
  it.GoToBegin();
  oit.GoToBegin();
  
  vnl_vector< double > B(m_NumberOfGradientDirections);
  
  for( unsigned int i = 1; i<= m_NumberOfGradientDirections; i++ )
    {
    typename GradientImageType::Pointer gradientImagePointer = 
        static_cast< GradientImageType * >( this->ProcessObject::GetInput(i) );
    GradientIteratorType *git = new GradientIteratorType( 
                        gradientImagePointer, outputRegionForThread );
    git->GoToBegin();
    gradientItContainer.push_back(git);
    }
  
  vnl_vector<double> D(6);
  
  while( !it.IsAtEnd() )
    {
    ReferencePixelType b0 = it.Get();
    
    TensorPixelType tensor(0.0);

    if( (b0 != 0) && (b0 >= m_Threshold) )
      {
      for( unsigned int i = 0; i< m_NumberOfGradientDirections; i++ )
        {
        GradientPixelType b = gradientItContainer[i]->Get();
       
        if( b == 0 )
          {
          B[i] = 0;
          }
        else
          {
          B[i] = -log( static_cast<double>(b) / static_cast<double>(b0) );
          }
      
        ++(*gradientItContainer[i]);  
        }
      
      vnl_svd< double > pseudoInverseSolver( m_TensorBasis );
      if( m_NumberOfGradientDirections > 6 )
        {
        D = pseudoInverseSolver.solve( m_Coeffs * B );
        }
      else
        {
        D = pseudoInverseSolver.solve( B );
        }
        
      tensor(0,0) = D[0];
      tensor(0,1) = D[1]; 
      tensor(0,2) = D[2];
      tensor(1,1) = D[3]; 
      tensor(1,2) = D[4];
      tensor(2,2) = D[5];
      
      
      }
    else
      {
      for( unsigned int i = 0; i< m_NumberOfGradientDirections; i++ )
        {
        ++(*gradientItContainer[i]);  
        }
      }

      
    oit.Set( tensor );
    ++oit;
    ++it;
    }

  for( unsigned int i = 0; i< gradientItContainer.size(); i++ )
    {
    delete gradientItContainer[i];
    }
}


template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::ComputeTensorBasis()
{
  if( m_NumberOfGradientDirections < 6 )
    {
    itkExceptionMacro( << "Not enough gradient directions supplied. Need to supply at least 6" );
    }

  m_Coeffs.set_size( m_NumberOfGradientDirections, 6 );
  for (int m = 0; m < m_NumberOfGradientDirections; m++)
    {
    m_Coeffs[m][0] =     m_GradientDirectionContainer->ElementAt(m)[0] * m_GradientDirectionContainer->ElementAt(m)[0];
    m_Coeffs[m][1] = 2 * m_GradientDirectionContainer->ElementAt(m)[0] * m_GradientDirectionContainer->ElementAt(m)[1];
    m_Coeffs[m][2] = 2 * m_GradientDirectionContainer->ElementAt(m)[0] * m_GradientDirectionContainer->ElementAt(m)[2];
    m_Coeffs[m][3] =     m_GradientDirectionContainer->ElementAt(m)[1] * m_GradientDirectionContainer->ElementAt(m)[1];
    m_Coeffs[m][4] = 2 * m_GradientDirectionContainer->ElementAt(m)[1] * m_GradientDirectionContainer->ElementAt(m)[2];
    m_Coeffs[m][5] =     m_GradientDirectionContainer->ElementAt(m)[2] * m_GradientDirectionContainer->ElementAt(m)[2];
    }
 
  if( m_NumberOfGradientDirections > 6 )
    {
    m_TensorBasis = m_Coeffs.transpose() * m_Coeffs;
    }
  else
    {
    m_TensorBasis = m_Coeffs;
    }

  m_Coeffs.inplace_transpose();
    
}


template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::AddGradientImage( const GradientDirectionType &gradientDirection, 
                        const GradientImageType *gradientImage )
{
  m_GradientDirectionContainer->InsertElement( 
              m_NumberOfGradientDirections, gradientDirection );
  ++m_NumberOfGradientDirections;
  this->ProcessObject::SetNthInput( m_NumberOfGradientDirections, 
      const_cast< GradientImageType* >(gradientImage) );
}


template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "TensorBasisMatrix: " << m_TensorBasis << std::endl;
  os << indent << "Coeffs: " << m_Coeffs << std::endl;
  os << indent << "GradientDirectionContainer: "
     << m_GradientDirectionContainer << std::endl;
  os << indent << "NumberOfGradientDirections: " << 
              m_NumberOfGradientDirections << std::endl;
  os << indent << "Threshold for reference B0 image: " << m_Threshold << std::endl;
}

}

#endif
