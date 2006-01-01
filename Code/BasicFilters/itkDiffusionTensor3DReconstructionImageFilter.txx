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
  // At least 2 inputs are necessary, (gradient and reference image)
  this->SetNumberOfRequiredInputs( 2 ); 
  m_NumberOfGradientDirections = 0;
  m_Threshold = NumericTraits< ReferencePixelType >::min();
  m_GradientImageTypeEnumeration = Else;
  m_GradientDirectionContainer = NULL;
  m_TensorBasis.set_identity();
}


template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::BeforeThreadedGenerateData()
{
  // If we have more than 2 inputs, then each input, except the first is a 
  // gradient image. The number of gradient images must match the number of
  // gradient directions.
  const unsigned int numberOfInputs = this->GetNumberOfInputs();

  // There need to be at least 6 gradient directions to be able to compute the 
  // tensor basis
  if( m_NumberOfGradientDirections < 6 )
    {
    itkExceptionMacro( << "At least 6 gradient directions are required" );
    }
    
  // If there is only 1 gradient image, it must be an itk::VectorImage. Otherwise 
  // we must have a container of (numberOfInputs-1) itk::Image. Check to make sure
  if ( numberOfInputs == 2  
      && m_GradientImageTypeEnumeration != GradientIsInASingleImage )
    {
    std::string gradientImageClassName(
        this->ProcessObject::GetInput(1)->GetNameOfClass());
    if ( strcmp(gradientImageClassName.c_str(),"VectorImage") != 0 )
      {
      itkExceptionMacro( << 
          "There is only one Gradient image. I expect that to be a VectorImage. "
          << "But its of type: " << gradientImageClassName );
      }
    }
    
  this->ComputeTensorBasis();
}


// POTENTIAL WARNING:
//
// Until we fix netlib svd routines, we will need to set the number of thread
// to 1.
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
  it.GoToBegin();
  oit.GoToBegin();

  vnl_vector< double > B(m_NumberOfGradientDirections);
  vnl_vector<double> D(6);
    
  // Two cases here .
  // 1. If the Gradients have been specified in multiple images, we will create
  // 'n' iterators for each of the gradient images and solve the Stejskal-Tanner
  // equations for every pixel. 
  // 2. If the Gradients have been specified in a single multi-component image,
  // one iterator will suffice to do the same.

  if( m_GradientImageTypeEnumeration == GradientIsInManyImages )
    {
    typedef ImageRegionConstIterator< GradientImageType > GradientIteratorType;
    std::vector< GradientIteratorType * > gradientItContainer;
    
    for( unsigned int i = 1; i<= m_NumberOfGradientDirections; i++ )
      {
      typename GradientImageType::Pointer gradientImagePointer = NULL;
      
      // Would have liked a dynamic_cast here, but seems SGI doesn't like it
      // The enum will ensure that an inappropriate cast is not done
      gradientImagePointer = static_cast< GradientImageType * >( 
                                this->ProcessObject::GetInput(i) );
      
      GradientIteratorType *git = new GradientIteratorType( 
                          gradientImagePointer, outputRegionForThread );
      git->GoToBegin();
      gradientItContainer.push_back(git);
      }
    
    // Iterate over the reference and gradient images and solve the steskal
    // equations to reconstruct the Diffusion tensor.
    // See splweb.bwh.harvard.edu:8000/pages/papers/westin/ISMRM2002.pdf
    // "A Dual Tensor Basis Solution to the Stejskal-Tanner Equations for DT-MRI"

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
  // The gradients are specified in a single multi-component image
  else if( m_GradientImageTypeEnumeration == GradientIsInASingleImage )
    {
    typedef ImageRegionConstIterator< GradientImagesType > GradientIteratorType;
    typedef typename GradientImagesType::PixelType         GradientVectorType;
    typename GradientImagesType::Pointer gradientImagePointer = NULL;
    
    // Would have liked a dynamic_cast here, but seems SGI doesn't like it
    // The enum will ensure that an inappropriate cast is not done
    gradientImagePointer = static_cast< GradientImagesType * >( 
                              this->ProcessObject::GetInput(1) );
    
    GradientIteratorType git(gradientImagePointer, outputRegionForThread );
    git.GoToBegin();

    while( !it.IsAtEnd() )
      {
      ReferencePixelType b0 = it.Get();
      
      TensorPixelType tensor(0.0);

      if( (b0 != 0) && (b0 >= m_Threshold) )
        {
        GradientVectorType b = git.Get();
        for( unsigned int i = 0; i< m_NumberOfGradientDirections; i++ )
          {
          if( b[i] == 0 )
            {
            B[i] = 0;
            }
          else
            {
            B[i] = -log( static_cast<double>(b[i]) / static_cast<double>(b0) );
            }
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
        
      oit.Set( tensor );
      ++oit; // Output (reconstructed tensor image) iterator
      ++it;  // Reference image iterator
      ++git; // Gradient  image iterator
      }
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
  for (unsigned int m = 0; m < m_NumberOfGradientDirections; m++)
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
  // Make sure crazy users did not call both AddGradientImage and 
  // SetGradientImage
  if( m_GradientImageTypeEnumeration == GradientIsInASingleImage)
    {
    itkExceptionMacro( << "Cannot call both methods:" 
    << "AddGradientImage and SetGradientImage. Please call only one of them.");
    }

  // If the container to hold the gradient directions hasn't been allocated
  // yet, allocate it.
  if( !this->m_GradientDirectionContainer )
    {
    this->m_GradientDirectionContainer = GradientDirectionContainerType::New();
    }
    
  m_GradientDirectionContainer->InsertElement( 
              m_NumberOfGradientDirections, gradientDirection );
  ++m_NumberOfGradientDirections;
  this->ProcessObject::SetNthInput( m_NumberOfGradientDirections, 
      const_cast< GradientImageType* >(gradientImage) );
  m_GradientImageTypeEnumeration = GradientIsInManyImages;
}

template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType >
void DiffusionTensor3DReconstructionImageFilter< TReferenceImagePixelType,
  TGradientImagePixelType, TTensorPixelType >
::SetGradientImage( GradientDirectionContainerType *gradientDirection, 
                        const GradientImagesType *gradientImage )
{
  // Make sure crazy users did not call both AddGradientImage and 
  // SetGradientImage
  if( m_GradientImageTypeEnumeration == GradientIsInManyImages )
    {
    itkExceptionMacro( << "Cannot call both methods:" 
    << "AddGradientImage and SetGradientImage. Please call only one of them.");
    }

  this->m_GradientDirectionContainer = gradientDirection;
  this->m_NumberOfGradientDirections = gradientDirection->Size();

  // ensure that the gradient image we received has as many components as 
  // the number of gradient directions
  if( gradientImage->GetVectorLength() != this->m_NumberOfGradientDirections )
    {
    itkExceptionMacro( << this->m_NumberOfGradientDirections << " gradient " <<
      "directions specified but image has " << gradientImage->GetVectorLength()
      << " components.");
    }
  
  this->ProcessObject::SetNthInput( 1, 
      const_cast< GradientImagesType* >(gradientImage) );
  m_GradientImageTypeEnumeration = GradientIsInASingleImage;
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
  if ( m_GradientDirectionContainer )
    {
    os << indent << "GradientDirectionContainer: "
       << m_GradientDirectionContainer << std::endl;
    }
  else
    {
    os << indent << 
    "GradientDirectionContainer: (Gradient directions not set)" << std::endl;
    }
  os << indent << "NumberOfGradientDirections: " << 
              m_NumberOfGradientDirections << std::endl;
  os << indent << "Threshold for reference B0 image: " << m_Threshold << std::endl;
  if ( this->m_GradientImageTypeEnumeration == GradientIsInManyImages )
    {
    os << indent << "Gradient images haven been supplied " << std::endl;
    }
  else if ( this->m_GradientImageTypeEnumeration == GradientIsInManyImages )
    {
    os << indent << "A multicomponent gradient image has been supplied" << std::endl;
    }
}

}

#endif
