/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImagePCADecompositionCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkImagePCADecompositionCalculator_txx
#define _itkImagePCADecompositionCalculator_txx
#ifdef _MSC_VER
#pragma warning( disable : 4288 )
#endif

#include "itkImagePCADecompositionCalculator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{ 
    
/*
 * Constructor
  */
template<class TInputImage, class TBasisImage>
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::ImagePCADecompositionCalculator()
{
  m_Image = NULL;
  m_MeanImage = NULL;
  m_BasisMatrixCalculated = false;
}

template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::SetBasisImages(const BasisImagePointerVector &v)
{ 
  itkDebugMacro(<< "setting BasisImages");
  this->m_BasisMatrixCalculated = false;
  // We need this modified setter function so that the calculator
  // can cache the basis set between calculations. Note that computing the
  // basis matrix from the input images is rather expensive, and the basis
  // images are likely to be changed less often than the input images. So
  // it makes sense to try to cache the pre-computed matrix.
  
  this->m_BasisImages = v; 
  this->Modified();
} 

/*
 * Compute the projection
  */
template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::Compute(void)
{
  if (!m_BasisMatrixCalculated) 
    {
    this->CalculateBasisMatrix();
    }
  this->CalculateRecenteredImageAsVector();
  m_Projection = m_BasisMatrix * m_ImageAsVector;
}



/*
 * Convert a vector of basis images into a matrix. Each image is flattened into 1-D.
  */
template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::CalculateBasisMatrix(void) {
  m_Size = m_BasisImages[0]->GetRequestedRegion().GetSize();
  m_NumPixels = 1;
  for( unsigned int i = 0; i < BasisImageDimension; i++ )
    {
    m_NumPixels *= m_Size[i]; 
    }
  
  m_BasisMatrix = BasisMatrixType(m_BasisImages.size(), m_NumPixels);
  
  int i = 0;
  for(typename BasisImagePointerVector::const_iterator basis_it = m_BasisImages.begin();
      basis_it != m_BasisImages.end(); ++basis_it) 
    {
      if( (*basis_it)->GetRequestedRegion().GetSize() != m_Size)
        {
        itkExceptionMacro("All basis images must be the same size!");
        }
    
      ImageRegionConstIterator<BasisImageType> image_it(*basis_it,
                                                        (*basis_it)->GetRequestedRegion());
      int j = 0;
      for (image_it.GoToBegin(); !image_it.IsAtEnd(); ++image_it)
        {
        m_BasisMatrix(i, j++) = image_it.Get();
        }
      i++;
    }
m_BasisMatrixCalculated = true;
m_ImageAsVector.set_size(m_NumPixels);
}

/*
 * Convert an image into a 1-D vector, changing the pixel type if necessary.
  */
template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::CalculateRecenteredImageAsVector(void) {
  if ( m_Image->GetRequestedRegion().GetSize() != m_Size) 
    {
    itkExceptionMacro("Input image must be the same size as the basis images!");
    }
  
  ImageRegionConstIterator<InputImageType> image_it(m_Image,
                                                  m_Image->GetRequestedRegion());
  typename BasisVectorType::iterator vector_it;
  for (image_it.GoToBegin(), vector_it = m_ImageAsVector.begin();
       !image_it.IsAtEnd(); ++image_it, ++vector_it)
    {
      *vector_it = static_cast<BasisPixelType> (image_it.Get());
    }
  
  if (m_MeanImage) 
    {
    ImageRegionConstIterator<BasisImageType> mimage_it(m_MeanImage,
                                              m_MeanImage->GetRequestedRegion());
    for (mimage_it.GoToBegin(), vector_it = m_ImageAsVector.begin();
         !mimage_it.IsAtEnd(); ++mimage_it, ++vector_it)
      {
      *vector_it -= (mimage_it.Get());
      } 
    }
}


template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::SetBasisFromModel(ModelPointerType model) {
  BasisImagePointerVector images;
  unsigned int nImages = model->GetNumberOfPrincipalComponentsRequired();
  images.reserve(nImages);
  for(int i = 1; i <= nImages; i++)
    {
    images.push_back(model->GetOutput(i));
    }
  this->SetBasisImages(images);
  this->SetMeanImage(model->GetOutput(0));
}

template<class TInputImage, class TBasisImage>
void
ImagePCADecompositionCalculator<TInputImage, TBasisImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Projection: " << m_Projection << std::endl;
  os << indent << "Image: " << m_Image.GetPointer() << std::endl;
  os << indent << "Mean Image: " << m_MeanImage.GetPointer() << std::endl;
}

} // end namespace itk

#endif
