/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGridImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGridImageSource_txx
#define __itkGridImageSource_txx

#include "itkGaussianKernelFunction.h"
#include "itkGridImageSource.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
template< class TOutputImage >
GridImageSource< TOutputImage >
::GridImageSource()
{
  this->m_Size.Fill(64);
  this->m_Spacing.Fill(1.0);
  this->m_Origin.Fill(0.0);
  this->m_Direction.SetIdentity();

  this->m_Sigma.Fill(0.5);
  this->m_GridSpacing.Fill(4.0);
  this->m_GridOffset.Fill(0.0);
  this->m_WhichDimensions.Fill(true);
  this->m_Scale = 255.0;

  this->m_KernelFunction  = dynamic_cast< KernelFunction * >(
    GaussianKernelFunction::New().GetPointer() );
}

template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::BeforeThreadedGenerateData()
{
  typename ImageType::Pointer output = this->GetOutput(0);

  this->m_PixelArrays = PixelArrayContainerType::New();
  m_PixelArrays->Initialize();

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_GridOffset[i] > this->m_GridSpacing[i] )
      {
      this->m_GridOffset[i] = this->m_GridSpacing[i];
      }
    PixelArrayType pixels = m_PixelArrays->CreateElementAt(i);
    pixels.set_size(this->m_Size[i]);
    pixels.fill(1);
    if ( this->m_WhichDimensions[i] )
      {
      ImageLinearIteratorWithIndex< ImageType > It( output, output->GetRequestedRegion() );
      It.SetDirection(i);

      /** Add two extra functions in the front and one in the back to ensure
        coverage */
      unsigned int numberOfGaussians = Math::Ceil< unsigned int >(
        this->m_Size[i] * this->m_Spacing[i] / this->m_GridSpacing[i]) + 4u;
      for ( It.GoToBegin(); !It.IsAtEndOfLine(); ++It )
        {
        typename ImageType::IndexType index = It.GetIndex();
        typename ImageType::PointType point;
        output->TransformIndexToPhysicalPoint(index, point);

        RealType val = 0;
        for ( unsigned int j = 0; j < numberOfGaussians; j++ )
          {
          RealType num = point[i] - static_cast< RealType >( j - 2 ) * this->m_GridSpacing[i]
                         - this->m_Origin[i] - this->m_GridOffset[i];
          val += this->m_KernelFunction->Evaluate(num / this->m_Sigma[i]);
          }
        pixels[index[i]] = val;
        }
      pixels = 1.0 - pixels / pixels.max_value();
      }
    this->m_PixelArrays->SetElement(i, pixels);
    }
}

template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::ThreadedGenerateData(const ImageRegionType & outputRegionForThread, int threadId)
{
  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  typename ImageType::Pointer output = this->GetOutput(0);

  ImageRegionIteratorWithIndex< ImageType > It(output, outputRegionForThread);

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    RealType val = 1.0;
    typename ImageType::IndexType index = It.GetIndex();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      val *= this->m_PixelArrays->GetElement(i)[index[i]];
      }
    It.Set(this->m_Scale * val);
    progress.CompletedPixel();
    }
}

template< class TOutputImage >
void
GridImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output image information: " << std::endl;
  os << indent << "   Size       = " << this->GetSize() << std::endl;
  os << indent << "   Spacing    = " << this->GetSpacing() << std::endl;
  os << indent << "   Origin     = " << this->GetOrigin() << std::endl;
  os << indent << "   Direction  = " << this->GetDirection() << std::endl;
  os << indent << "   Scale      = " << this->GetScale() << std::endl;

  os << indent << "Grid information: " << std::endl;
  os << indent << "   WhichDimensions = " << this->GetWhichDimensions() << std::endl;
  os << indent << "   Kernel          = " << this->GetKernelFunction() << std::endl;
  os << indent << "   Sigma           = " << this->GetSigma() << std::endl;
  os << indent << "   Grid spacing    = " << this->GetGridSpacing() << std::endl;
  os << indent << "   Grid offset     = " << this->GetGridOffset() << std::endl;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::GenerateOutputInformation()
{
  ImageType *output;

  output = this->GetOutput(0);

  typename ImageType::IndexType index;
  index.Fill(0);

  typename ImageType::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(this->m_Size);
  largestPossibleRegion.SetIndex(index);
  output->SetLargestPossibleRegion(largestPossibleRegion);

  output->SetSpacing(this->m_Spacing);
  output->SetOrigin(this->m_Origin);
  output->SetDirection(this->m_Direction);
}
} // end namespace itk

#endif
