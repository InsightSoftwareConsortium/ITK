/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingUpwindGradientImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkFastMarchingUpwindGradientImageFilter_txx
#define _itkFastMarchingUpwindGradientImageFilter_txx

#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include <algorithm>


namespace itk
{

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
FastMarchingUpwindGradientImageFilter<TLevelSet,TSpeedImage>
::FastMarchingUpwindGradientImageFilter()
{
  m_TargetPoints = NULL;
  m_ReachedTargetPoints = NULL;
  m_GradientImage = GradientImageType::New();
  m_GenerateGradientImage = false;
  m_TargetOffset = 1.0;
  m_TargetReachedMode = OneTarget;
  m_TargetValue = 0.0;
}


/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingUpwindGradientImageFilter<TLevelSet,TSpeedImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Target points: " << m_TargetPoints.GetPointer() << std::endl;
  os << indent << "Reached points: " << m_ReachedTargetPoints.GetPointer() << std::endl;
  os << indent << "Gradient image: " << m_GradientImage.GetPointer() << std::endl;
  os << indent << "Generate gradient image: " << m_GenerateGradientImage << std::endl;
  os << indent << "Target offset: " << m_TargetOffset << std::endl;
  os << indent << "Target reach mode: " << m_TargetReachedMode << std::endl;
  os << indent << "Target value: " << m_TargetValue << std::endl;
}

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingUpwindGradientImageFilter<TLevelSet,TSpeedImage>
::Initialize( LevelSetImageType * output )
{
  Superclass::Initialize(output);

  // allocate memory for the GradientImage if requested
  if (m_GenerateGradientImage)
    {
    m_GradientImage->CopyInformation( this->GetInput() );    
    m_GradientImage->SetBufferedRegion( output->GetBufferedRegion() );
    m_GradientImage->Allocate();
    }

  // set all gradient vectors to zero
  if (m_GenerateGradientImage)
    {
    typedef ImageRegionIterator< GradientImageType > GradientIterator;

    GradientIterator gradientIt( m_GradientImage, 
                                 m_GradientImage->GetBufferedRegion() );

    GradientPixelType zeroGradient;
    typedef typename GradientPixelType::ValueType GradientPixelValueType;
    zeroGradient.Fill(NumericTraits< GradientPixelValueType >::Zero);
    for ( gradientIt.GoToBegin(); !gradientIt.IsAtEnd(); ++gradientIt )
      {
      gradientIt.Set( zeroGradient );
      }
    }

  if ( m_TargetReachedMode == AllTargets )
    {
    m_ReachedTargetPoints = NodeContainer::New();
    }
}

template <class TLevelSet, class TSpeedImage>
void
FastMarchingUpwindGradientImageFilter<TLevelSet,TSpeedImage>
::UpdateNeighbors(
  const IndexType& index,
  const SpeedImageType * speedImage,
  LevelSetImageType * output )
{
  Superclass::UpdateNeighbors(index,speedImage,output);

  IndexType neighIndex = index;

  if (m_GenerateGradientImage)
    {
    this->ComputeGradient(index, output, this->GetLabelImage(), m_GradientImage);
    }

  AxisNodeType node;

  if ( m_TargetPoints )
    {
    bool targetReached = false;
    
    if (m_TargetReachedMode == OneTarget)
      {
      typename NodeContainer::ConstIterator pointsIter = m_TargetPoints->Begin();
      typename NodeContainer::ConstIterator pointsEnd = m_TargetPoints->End();
      for ( ; pointsIter != pointsEnd; ++pointsIter )
        {
        node = pointsIter.Value();
        if (node.GetIndex() == index)
          {
          targetReached = true;
          break;
          }
        }
      }
    else if (m_TargetReachedMode == AllTargets)
      {
      typename NodeContainer::ConstIterator pointsIter = m_TargetPoints->Begin();
      typename NodeContainer::ConstIterator pointsEnd = m_TargetPoints->End();
      for ( ; pointsIter != pointsEnd; ++pointsIter )
        {
        node = pointsIter.Value();

        if (node.GetIndex() == index)
          {
          m_ReachedTargetPoints->InsertElement(m_ReachedTargetPoints->Size()-1,node);
          break;
          }
        }

      if (m_ReachedTargetPoints->Size() == m_TargetPoints->Size())
        {
        targetReached = true;
        }
      }

    if (targetReached)
      {
      m_TargetValue = static_cast<double>(output->GetPixel(index));
      double newStoppingValue = m_TargetValue + m_TargetOffset;
      if (newStoppingValue < this->GetStoppingValue())
        {
        this->SetStoppingValue(newStoppingValue);
        }
      }
    }      
}

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingUpwindGradientImageFilter<TLevelSet,TSpeedImage>
::ComputeGradient( const IndexType& index,
  const LevelSetImageType * output,
  const LabelImageType * labelImage,
  GradientImageType * gradientImage)
{
  IndexType neighIndex = index;
  typedef typename TLevelSet::PixelType LevelSetPixelType;
  LevelSetPixelType centerPixel;
  LevelSetPixelType dx_forward;
  LevelSetPixelType dx_backward;
  GradientPixelType gradientPixel;
  
  const LevelSetIndexType & lastIndex = this->GetLastIndex();
  const LevelSetIndexType & startIndex = this->GetStartIndex();

  const LevelSetPixelType ZERO = 
            NumericTraits< LevelSetPixelType >::Zero;

  OutputSpacingType spacing = this->GetOutput()->GetSpacing();

  unsigned int xStride[itkGetStaticConstMacro(SetDimension)];

  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    centerPixel = output->GetPixel(index);

    neighIndex = index;

    // Set stride of one in each direction
    xStride[j] = 1;

    // Compute one-sided finite differences with alive neighbors 
    // (the front can only come from there)
    dx_backward = 0.0;
    neighIndex[j] = index[j] - xStride[j];

    if(! (neighIndex[j] > lastIndex[j] || 
          neighIndex[j] < startIndex[j]) )
      {
      if ( this->GetLabelImage()->GetPixel( neighIndex ) == Superclass::AlivePoint )
        {
        dx_backward = centerPixel - output->GetPixel( neighIndex );
        }
      }

    dx_forward = 0.0;
    neighIndex[j] = index[j] + xStride[j];

    if(! (neighIndex[j] > lastIndex[j] || 
          neighIndex[j] < startIndex[j]) )
      {
      if ( this->GetLabelImage()->GetPixel( neighIndex ) == Superclass::AlivePoint )
        {
        dx_forward = output->GetPixel( neighIndex ) - centerPixel;
        }
      }

    // Compute upwind finite differences
    if (vnl_math_max(dx_backward,-dx_forward) < ZERO)
      {
      gradientPixel[j] = ZERO;
      }
    else if (dx_backward > -dx_forward)
      {
      gradientPixel[j] = dx_backward;
      }
    else
      {
      gradientPixel[j] = dx_forward;
      }

    gradientPixel[j] /= spacing[j];
    }

  gradientImage->SetPixel( index, gradientPixel );
}

} // namespace itk


#endif
