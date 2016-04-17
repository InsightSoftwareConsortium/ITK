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
#ifndef itkFastMarchingUpwindGradientImageFilter_hxx
#define itkFastMarchingUpwindGradientImageFilter_hxx

#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <algorithm>

namespace itk
{
/**
 *
 */
template< typename TLevelSet, typename TSpeedImage >
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::FastMarchingUpwindGradientImageFilter()
{
  m_TargetPoints = ITK_NULLPTR;
  m_ReachedTargetPoints = ITK_NULLPTR;
  m_GradientImage = GradientImageType::New();
  m_GenerateGradientImage = false;
  m_TargetOffset = 1.0;
  m_TargetReachedMode = NoTargets;
  m_TargetValue = 0.0;
  m_NumberOfTargets = 0;
}

/**
 *
 */
template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Target points: " << m_TargetPoints.GetPointer() << std::endl;
  os << indent << "Reached points: " << m_ReachedTargetPoints.GetPointer() << std::endl;
  os << indent << "Gradient image: " << m_GradientImage.GetPointer() << std::endl;
  os << indent << "Generate gradient image: " << m_GenerateGradientImage << std::endl;
  os << indent << "Number of targets: " << m_NumberOfTargets << std::endl;
  os << indent << "Target offset: " << m_TargetOffset << std::endl;
  os << indent << "Target reach mode: " << m_TargetReachedMode << std::endl;
  os << indent << "Target value: " << m_TargetValue << std::endl;
}

/**
 *
 */
template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::Initialize(LevelSetImageType *output)
{
  Superclass::Initialize(output);

  // allocate memory for the GradientImage if requested
  if ( m_GenerateGradientImage )
    {
    m_GradientImage->CopyInformation( this->GetInput() );
    m_GradientImage->SetBufferedRegion( output->GetBufferedRegion() );
    m_GradientImage->Allocate();
    }

  // set all gradient vectors to zero
  if ( m_GenerateGradientImage )
    {
    typedef ImageRegionIterator< GradientImageType > GradientIterator;

    GradientIterator gradientIt( m_GradientImage,
                                 m_GradientImage->GetBufferedRegion() );

    GradientPixelType zeroGradient;
    typedef typename GradientPixelType::ValueType GradientPixelValueType;
    zeroGradient.Fill(NumericTraits< GradientPixelValueType >::ZeroValue());
    for ( gradientIt.GoToBegin(); !gradientIt.IsAtEnd(); ++gradientIt )
      {
      gradientIt.Set(zeroGradient);
      }
    }

  // Need to reset the target value.
  m_TargetValue = 0.0;

  // Even if there are no targets, a new NodeContainer should be created
  // so that querying this structure does not crash.
  m_ReachedTargetPoints = NodeContainer::New();
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::GenerateData()
{
  // cache the original stopping value that was set by the user
  // because this subclass may change it once a target point is
  // reached in order to control the execution of the superclass.
  double stoppingValue = this->GetStoppingValue();

  // run the GenerateData() method of the superclass
  try
    {
    Superclass::GenerateData();
    }
  catch ( ProcessAborted & exc )
    {
    // process was aborted, clean up the state of the filter
    // (most of the cleanup will have already been done by the
    // superclass)

    // restore the original stopping value
    this->SetStoppingValue(stoppingValue);
    throw exc;
    }

  // restore the original stopping value
  this->SetStoppingValue(stoppingValue);
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::UpdateNeighbors(
  const IndexType & index,
  const SpeedImageType *speedImage,
  LevelSetImageType *output)
{
  Superclass::UpdateNeighbors(index, speedImage, output);

  if ( m_GenerateGradientImage )
    {
    this->ComputeGradient(index, output, this->GetLabelImage(), m_GradientImage);
    }

  AxisNodeType node;

  // Only check for reached targets if the mode is not NoTargets and
  // there is at least one TargetPoint.
  if ( m_TargetReachedMode != NoTargets &&  m_TargetPoints )
    {
    bool targetReached = false;

    if ( m_TargetReachedMode == OneTarget )
      {
      typename NodeContainer::ConstIterator pointsIter = m_TargetPoints->Begin();
      typename NodeContainer::ConstIterator pointsEnd = m_TargetPoints->End();
      for (; pointsIter != pointsEnd; ++pointsIter )
        {
        node = pointsIter.Value();
        if ( node.GetIndex() == index )
          {
          m_ReachedTargetPoints->InsertElement(m_ReachedTargetPoints->Size(), node);
          targetReached = true;
          break;
          }
        }
      }
    else if ( m_TargetReachedMode == SomeTargets )
      {
      typename NodeContainer::ConstIterator pointsIter = m_TargetPoints->Begin();
      typename NodeContainer::ConstIterator pointsEnd = m_TargetPoints->End();
      for (; pointsIter != pointsEnd; ++pointsIter )
        {
        node = pointsIter.Value();

        if ( node.GetIndex() == index )
          {
          m_ReachedTargetPoints->InsertElement(m_ReachedTargetPoints->Size(), node);
          break;
          }
        }

      if ( static_cast< SizeValueType >( m_ReachedTargetPoints->Size() ) == m_NumberOfTargets )
        {
        targetReached = true;
        }
      }
    else if ( m_TargetReachedMode == AllTargets )
      {
      typename NodeContainer::ConstIterator pointsIter = m_TargetPoints->Begin();
      typename NodeContainer::ConstIterator pointsEnd = m_TargetPoints->End();
      for (; pointsIter != pointsEnd; ++pointsIter )
        {
        node = pointsIter.Value();

        if ( node.GetIndex() == index )
          {
          m_ReachedTargetPoints->InsertElement(m_ReachedTargetPoints->Size(), node);
          break;
          }
        }

      if ( m_ReachedTargetPoints->Size() == m_TargetPoints->Size() )
        {
        targetReached = true;
        }
      }

    if ( targetReached )
      {
      m_TargetValue = static_cast< double >( output->GetPixel(index) );
      double newStoppingValue = m_TargetValue + m_TargetOffset;
      if ( newStoppingValue < this->GetStoppingValue() )
        {
        // This changes the stopping value that may have been set by
        // the user.  Therefore, the value set by the user needs to be
        // cached in GenerateUpdate() so that it will be correct for
        // future Update() commands.
        this->SetStoppingValue(newStoppingValue);
        }
      }
    }
  else
    {
    m_TargetValue = static_cast< double >( output->GetPixel(index) );
    }
}

/**
 *
 */
template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingUpwindGradientImageFilter< TLevelSet, TSpeedImage >
::ComputeGradient(const IndexType & index,
                  const LevelSetImageType *output,
                  const LabelImageType *itkNotUsed(labelImage),
                  GradientImageType *gradientImage)
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
    NumericTraits< LevelSetPixelType >::ZeroValue();

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

    if ( !( neighIndex[j] > lastIndex[j]
            || neighIndex[j] < startIndex[j] ) )
      {
      if ( this->GetLabelImage()->GetPixel(neighIndex) == Superclass::AlivePoint )
        {
        dx_backward = centerPixel - output->GetPixel(neighIndex);
        }
      }

    dx_forward = 0.0;
    neighIndex[j] = index[j] + xStride[j];

    if ( !( neighIndex[j] > lastIndex[j]
            || neighIndex[j] < startIndex[j] ) )
      {
      if ( this->GetLabelImage()->GetPixel(neighIndex) == Superclass::AlivePoint )
        {
        dx_forward = output->GetPixel(neighIndex) - centerPixel;
        }
      }

    // Compute upwind finite differences
    if ( std::max<LevelSetPixelType>(dx_backward, -dx_forward) < ZERO )
      {
      gradientPixel[j] = ZERO;
      }
    else if ( dx_backward > -dx_forward )
      {
      gradientPixel[j] = dx_backward;
      }
    else
      {
      gradientPixel[j] = dx_forward;
      }
    gradientPixel[j] /= spacing[j];
    }

  gradientImage->SetPixel(index, gradientPixel);
}
} // namespace itk

#endif
