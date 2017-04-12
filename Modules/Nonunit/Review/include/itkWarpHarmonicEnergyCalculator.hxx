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
#ifndef itkWarpHarmonicEnergyCalculator_hxx
#define itkWarpHarmonicEnergyCalculator_hxx

#include "itkWarpHarmonicEnergyCalculator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

#include "vnl/vnl_matrix.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage >
WarpHarmonicEnergyCalculator< TInputImage >
::WarpHarmonicEnergyCalculator() :
  m_HarmonicEnergy( 0.0 ),
  m_RegionSetByUser( false ),
  m_UseImageSpacing( true )
{
  m_Image = TInputImage::New();
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_NeighborhoodRadius[i] = 1; // radius of neighborhood we will use
    m_DerivativeWeights[i] = 1.0;
    }
}

template< typename TInputImage >
void
WarpHarmonicEnergyCalculator< TInputImage >
::SetUseImageSpacing(bool f)
{
  if ( m_UseImageSpacing == f )
    {
    return;
    }

  // Only reset the weights if they were previously set to the image spacing,
  // otherwise, the user may have provided their own weightings.
  if ( f == false && m_UseImageSpacing == true )
    {
    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      m_DerivativeWeights[i] = 1.0;
      }
    }

  m_UseImageSpacing = f;
}

template< typename TInputImage >
void
WarpHarmonicEnergyCalculator< TInputImage >
::Compute(void)
{
  if ( !m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }

  // Set the weights on the derivatives.
  // Are we using image spacing in the calculations?  If so we must update now
  // in case our input image has changed.
  if ( m_UseImageSpacing == true )
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( m_Image->GetSpacing()[i] <= 0.0 )
        {
        itkExceptionMacro(<< "Image spacing in dimension " << i << " is zero.");
        }
      m_DerivativeWeights[i] = 1.0 / static_cast< double >( m_Image->GetSpacing()[i] );
      }
    }

  m_HarmonicEnergy = 0.0;

  ZeroFluxNeumannBoundaryCondition< ImageType > nBc;
  ConstNeighborhoodIteratorType                 bIt;

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< ImageType >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< ImageType > bC;
  faceList = bC(m_Image, m_Region, m_NeighborhoodRadius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< ImageType >::
  FaceListType::iterator fIt;
  fIt = faceList.begin();

  // Process each of the data set faces.  The iterator is reinitialized on each
  // face so that it can determine whether or not to check for boundary
  // conditions.
  for ( fIt = faceList.begin(); fIt != faceList.end(); ++fIt )
    {
    bIt = ConstNeighborhoodIteratorType(m_NeighborhoodRadius,
                                        m_Image,
                                        *fIt);
    bIt.OverrideBoundaryCondition(&nBc);
    bIt.GoToBegin();

    while ( !bIt.IsAtEnd() )
      {
      m_HarmonicEnergy += this->EvaluateAtNeighborhood(bIt);
      ++bIt;
      }
    }

  m_HarmonicEnergy /= m_Region.GetNumberOfPixels();
}

template< typename TInputImage >
double
WarpHarmonicEnergyCalculator< TInputImage >
::EvaluateAtNeighborhood(ConstNeighborhoodIteratorType & it) const
{
  // Simple method using field derivatives

  vnl_matrix_fixed< double, ImageDimension, VectorDimension > J;

  PixelType next, prev;

  double weight;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    next = it.GetNext(i);
    prev = it.GetPrevious(i);

    weight = 0.5 * m_DerivativeWeights[i];

    for ( unsigned int j = 0; j < VectorDimension; ++j )
      {
      J[i][j] = weight * ( static_cast< double >( next[j] ) - static_cast< double >( prev[j] ) );
      }

    // Add one on the diagonal to consider the warping and not only the
    // deformation field
    //J[i][i] += 1.0;
    }

  const double norm = J.fro_norm();
  return norm * norm;
}

template< typename TInputImage >
void
WarpHarmonicEnergyCalculator< TInputImage >
::SetRegion(const RegionType & region)
{
  m_Region = region;
  m_RegionSetByUser = true;
}

template< typename TInputImage >
void
WarpHarmonicEnergyCalculator< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "HarmonicEnergy: " << m_HarmonicEnergy << std::endl;
  itkPrintSelfObjectMacro( Image );
  os << indent << "Region: " << std::endl;
  m_Region.Print( os, indent.GetNextIndent() );
  os << indent << "Region set by User: " << m_RegionSetByUser << std::endl;
  os << indent << "Use image spacing: " << this->m_UseImageSpacing << std::endl;
  os << indent << "Derivative Weights: " << this->m_DerivativeWeights << std::endl;
  os << indent << "Neighborhood Radius: " << this->m_NeighborhoodRadius << std::endl;
}
} // end namespace itk

#endif
