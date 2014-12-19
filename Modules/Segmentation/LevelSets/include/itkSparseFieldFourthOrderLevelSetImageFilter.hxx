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
#ifndef itkSparseFieldFourthOrderLevelSetImageFilter_hxx
#define itkSparseFieldFourthOrderLevelSetImageFilter_hxx

#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
const SizeValueType
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::m_NumVertex = 1 << ImageDimension;

template< typename TInputImage, typename TOutputImage >
const typename SparseFieldFourthOrderLevelSetImageFilter< TInputImage,
                                                          TOutputImage >::ValueType
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::m_DimConst = static_cast< ValueType >( 2.0 / m_NumVertex );

template< typename TInputImage, typename TOutputImage >
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::SparseFieldFourthOrderLevelSetImageFilter()
{
  m_RefitIteration = 0;
  m_LevelSetFunction = ITK_NULLPTR;
  m_ConvergenceFlag = false;

  this->SetIsoSurfaceValue(0);
  m_MaxRefitIteration = 100;
  m_MaxNormalIteration = 25;
  m_RMSChangeNormalProcessTrigger = NumericTraits< ValueType >::ZeroValue();
  m_CurvatureBandWidth = static_cast< ValueType >( ImageDimension ) + 0.5;
  m_NormalProcessType = 0;
  m_NormalProcessConductance = NumericTraits< ValueType >::ZeroValue();
  m_NormalProcessUnsharpFlag = false;
  m_NormalProcessUnsharpWeight = NumericTraits< ValueType >::ZeroValue();
}

template< typename TInputImage, typename TOutputImage >
void
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaxRefitIteration: " << m_MaxRefitIteration << std::endl;
  os << indent << "MaxNormalIteration: " << m_MaxNormalIteration << std::endl;
  os << indent << "CurvatureBandWidth: " << m_CurvatureBandWidth << std::endl;

  os << indent << "RMSChangeNormalProcessTrigger: "
     << m_RMSChangeNormalProcessTrigger << std::endl;

  os << indent << "NormalProcessType: " << m_NormalProcessType << std::endl;

  os << indent << "NormalProcessConductance: "
     << m_NormalProcessConductance << std::endl;

  os << indent << "NormalProcessUnsharpFlag: "
     << m_NormalProcessUnsharpFlag << std::endl;

  os << indent << "NormalProcessUnsharpWeight: "
     << m_NormalProcessUnsharpWeight << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::SetLevelSetFunction(LevelSetFunctionType *lsf)
{
  m_LevelSetFunction = lsf;
  Superclass::SetDifferenceFunction(lsf);
}

template< typename TInputImage, typename TOutputImage >
typename SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::ValueType
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::ComputeCurvatureFromSparseImageNeighborhood(SparseImageIteratorType & it) const
{
  unsigned int        counter;
  SizeValueType       position,  stride[ImageDimension], indicator[ImageDimension];
  const SizeValueType one = 1;
  const SizeValueType center = it.Size() / 2;
  NormalVectorType    normalvector;
  ValueType           curvature;
  bool                flag = false;

  const NeighborhoodScalesType neighborhoodScales = this->GetDifferenceFunction()->ComputeNeighborhoodScales();

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    stride[j] = it.GetStride( j );
    indicator[j] = one << j;
    }

  curvature = NumericTraits< ValueType >::ZeroValue();

  for ( counter = 0; counter < m_NumVertex; counter++ )
    {
    position = center;
    for ( unsigned int k = 0; k < ImageDimension; k++ )
      {
      if ( counter & indicator[k] )
        {
        position -= stride[k];
        }
      }
    if ( it.GetPixel (position) == ITK_NULLPTR )
      {
      flag = true;
      }
    else
      {
      normalvector = it.GetPixel (position)->m_Data;
      for ( unsigned int j = 0; j < ImageDimension; j++ ) // derivative axis
        {
        if ( counter & indicator[j] )
          {
          curvature -= normalvector[j] * neighborhoodScales[j];
          }
        else
          {
          curvature += normalvector[j] * neighborhoodScales[j];
          }
        } // end derivative axis
      }
    } // end counter

  if ( flag == true ) { curvature = NumericTraits< ValueType >::ZeroValue(); }
  curvature *= m_DimConst;
  return curvature;
}

template< typename TInputImage, typename TOutputImage >
void
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::ComputeCurvatureTarget(const OutputImageType *distanceImage,
                         SparseImageType *sparseImage) const
{
  typedef ImageRegionConstIterator< OutputImageType > DistanceImageIteratorType;

  DistanceImageIteratorType distanceImageIterator (
    distanceImage,
    distanceImage->GetRequestedRegion() );
  typename SparseImageIteratorType::RadiusType radius;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    radius[j] = 1;
    }
  SparseImageIteratorType
  sparseImageIterator ( radius, sparseImage,
                        sparseImage->GetRequestedRegion() );

  ValueType distance;
  NodeType *node;

  sparseImageIterator.GoToBegin();
  distanceImageIterator.GoToBegin();
  while ( !distanceImageIterator.IsAtEnd() )
    {
    distance = distanceImageIterator.Value();
    node = sparseImageIterator.GetCenterPixel();
    if ( ( distance >= -m_CurvatureBandWidth  )
         && ( distance <= m_CurvatureBandWidth ) )
      {
      node->m_Curvature =
        ComputeCurvatureFromSparseImageNeighborhood (sparseImageIterator);
      node->m_CurvatureFlag = true;
      }
    else
      {
      if ( node != ITK_NULLPTR )
        {
        node->m_CurvatureFlag = false;
        }
      }
    ++sparseImageIterator;
    ++distanceImageIterator;
    }
}

template< typename TInputImage, typename TOutputImage >
bool
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::ActiveLayerCheckBand() const
{
  typename LayerType::Iterator layerIt;
  typename SparseImageType::Pointer
  im = m_LevelSetFunction->GetSparseTargetImage();
  bool      flag = false;
  NodeType *node;

  layerIt = this->m_Layers[0]->Begin();
  while ( layerIt != this->m_Layers[0]->End() )
    {
    node = im->GetPixel(layerIt->m_Value);
    if ( ( node == ITK_NULLPTR )
         || ( node->m_CurvatureFlag == false ) )
      {
      //level set touching edge of normal band
      flag = true;
      break;
      }
    ++layerIt;
    }
  return flag;
}

template< typename TInputImage, typename TOutputImage >
void
SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
::ProcessNormals()
{
  typename NormalVectorFilterType::Pointer NormalVectorFilter;
  typename NormalVectorFunctionType::Pointer NormalVectorFunction;

  ValueType temp = static_cast< ValueType >( ImageDimension );

  NormalVectorFilter    = NormalVectorFilterType::New();
  NormalVectorFunction = NormalVectorFunctionType::New();
  NormalVectorFunction->SetNormalProcessType (m_NormalProcessType);
  NormalVectorFunction->SetConductanceParameter (m_NormalProcessConductance);
  NormalVectorFilter->SetNormalFunction (NormalVectorFunction);
  NormalVectorFilter->SetIsoLevelLow  (-m_CurvatureBandWidth - temp);
  NormalVectorFilter->SetIsoLevelHigh (m_CurvatureBandWidth + temp);
  NormalVectorFilter->SetMaxIteration (m_MaxNormalIteration);
  NormalVectorFilter->SetUnsharpMaskingFlag (m_NormalProcessUnsharpFlag);
  NormalVectorFilter->SetUnsharpMaskingWeight (m_NormalProcessUnsharpWeight);

  // Move the pixel container and image information of the image we are working
  // on into a temporary image to  use as the input to the mini-pipeline.  This
  // avoids a complete copy of the image.
  typename OutputImageType::Pointer phi = this->GetOutput();
  typename OutputImageType::Pointer tmp = OutputImageType::New();
  tmp->SetRequestedRegion( phi->GetRequestedRegion() );
  tmp->SetBufferedRegion( phi->GetBufferedRegion() );
  tmp->SetLargestPossibleRegion( phi->GetLargestPossibleRegion() );
  tmp->SetPixelContainer( phi->GetPixelContainer() );
  tmp->CopyInformation(phi);

  NormalVectorFilter->SetInput(tmp);
  NormalVectorFilter->Update();

  typename SparseImageType::Pointer SparseNormalImage =
    NormalVectorFilter->GetOutput();

  this->ComputeCurvatureTarget(tmp, SparseNormalImage);
  m_LevelSetFunction->SetSparseTargetImage(SparseNormalImage);
}
} // end namespace itk

#endif
