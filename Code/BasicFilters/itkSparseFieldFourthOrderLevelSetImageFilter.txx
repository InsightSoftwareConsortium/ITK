/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFieldFourthOrderLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseFieldFourthOrderLevelSetImageFilter_txx_
#define __itkSparseFieldFourthOrderLevelSetImageFilter_txx_

#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkSparseImage.h"
#include "itkNumericTraits.h"

namespace itk {

template <class TInputImage, class TOutputImage> 
const unsigned long
SparseFieldFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
::m_NumVertex = 1 << ImageDimension;

template <class TInputImage, class TOutputImage> 
const typename SparseFieldFourthOrderLevelSetImageFilter <TInputImage,
                                                     TOutputImage>::ValueType
SparseFieldFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
::m_DimConst = static_cast <ValueType> (2.0/m_NumVertex);

template<class TInputImage, class TOutputImage>
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::SparseFieldFourthOrderLevelSetImageFilter()
{
  m_RefitIteration = 0;
  m_LevelSetFunction = 0;
  m_ConvergenceFlag = false;

  this->SetIsoSurfaceValue(0);
  this->SetMaxRefitIteration(100);
  this->SetMaxNormalIteration(25);
  this->SetRMSChangeNormalProcessTrigger (0);
  this->SetCurvatureBandWidth (static_cast<ValueType>(ImageDimension) + 0.5);
  this->SetNormalProcessType (0);
  this->SetNormalProcessConductance (0);
  this->SetNormalProcessUnsharpFlag (false);
  this->SetNormalProcessUnsharpWeight (0);
}

template<class TInputImage, class TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaxRefitIteration: " << m_MaxRefitIteration << std::endl;
  os << indent << "MaxNormalIteration: " << m_MaxNormalIteration << std::endl;
  os << indent << "CurvatureBandWidth: " << m_CurvatureBandWidth << std::endl;

  os << indent << "RMSChangeNormalProcessTrigger: "
     << m_RMSChangeNormalProcessTrigger<<std::endl;

  os << indent << "NormalProcessType: " << m_NormalProcessType << std::endl;

  os << indent <<"NormalProcessConductance: "
     << m_NormalProcessConductance<<std::endl;

  os << indent << "NormalProcessUnsharpFlag: "
     << m_NormalProcessUnsharpFlag << std::endl;

  os << indent <<"NormalProcessUnsharpWeight: "
     << m_NormalProcessUnsharpWeight<<std::endl;
}

template<class TInputImage, class TOutputImage>
void SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::SetLevelSetFunction (LevelSetFunctionType *lsf)
{
  m_LevelSetFunction = lsf;
  Superclass::SetDifferenceFunction(lsf);
}

template<class TInputImage, class TOutputImage>
typename SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::ValueType
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::ComputeCurvatureFromSparseImageNeighborhood (SparseImageIteratorType &it) const
{
  unsigned int j, k;
  unsigned int counter;
  unsigned long position,  stride[ImageDimension], indicator[ImageDimension];
  const unsigned long center = it.Size() / 2;
  NormalVectorType normalvector;
  ValueType curvature;
  bool flag = false;
  
  for( j = 0; j < ImageDimension; j++ )
    {
        stride[j] = it.GetStride( (unsigned long) j);
        indicator[j] = 1 << j;
    }
 
  curvature = NumericTraits<ValueType>::Zero;
  
  for (counter = 0; counter < m_NumVertex; counter++)
    {
    position = center;
    for (k = 0; k < ImageDimension; k++)
      {
      if (counter & indicator[k])
        {
        position -= stride[k];
        }
      }
    if (it.GetPixel (position)==0)
      {
      flag = true;
      }
    else
      {
      normalvector = it.GetPixel (position)->m_Data;
      for (j = 0; j < ImageDimension; j++) // derivative axis
        {
        if ( counter & indicator[j] )
          {
          curvature -= normalvector[j];
          }
        else
          {
          curvature += normalvector[j];
          }
        } // end derivative axis
      }
    } // end counter

  if (flag == true) curvature = NumericTraits<ValueType>::Zero;
  curvature *= m_DimConst;
  return curvature;
}

template<class TInputImage, class TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::ComputeCurvatureTarget (const OutputImageType *distanceImage,
                          SparseImageType *sparseImage) const
{
  typedef ImageRegionConstIterator <OutputImageType> DistanceImageIteratorType;
  
  DistanceImageIteratorType
    distanceImageIterator (distanceImage,
                           distanceImage->GetRequestedRegion());
  unsigned int j;
  typename SparseImageIteratorType::RadiusType radius;
  for( j = 0; j < ImageDimension; j++ )
    {
    radius[j] = 1;
    }
  SparseImageIteratorType
    sparseImageIterator (radius,sparseImage,
                         sparseImage->GetRequestedRegion());
  
  ValueType distance;
  NodeType* node;
  
  sparseImageIterator.GoToBegin();
  distanceImageIterator.GoToBegin();
  while ( !distanceImageIterator.IsAtEnd() )
    {
    distance = distanceImageIterator.Value();
    node = sparseImageIterator.GetCenterPixel();
    if ( (distance >= -m_CurvatureBandWidth  )&&
         (distance <= m_CurvatureBandWidth ) )
      {
      node->m_Curvature =
        ComputeCurvatureFromSparseImageNeighborhood (sparseImageIterator);
      node->m_CurvatureFlag = true;
      }
    else
      {
      if (node != 0)
        {
        node->m_CurvatureFlag = false;
        }
      }
    ++sparseImageIterator;
    ++distanceImageIterator;
    }
}

template<class TInputImage, class TOutputImage>
bool
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::ActiveLayerCheckBand () const
{
  typename LayerType::Iterator layerIt;
  typename SparseImageType::Pointer
    im = m_LevelSetFunction->GetSparseTargetImage();
  bool flag = false;
  NodeType *node;
  
  layerIt = m_Layers[0]->Begin();
  while (layerIt != m_Layers[0]->End() )
    {
    node = im->GetPixel(layerIt->m_Value);
    if ((node == 0)||
        (node->m_CurvatureFlag == false))
      {
      //level set touching edge of normal band
      flag = true;
      break;
      }
    ++layerIt;
    }
  return flag;
}

template<class TInputImage, class TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>
::ProcessNormals()
{
  typename NormalVectorFilterType::Pointer   NormalVectorFilter;
  typename NormalVectorFunctionType::Pointer NormalVectorFunction;

  ValueType temp = static_cast<ValueType>(ImageDimension);
  
  NormalVectorFilter    = NormalVectorFilterType::New();
  NormalVectorFunction = NormalVectorFunctionType::New();
  NormalVectorFunction->SetNormalProcessType ( m_NormalProcessType );
  NormalVectorFunction->SetConductanceParameter ( m_NormalProcessConductance );
  NormalVectorFilter->SetNormalFunction ( NormalVectorFunction );
  NormalVectorFilter->SetIsoLevelLow  (-m_CurvatureBandWidth-temp );
  NormalVectorFilter->SetIsoLevelHigh ( m_CurvatureBandWidth+temp );
  NormalVectorFilter->SetMaxIteration ( m_MaxNormalIteration );
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
  tmp->CopyInformation( phi );
  
  NormalVectorFilter->SetInput(tmp);
  NormalVectorFilter->Update();

  typename SparseImageType::Pointer SparseNormalImage
    = NormalVectorFilter->GetOutput();

  this->ComputeCurvatureTarget(tmp, SparseNormalImage);
  m_LevelSetFunction->SetSparseTargetImage(SparseNormalImage);
}

} // end namespace itk

#endif
