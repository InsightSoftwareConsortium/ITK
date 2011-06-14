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
#ifndef __itkDanielssonDistanceMapImageFilter_txx
#define __itkDanielssonDistanceMapImageFilter_txx

#include <iostream>

#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkReflectiveImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 *    Constructor
 */
template< class TInputImage, class TOutputImage >
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::DanielssonDistanceMapImageFilter()
{
  this->SetNumberOfRequiredOutputs(3);

  OutputImagePointer distanceMap = OutputImageType::New();
  this->SetNthOutput( 0, distanceMap.GetPointer() );

  OutputImagePointer voronoiMap = OutputImageType::New();
  this->SetNthOutput( 1, voronoiMap.GetPointer() );

  VectorImagePointer distanceVectors = VectorImageType::New();
  this->SetNthOutput( 2, distanceVectors.GetPointer() );

  m_SquaredDistance     = false;
  m_InputIsBinary       = false;
  m_UseImageSpacing     = false;
}

/**
 *  Return the distance map Image pointer
 */
template< class TInputImage, class TOutputImage >
typename DanielssonDistanceMapImageFilter<
  TInputImage, TOutputImage >::OutputImageType *
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::GetDistanceMap(void)
{
  return dynamic_cast< OutputImageType * >(
           this->ProcessObject::GetOutput(0) );
}

/**
 *  Return Closest Points Map
 */
template< class TInputImage, class TOutputImage >
typename
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >::OutputImageType *
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::GetVoronoiMap(void)
{
  return dynamic_cast< OutputImageType * >(
           this->ProcessObject::GetOutput(1) );
}

/**
 *  Return the distance vectors
 */
template< class TInputImage, class TOutputImage >
typename DanielssonDistanceMapImageFilter<
  TInputImage, TOutputImage >::VectorImageType *
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::GetVectorDistanceMap(void)
{
  return dynamic_cast< VectorImageType * >(
           this->ProcessObject::GetOutput(2) );
}

/**
 *  Prepare data for computation
 */
template< class TInputImage, class TOutputImage >
void
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::PrepareData(void)
{
  itkDebugMacro(<< "PrepareData Start");
  OutputImagePointer voronoiMap = this->GetVoronoiMap();

  InputImagePointer inputImage  =
    dynamic_cast< const InputImageType * >( ProcessObject::GetInput(0) );

  voronoiMap->SetLargestPossibleRegion(
    inputImage->GetLargestPossibleRegion() );

  voronoiMap->SetBufferedRegion(
    inputImage->GetBufferedRegion() );

  voronoiMap->SetRequestedRegion(
    inputImage->GetRequestedRegion() );

  voronoiMap->Allocate();

  OutputImagePointer distanceMap = this->GetDistanceMap();

  distanceMap->SetLargestPossibleRegion(
    inputImage->GetLargestPossibleRegion() );

  distanceMap->SetBufferedRegion(
    inputImage->GetBufferedRegion() );

  distanceMap->SetRequestedRegion(
    inputImage->GetRequestedRegion() );

  distanceMap->Allocate();

  typename OutputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  // find the largest of the image dimensions
  SizeType size = region.GetSize();
  SizeValueType maxLength = 0;

  for ( unsigned int dim = 0; dim < InputImageDimension; dim++ )
    {
    if ( maxLength < size[dim] )
      {
      maxLength = size[dim];
      }
    }

  ImageRegionConstIteratorWithIndex< InputImageType > it(inputImage,  region);
  ImageRegionIteratorWithIndex< OutputImageType >     ot(voronoiMap,  region);

  it.GoToBegin();
  ot.GoToBegin();

  itkDebugMacro(<< "PrepareData: Copy input to output");
  if ( m_InputIsBinary )
    {
    IdentifierType npt = 1;
    while ( !ot.IsAtEnd() )
      {
      if ( it.Get() )
        {
        ot.Set(npt++);
        }
      else
        {
        ot.Set(0);
        }
      ++it;
      ++ot;
      }
    }
  else
    {
    while ( !ot.IsAtEnd() )
      {
      ot.Set( static_cast< OutputPixelType >( it.Get() ) );
      ++it;
      ++ot;
      }
    }

  VectorImagePointer distanceComponents = GetVectorDistanceMap();

  distanceComponents->SetLargestPossibleRegion(
    inputImage->GetLargestPossibleRegion() );

  distanceComponents->SetBufferedRegion(
    inputImage->GetBufferedRegion() );

  distanceComponents->SetRequestedRegion(
    inputImage->GetRequestedRegion() );

  distanceComponents->Allocate();

  ImageRegionIteratorWithIndex< VectorImageType > ct(distanceComponents,  region);

  OffsetType maxValue;
  OffsetType minValue;

  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    maxValue[j] =  2 * maxLength;
    minValue[j] =              0;
    }

  itkDebugMacro(<< "PrepareData: Copy output to ct");

  ot.GoToBegin();
  ct.GoToBegin();
  while ( !ot.IsAtEnd() )
    {
    if ( ot.Get() )
      {
      ct.Set(minValue);
      }
    else
      {
      ct.Set(maxValue);
      }
    ++ot;
    ++ct;
    }
  itkDebugMacro(<< "PrepareData End");
}

/**
 *  Post processing for computing the Voronoi Map
 */
template< class TInputImage, class TOutputImage >
void
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::ComputeVoronoiMap()
{
  itkDebugMacro(<< "ComputeVoronoiMap Start");
  OutputImagePointer voronoiMap          =  this->GetVoronoiMap();
  OutputImagePointer distanceMap         =  this->GetDistanceMap();
  VectorImagePointer distanceComponents  =  this->GetVectorDistanceMap();

  typename OutputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  ImageRegionIteratorWithIndex< OutputImageType > ot(voronoiMap,          region);
  ImageRegionIteratorWithIndex< VectorImageType > ct(distanceComponents,  region);
  ImageRegionIteratorWithIndex< OutputImageType > dt(distanceMap,         region);

  typename InputImageType::SpacingType spacing = Self::GetInput()->GetSpacing();

  itkDebugMacro(<< "ComputeVoronoiMap Region: " << region);
  ot.GoToBegin();
  ct.GoToBegin();
  dt.GoToBegin();
  while ( !ot.IsAtEnd() )
    {
    IndexType index = ct.GetIndex() + ct.Get();
    if ( region.IsInside(index) )
      {
      ot.Set( voronoiMap->GetPixel(index) );
      }

    OffsetType distanceVector = ct.Get();
    double     distance = 0.0;
    if ( m_UseImageSpacing )
      {
      for ( unsigned int i = 0; i < InputImageDimension; i++ )
        {
        double component = distanceVector[i] * static_cast< double >( spacing[i] );
        distance += component * component;
        }
      }
    else
      {
      for ( unsigned int i = 0; i < InputImageDimension; i++ )
        {
        distance += distanceVector[i] * distanceVector[i];
        }
      }

    if ( m_SquaredDistance )
      {
      dt.Set( static_cast< OutputPixelType >( distance ) );
      }
    else
      {
      dt.Set( static_cast< OutputPixelType >( vcl_sqrt(distance) ) );
      }
    ++ot;
    ++ct;
    ++dt;
    }
  itkDebugMacro(<< "ComputeVoronoiMap End");
}

/**
 *  Locally update the distance.
 */
template< class TInputImage, class TOutputImage >
void
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::UpdateLocalDistance(VectorImageType *components,
                      const IndexType & here,
                      const OffsetType & offset)
{
  IndexType  there            = here + offset;
  OffsetType offsetValueHere  = components->GetPixel(here);
  OffsetType offsetValueThere = components->GetPixel(there) + offset;

  SpacingType spacing = this->GetInput()->GetSpacing();

  double norm1 = 0.0;
  double norm2 = 0.0;
  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    double v1 = static_cast< double >(  offsetValueHere[i]  );
    double v2 = static_cast< double >(  offsetValueThere[i] );

    if ( m_UseImageSpacing )
      {
      double spacingComponent = static_cast< double >( spacing[i] );
      v1 *= spacingComponent;
      v2 *= spacingComponent;
      }

    norm1 += v1 * v1;
    norm2 += v2 * v2;
    }

  if ( norm1 > norm2 )
    {
    components->SetPixel( here, offsetValueThere );
    }
}

/**
 *  Compute Distance and Voronoi maps
 */
template< class TInputImage, class TOutputImage >
void
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->PrepareData();

  // Specify images and regions.

  OutputImagePointer voronoiMap             =  this->GetVoronoiMap();
  VectorImagePointer distanceComponents     =  this->GetVectorDistanceMap();

  RegionType region  = voronoiMap->GetRequestedRegion();

  itkDebugMacro (<< "Region to process: " << region);

  // Instantiate reflective iterator

  ReflectiveImageRegionConstIterator< VectorImageType >
  it(distanceComponents, region);

  typename VectorImageType::OffsetType voffset;
  for ( unsigned int dim = 0; dim < InputImageDimension; dim++ )
    {
    if ( region.GetSize()[dim] > 1 )
      {
      voffset[dim] = 1;
      }
    else
      {
      voffset[dim] = 0;
      }
    }
  it.SetBeginOffset(voffset);
  it.SetEndOffset(voffset);

  it.GoToBegin();

  // Support progress methods/callbacks.

  // Each pixel is visited 2^InputImageDimension times, and the number
  // of visits per pixel needs to be computed for progress reporting.
  SizeValueType visitsPerPixel = ( 1 << InputImageDimension );
  SizeValueType updateVisits = region.GetNumberOfPixels() * visitsPerPixel / 10;
  if ( updateVisits < 1 )
    {
    updateVisits = 1;
    }
  const float updatePeriod = static_cast< float >( updateVisits ) * 10.0;

  // Process image.

  OffsetType offset;
  offset.Fill(0);

  SizeValueType i = 0;

  itkDebugMacro(<< "GenerateData: Computing distance transform");
  while ( !it.IsAtEnd() )
    {
    if ( !( i % updateVisits ) )
      {
      this->UpdateProgress( static_cast< float >( i ) / updatePeriod );
      }

    IndexType here = it.GetIndex();
    for ( unsigned int dim = 0; dim < InputImageDimension; dim++ )
      {
      if ( region.GetSize()[dim] <= 1 )
        {
        continue;
        }
      if ( it.IsReflected(dim) )
        {
        offset[dim]++;
        UpdateLocalDistance(distanceComponents, here, offset);
        offset[dim] = 0;
        }
      else
        {
        offset[dim]--;
        UpdateLocalDistance(distanceComponents, here, offset);
        offset[dim] = 0;
        }
      }
    ++it;
    ++i;
    }

  itkDebugMacro(<< "GenerateData: ComputeVoronoiMap");

  this->ComputeVoronoiMap();
} // end GenerateData()

/**
 *  Print Self
 */
template< class TInputImage, class TOutputImage >
void
DanielssonDistanceMapImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Danielson Distance: " << std::endl;
  os << indent << "Input Is Binary   : " << m_InputIsBinary << std::endl;
  os << indent << "Use Image Spacing : " << m_UseImageSpacing << std::endl;
  os << indent << "Squared Distance  : " << m_SquaredDistance << std::endl;
}
} // end namespace itk

#endif
