/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: 
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkReflectiveImageRegionIterator.h"
#include "itkRegionNeighborhoodIterator.h"


namespace itk
{




/**
 *    Constructor
 */
template <class TInputImage,class TOutputImage>
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::DanielssonDistanceMapImageFilter()
{

  this->SetNumberOfRequiredOutputs( 3 );

  OutputImagePointer distanceMap = OutputImageType::New();
  this->SetNthOutput( 0, distanceMap.GetPointer() );

  OutputImagePointer voronoiMap = OutputImageType::New();
  this->SetNthOutput( 1, voronoiMap.GetPointer() );

  VectorImagePointer distanceVectors = VectorImageType::New();
  this->SetNthOutput( 2, distanceVectors.GetPointer() );

  m_SquaredDistance     = false;
  m_InputIsBinary       = false;

}



/**
 *  Return the distance map Image pointer
 */
template <class TInputImage,class TOutputImage>
DanielssonDistanceMapImageFilter<
              TInputImage,TOutputImage>::OutputImagePointer 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetDistanceMap(void)
{
  return  dynamic_cast< OutputImageType * >(
                  this->ProcessObject::GetOutput(0).GetPointer() );
}





/**
 *  Return Closest Points Map
 */
template <class TInputImage,class TOutputImage>
DanielssonDistanceMapImageFilter<
         TInputImage,TOutputImage>::OutputImagePointer 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetVoronoiMap(void)
{
  return  dynamic_cast< OutputImageType * >(
                  this->ProcessObject::GetOutput(1).GetPointer() );
}






/**
 *  Return the distance vectors
 */
template <class TInputImage,class TOutputImage>
DanielssonDistanceMapImageFilter<
           TInputImage,TOutputImage>::VectorImagePointer 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetVectorDistanceMap(void)
{
  return  dynamic_cast< VectorImageType * >(
                  this->ProcessObject::GetOutput(2).GetPointer() );
}






/**
 *  Prepare data for computation
 */
template <class TInputImage,class TOutputImage>
void 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::PrepareData(void) 
{
  
  OutputImagePointer voronoiMap = GetVoronoiMap();

  InputImagePointer  inputImage  = dynamic_cast<TInputImage  *>(
                                (ProcessObject::GetInput(  0 )).GetPointer());

  voronoiMap->SetLargestPossibleRegion( 
                  inputImage->GetLargestPossibleRegion() );

  voronoiMap->SetBufferedRegion( 
                  inputImage->GetBufferedRegion() );

  voronoiMap->SetRequestedRegion( 
                  inputImage->GetRequestedRegion() );

  voronoiMap->Allocate();

  OutputImagePointer distanceMap = GetDistanceMap();

  distanceMap->SetLargestPossibleRegion( 
                  inputImage->GetLargestPossibleRegion() );

  distanceMap->SetBufferedRegion( 
                  inputImage->GetBufferedRegion() );

  distanceMap->SetRequestedRegion( 
                  inputImage->GetRequestedRegion() );

  distanceMap->Allocate();


  typename OutputImageType::RegionType region  = voronoiMap->GetRequestedRegion();


  // find the largest of the image dimensions
  typename TInputImage::SizeType size = region.GetSize();
  unsigned int maxLength = 0;
  for( unsigned int dim=0; dim < TInputImage::ImageDimension; dim++)
  {
    if( maxLength < size[ dim ] )
    {
      maxLength = size[ dim ];
    }
  }



  SimpleImageRegionIterator< TInputImage >  it( inputImage,  region );
  SimpleImageRegionIterator< TOutputImage > ot( voronoiMap,  region );

  it.Begin();
  ot.Begin();

  if( m_InputIsBinary ) 
  {
    unsigned int npt = 1;
    while( !ot.IsAtEnd() )
    {
      if( it.Get() )
      {
        ot.Set( npt++ );
      }
      else 
      {
        ot.Set( 0 );
      }
    ++it;
    ++ot;
    }
  }
  else 
  {
    while( !ot.IsAtEnd() )
    {
      ot.Set( static_cast< typename OutputImageType::PixelType >( it.Get() ) );
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

  SimpleImageRegionIterator< VectorImageType >  ct( distanceComponents,  region );

  typename VectorImageType::PixelType maxValue;
  typename VectorImageType::PixelType minValue;

  for( unsigned int j=0; j<InputImageType::ImageDimension; j++ )
  {
    maxValue[j] =  2 * maxLength;
    minValue[j] =              0;
  }

  ot.Begin();
  ct.Begin();
  while( !ot.IsAtEnd() )
  {
    if( ot.Get() )
    {
      ct.Set( minValue );
    }
    else 
    {
      ct.Set( maxValue );
    }
  ++ot;
  ++ct;
  }
    
}




/**
 *  Post processing for computing the Voronoi Map
 */
template <class TInputImage,class TOutputImage>
void 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::ComputeVoronoiMap(void) 
{

  OutputImagePointer    voronoiMap          =  GetVoronoiMap();
  OutputImagePointer    distanceMap         =  GetDistanceMap();
  VectorImagePointer    distanceComponents  =  GetVectorDistanceMap();

  typename OutputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  SimpleImageRegionIterator< OutputImageType >  ot( voronoiMap,          region );
  SimpleImageRegionIterator< VectorImageType >  ct( distanceComponents,  region );
  SimpleImageRegionIterator< OutputImageType >  dt( distanceMap,         region );

  ot.Begin();
  ct.Begin();
  dt.Begin();
  while( ! ot.IsAtEnd() )
  {
    IndexType index = ct.GetIndex() + ct.Get();
    if( region.IsInside( index ) )
    { 
      ot.Set( voronoiMap->GetPixel( index ) );
    }

  OffsetType distanceVector = ct.Get();
  double distance = 0.0;
  for(unsigned int i=0; i<InputImageType::ImageDimension; i++)
  {
    distance += distanceVector[i] * distanceVector[i];
  }
  dt.Set( sqrt( distance ) );
  ++ot;
  ++ct;
  ++dt;
  }

}



/**
 *  Update locally the distance
 */
template <class TInputImage,class TOutputImage>
void 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::UpdateLocalDistance( 
 DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>::VectorImagePointer & components,\
          const IndexType  & here,
          const OffsetType & offset )
{

  IndexType  there            = here + offset;
  OffsetType offsetValueHere  = components->GetPixel( here  );
  OffsetType offsetValueThere = components->GetPixel( there );

  double norm1 = 0.0;
  double norm2 = 0.0;
  for( unsigned int i=0; i<InputImageType::ImageDimension; i++ )
  {
    const double v1 = static_cast< double >(  offsetValueHere[ i]  );
    const double v2 = static_cast< double >(  offsetValueThere[i] );
    norm1 +=  v1 * v1;
    norm2 +=  v2 * v2;
  }
  

  if( norm1 > norm2 ) 
  {
     components->GetPixel( here ) = offsetValueThere + offset;
  }

}





/**
 *  Compute Distance and Voronoi maps
 */
template <class TInputImage,class TOutputImage>
void 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GenerateData(void) 
{

  PrepareData();
 
  
  InputImagePointer     voronoiMap             =  GetVoronoiMap();
  VectorImagePointer    distanceComponents     =  GetVectorDistanceMap();
  
  typename InputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  IndexType   start   = region.GetIndex();
  SizeType    size    = region.GetSize();
  OffsetType  offset;

  for(unsigned int dim=0; dim<InputImageType::ImageDimension; dim++)
  {
    start [ dim ] += 1;
    size  [ dim ] -= 2;
    offset[ dim ] = 0;
  }

  region.SetIndex( start );
  region.SetSize ( size  );


  ReflectiveImageRegionIterator< VectorImageType > 
                                it( distanceComponents, region );
  it.Begin();

  while( !it.IsAtEnd() )
  {
    IndexType here = it.GetIndex();
    for(unsigned int dim=0; dim <VectorImageType::ImageDimension; dim++)
    {
      if( it.IsReflected(dim) ) 
      {
        offset[dim]++;
        UpdateLocalDistance( distanceComponents, here, offset );
        offset[dim]=0;
      }
      else
      {
        offset[dim]--;
        UpdateLocalDistance( distanceComponents, here, offset );
        offset[dim]=0;
      }
    }
    ++it;
  }
  
  ComputeVoronoiMap();

} // end GenerateData()




} // end namespace itk
