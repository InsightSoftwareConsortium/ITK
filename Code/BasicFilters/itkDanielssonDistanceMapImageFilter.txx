/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDanielssonDistanceMapImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkDanielssonDistanceMapImageFilter_txx
#define _itkDanielssonDistanceMapImageFilter_txx


#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkReflectiveImageRegionIterator.h"


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
::GetDistanceMap()
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
::GetVoronoiMap()
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
::GetVectorDistanceMap()
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
::PrepareData() 
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



  ImageRegionIteratorWithIndex< TInputImage >  it( inputImage,  region );
  ImageRegionIteratorWithIndex< TOutputImage > ot( voronoiMap,  region );

  it.GoToBegin();
  ot.GoToBegin();

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

  ImageRegionIteratorWithIndex< VectorImageType >  ct( distanceComponents,  region );

  typename VectorImageType::PixelType maxValue;
  typename VectorImageType::PixelType minValue;

  for( unsigned int j=0; j<InputImageDimension; j++ )
  {
    maxValue[j] =  2 * maxLength;
    minValue[j] =              0;
  }

  ot.GoToBegin();
  ct.GoToBegin();
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
::ComputeVoronoiMap() 
{

  OutputImagePointer    voronoiMap          =  GetVoronoiMap();
  OutputImagePointer    distanceMap         =  GetDistanceMap();
  VectorImagePointer    distanceComponents  =  GetVectorDistanceMap();

  typename OutputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  ImageRegionIteratorWithIndex< OutputImageType >  ot( voronoiMap,          region );
  ImageRegionIteratorWithIndex< VectorImageType >  ct( distanceComponents,  region );
  ImageRegionIteratorWithIndex< OutputImageType >  dt( distanceMap,         region );

  ot.GoToBegin();
  ct.GoToBegin();
  dt.GoToBegin();
  while( ! ot.IsAtEnd() )
  {
    IndexType index = ct.GetIndex() + ct.Get();
    if( region.IsInside( index ) )
    { 
      ot.Set( voronoiMap->GetPixel( index ) );
    }

  OffsetType distanceVector = ct.Get();
  double distance = 0.0;
  for(unsigned int i=0; i<InputImageDimension; i++)
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
DanielssonDistanceMapImageFilter<TInputImage, TOutputImage>
::UpdateLocalDistance(VectorImageType* components,
                      const IndexType& here,
                      const OffsetType& offset)
{
  IndexType  there            = here + offset;
  OffsetType offsetValueHere  = components->GetPixel( here  );
  OffsetType offsetValueThere = components->GetPixel( there );

  double norm1 = 0.0;
  double norm2 = 0.0;
  for( unsigned int i=0; i<InputImageDimension; i++ )
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
::GenerateData() 
{

  PrepareData();
 
  
  InputImagePointer     voronoiMap             =  GetVoronoiMap();
  VectorImagePointer    distanceComponents     =  GetVectorDistanceMap();
  
  typename InputImageType::RegionType region  = voronoiMap->GetRequestedRegion();

  IndexType   start   = region.GetIndex();
  SizeType    size    = region.GetSize();
  OffsetType  offset;

  for(unsigned int dim=0; dim<InputImageDimension; dim++)
    {
    start [ dim ] += 1;
    size  [ dim ] -= 2;
    offset[ dim ] = 0;
    }

  region.SetIndex( start );
  region.SetSize ( size  );

  ReflectiveImageRegionIterator< VectorImageType > 
                                it( distanceComponents, region );
  it.GoToBegin();

  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  updateVisits = region.GetNumberOfPixels()/10;
  if ( updateVisits < 1 ) 
  {
    updateVisits = 1;
  }
 
  while( !it.IsAtEnd() )
  {

    if ( !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }

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
    ++i;
  }
  
  ComputeVoronoiMap();

} // end GenerateData()




/**
 *  Print Self
 */
template <class TInputImage,class TOutputImage>
void 
DanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Danielson Distance: " << std::endl;
  os << indent << "Input Is Binary   : " << m_InputIsBinary << std::endl;
  os << indent << "Squared Distance  : " << m_SquaredDistance << std::endl;

}



} // end namespace itk

#endif
