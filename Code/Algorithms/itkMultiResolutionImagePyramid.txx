/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImagePyramid.txx
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
#ifndef _itkMultiResolutionImagePyramid_txx
#define _itkMultiResolutionImagePyramid_txx

#include "itkMultiResolutionImagePyramid.h"
#include "itkShrinkImageFilter.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkVector.h"
#include "vnl/vnl_math.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::MultiResolutionImagePyramid()
{
  this->SetNumberOfLevels( 2 );
  this->SetCurrentLevel( 0 );
}


/**
 * Set the current level
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetCurrentLevel(
unsigned int level )
{

  if( level == m_CurrentLevel ) return;

  this->Modified();

  // clamp current level to be between 0 and NumberOfLevels - 1
  if( level > this->GetNumberOfLevels() - 1 )
    {
    m_CurrentLevel = this->GetNumberOfLevels() - 1;
    }
  else
    {
    m_CurrentLevel = level;
    }

}


/**
 * Set the number of computation levels
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetNumberOfLevels(
unsigned int num )
{
  // clamp value to be at least one
  m_NumberOfLevels = num;
  if( m_NumberOfLevels < 1 ) m_NumberOfLevels = 1;

  // resize the schedules
  m_Schedule.resize( m_NumberOfLevels, ImageDimension );

  // determine initial shrink factor
  unsigned int startfactor = 1;
  startfactor = startfactor << ( m_NumberOfLevels - 1 );

  // set the starting shrink factors
  this->SetStartingShrinkFactors( startfactor );

  this->Modified();

}


/**
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetStartingShrinkFactors(
unsigned int factor )
{

  unsigned int array[ImageDimension];
  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    array[dim] = factor;
    }

  this->SetStartingShrinkFactors( array );

}


/**
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::SetStartingShrinkFactors(
unsigned int * factors )
{

  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    m_Schedule[0][dim] = factors[dim];
    if( m_Schedule[0][dim] == 1 ) m_Schedule[0][dim] = 1;
    }

  for( unsigned int level = 1; level < m_NumberOfLevels; ++level )
    {
    for( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      m_Schedule[level][dim] = m_Schedule[level-1][dim] / 2;
      if( m_Schedule[level][dim] == 0 ) m_Schedule[level][dim] = 1;
      }
    }

  this->Modified();

}


/**
 * GenerateData
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::GenerateData()
{

  // Set up a mini-pipeline of smoother/downsampler pairs
  typedef ShrinkImageFilter<TInputImage,TOutputImage> 
		DownSamplerType;

  typedef RecursiveGaussianImageFilter<TInputImage,TOutputImage,double>
    SmootherType;
 
  typename DownSamplerType::Pointer downsampler[ImageDimension];
  typename SmootherType::Pointer smoother[ImageDimension];

  int lastValidFilter = -1;
  Vector<unsigned int,ImageDimension> factors;

  //FIXME: the smoothing part is skipped for now until
  //RecursiveGaussianImageFilter is fixed.
  //Currently RecursiveGaussianImageFilter zero pads the image
  //this causes large gradient at the image border making
  //the registration unstable.

  for( int j = 0; j < ImageDimension; j++ )
    {

    factors.Fill( 1 );
 
    factors[j] = m_Schedule[m_CurrentLevel][j];
    double stddev = 0.5 * factors[j];

    if( factors[j] > 1 )
     {
      // smooth and downsample only if factor > 1
      downsampler[j] = DownSamplerType::New();
      smoother[j] = SmootherType::New();

      downsampler[j]->SetShrinkFactors( factors.Begin() );
      smoother[j]->SetSigma( stddev );
      smoother[j]->SetDirection( j );

      if( lastValidFilter == -1 )
        {
        // FIXME: skipped smoothing for now
        //smoother[j]->SetInput( this->GetInput() );
        downsampler[j]->SetInput( this->GetInput() );
        }
      else
        {
        // FIXME: skipped smoothing for now
        //smoother[j]->SetInput( downsampler[lastValidFilter]->GetOutput() );
        downsampler[j]->SetInput( downsampler[lastValidFilter]->GetOutput() );
        }

      // FIXME: skipped smoothing for now
      //downsampler[j]->SetInput( smoother[j]->GetOutput() );

      lastValidFilter = j;

      }
    }

  if( lastValidFilter == -1 )
    {
    // all factors are 1 - copy the image to the output
    factors.Fill( 1 );
    downsampler[0] = DownSamplerType::New();
    downsampler[0]->SetShrinkFactors( factors.Begin() );
    downsampler[0]->SetInput( this->GetInput() );
    lastValidFilter = 0;
    }

  // update the pipeline
  downsampler[lastValidFilter]->Update();

  // connect it to the output of this filter
  this->SetOutput( downsampler[lastValidFilter]->GetOutput() );

}


/**
 * PrintSelf method
 */
template <class TInputImage, class TOutputImage>
void
MultiResolutionImagePyramid<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Schedule: " << m_Schedule << std::endl;

}



} // namespace itk

#endif
