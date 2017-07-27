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
#ifndef itkNormalDistributionImageSource_hxx
#define itkNormalDistributionImageSource_hxx

#include "itkNormalDistributionImageSource.h"
#include "itkNormalVariateGenerator.h"

#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TImage >
NormalDistributionImageSource< TImage >
::NormalDistributionImageSource()
{
}


template< typename TImage >
void
NormalDistributionImageSource< TImage >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
}


template< typename TImage >
void
NormalDistributionImageSource< TImage >
::ThreadedGenerateData( const OutputRegionType & outputRegion, ThreadIdType threadId )
{
  ImageType * output = this->GetOutput();

  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();
  normalGenerator->Initialize( 101 );

  const SizeValueType size0 = outputRegion.GetSize( 0 );
  if( size0 == 0 )
    {
    return;
    }
  const SizeValueType numberOfLinesToProcess = outputRegion.GetNumberOfPixels() / size0;

  typedef ImageScanlineIterator< ImageType > IteratorType;
  IteratorType it( output, outputRegion );
  ProgressReporter progress( this, threadId, numberOfLinesToProcess );

  while( !it.IsAtEnd() )
    {
    while( !it.IsAtEndOfLine() )
      {
      it.Set( normalGenerator->GetVariate() );
      ++it;
      }
    it.NextLine();
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif // itkNormalDistributionImageSource_hxx
