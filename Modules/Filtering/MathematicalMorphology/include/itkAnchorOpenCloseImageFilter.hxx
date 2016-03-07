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
#ifndef itkAnchorOpenCloseImageFilter_hxx
#define itkAnchorOpenCloseImageFilter_hxx

#include "itkAnchorOpenCloseImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkAnchorUtilities.h"
#include "itkImageRegionIterator.h"
namespace itk
{
template< typename TImage, typename TKernel, typename TCompare1, typename TCompare2 >
AnchorOpenCloseImageFilter< TImage, TKernel, TCompare1, TCompare2 >
::AnchorOpenCloseImageFilter():
  m_Boundary1( NumericTraits< InputImagePixelType >::ZeroValue() ),
  m_Boundary2( NumericTraits< InputImagePixelType >::ZeroValue() )
{
}

template< typename TImage, typename TKernel, typename TCompare1, typename TCompare2 >
void
AnchorOpenCloseImageFilter< TImage, TKernel, TCompare1, TCompare2 >
::ThreadedGenerateData(const InputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // check that we are using a decomposable kernel
  if ( !this->GetKernel().GetDecomposable() )
    {
    itkExceptionMacro("Anchor morphology only works with decomposable structuring elements");
    return;
    }
  // TFunction1 will be < for erosions
  // TFunction2 will be <=

  // the initial version will adopt the methodology of loading a line
  // at a time into a buffer vector, carrying out the opening or
  // closing, and then copy the result to the output. Hopefully this
  // will improve cache performance when working along non raster
  // directions.

  AnchorLineErodeType  AnchorLineErode;
  AnchorLineDilateType AnchorLineDilate;

  AnchorLineOpenType AnchorLineOpen;

  ProgressReporter progress(this, threadId, static_cast<SizeValueType>( this->GetKernel().GetLines().size() )* 2 + 1);

  InputImageConstPointer input = this->GetInput();

  InputImageRegionType IReg = outputRegionForThread;
  // seem to need a double padding for the multi threaded case because
  // we get boundary effects otherwise
  IReg.PadByRadius( this->GetKernel().GetRadius() );
  IReg.PadByRadius( this->GetKernel().GetRadius() );
  IReg.Crop( this->GetInput()->GetRequestedRegion() );

  // allocate an internal buffer
  typename InputImageType::Pointer internalbuffer = InputImageType::New();
  internalbuffer->SetRegions(IReg);
  internalbuffer->Allocate();
  InputImagePointer output = internalbuffer;

  // get the region size
  InputImageRegionType OReg = outputRegionForThread;
  // maximum buffer length is sum of dimensions
  unsigned int bufflength = 0;
  for ( unsigned i = 0; i < TImage::ImageDimension; i++ )
    {
    bufflength += IReg.GetSize()[i];
    }

  // compat
  bufflength += 2;

  std::vector<InputImagePixelType> buffer(bufflength);
  std::vector<InputImagePixelType> inbuffer(bufflength);

  // iterate over all the structuring elements
  typename KernelType::DecompType decomposition = this->GetKernel().GetLines();
  BresType BresLine;

  // first stage -- all of the erosions if we are doing an opening
  for ( unsigned i = 0; i < decomposition.size() - 1; i++ )
    {
    KernelLType     ThisLine = decomposition[i];
    BresOffsetArray TheseOffsets = BresLine.BuildLine(ThisLine, bufflength);
    unsigned int    SELength = GetLinePixels< KernelLType >(ThisLine);
    // want lines to be odd
    if ( !( SELength % 2 ) )
      {
      ++SELength;
      }
    AnchorLineErode.SetSize(SELength);

    InputImageRegionType BigFace = MakeEnlargedFace< InputImageType, KernelLType >(input, IReg, ThisLine);
    DoAnchorFace< TImage, BresType,
                  AnchorLineErodeType,
                  KernelLType >(input, output, m_Boundary1, ThisLine, AnchorLineErode,
                                TheseOffsets, inbuffer, buffer, IReg, BigFace);

    // after the first pass the input will be taken from the output
    input = internalbuffer;
    progress.CompletedPixel();
    }
  // now do the opening in the middle of the chain
    {
    unsigned    i = static_cast<unsigned>( decomposition.size() ) - 1;
    KernelLType ThisLine = decomposition[i];
    typename BresType::OffsetArray TheseOffsets = BresLine.BuildLine(ThisLine, bufflength);
    unsigned int SELength = GetLinePixels< KernelLType >(ThisLine);
    // want lines to be odd
    if ( !( SELength % 2 ) )
      {
      ++SELength;
      }

    AnchorLineOpen.SetSize(SELength);
    InputImageRegionType BigFace = MakeEnlargedFace< InputImageType, KernelLType >(input, IReg, ThisLine);

    // Now figure out which faces of the image we should be starting
    // from with this line
    DoFaceOpen(input, output, m_Boundary1, ThisLine, AnchorLineOpen,
               TheseOffsets, buffer,
               IReg, BigFace);
    // equivalent to two passes
    progress.CompletedPixel();
    progress.CompletedPixel();
    }

  // Now for the rest of the dilations -- note that i needs to be signed
  for ( int i = static_cast<int>( decomposition.size() ) - 2; i >= 0; --i )
    {
    KernelLType ThisLine = decomposition[i];
    typename BresType::OffsetArray TheseOffsets = BresLine.BuildLine(ThisLine, bufflength);
    unsigned int SELength = GetLinePixels< KernelLType >(ThisLine);
    // want lines to be odd
    if ( !( SELength % 2 ) )
      {
      ++SELength;
      }

    AnchorLineDilate.SetSize(SELength);

    InputImageRegionType BigFace = MakeEnlargedFace< InputImageType, KernelLType >(input, IReg, ThisLine);
    DoAnchorFace< TImage, BresType,
                  AnchorLineDilateType,
                  KernelLType >(input, output, m_Boundary2, ThisLine, AnchorLineDilate,
                                TheseOffsets, inbuffer, buffer, IReg, BigFace);

    progress.CompletedPixel();
    }

  // copy internal buffer to output
  typedef ImageRegionIterator< InputImageType > IterType;
  IterType oit(this->GetOutput(), OReg);
  IterType iit(internalbuffer, OReg);
  for ( oit.GoToBegin(), iit.GoToBegin(); !oit.IsAtEnd(); ++oit, ++iit )
    {
    oit.Set( iit.Get() );
    }
  progress.CompletedPixel();

}

template< typename TImage, typename TKernel, typename TCompare1, typename TCompare2 >
void
AnchorOpenCloseImageFilter< TImage, TKernel, TCompare1, TCompare2 >
::DoFaceOpen(InputImageConstPointer input,
             InputImagePointer output,
             InputImagePixelType border,
             KernelLType line,
             AnchorLineOpenType & AnchorLineOpen,
             const BresOffsetArray LineOffsets,
             std::vector<InputImagePixelType> & outbuffer,
             const InputImageRegionType AllImage,
             const InputImageRegionType face)
{
  // iterate over the face

  // we can't use an iterator with a region outside the image. All we need here
  // is to
  // iterate over all the indexes of the face, without accessing the content of
  // the image.
  // I can't find any cleaner way, so we use a dumb image, not even allocated,
  // to iterate
  // over all the indexes inside the region.
  //
  // typedef ImageRegionConstIteratorWithIndex<TImage> ItType;
  // ItType it(input, face);

  typename TImage::Pointer dumbImg = TImage::New();
  dumbImg->SetRegions(face);

  KernelLType NormLine = line;
  NormLine.Normalize();
  // set a generous tolerance
  float tol = 1.0 / LineOffsets.size();
  for ( unsigned int it = 0; it < face.GetNumberOfPixels(); it++ )
    {
    typename TImage::IndexType Ind = dumbImg->ComputeIndex(it);
    unsigned start, end, len;
    if ( FillLineBuffer< TImage, BresType, KernelLType >(input,
                                                         Ind,
                                                         NormLine,
                                                         tol,
                                                         LineOffsets,
                                                         AllImage,
                                                         outbuffer,
                                                         start,
                                                         end) )
      {
      len = end - start + 1;
      // compat
      outbuffer[0] = border;
      outbuffer[len + 1] = border;
      AnchorLineOpen.DoLine(outbuffer, len + 2);  // compat
      CopyLineToImage< TImage, BresType >(output, Ind, LineOffsets, outbuffer, start, end);
      }
    }
}

template< typename TImage, typename TKernel, typename TCompare1, typename TCompare2 >
void
AnchorOpenCloseImageFilter< TImage, TKernel, TCompare1, TCompare2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
