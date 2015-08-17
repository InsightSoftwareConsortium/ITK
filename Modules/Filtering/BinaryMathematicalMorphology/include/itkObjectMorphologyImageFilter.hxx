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
#ifndef itkObjectMorphologyImageFilter_hxx
#define itkObjectMorphologyImageFilter_hxx

#include <climits>

#include "itkNumericTraits.h"
#include "itkObjectMorphologyImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::ObjectMorphologyImageFilter():
  m_Kernel()
{
  m_DefaultBoundaryCondition.SetConstant(NumericTraits< PixelType >::ZeroValue());
  m_BoundaryCondition = &m_DefaultBoundaryCondition;

  m_UseBoundaryCondition = false;

  m_ObjectValue = NumericTraits< PixelType >::OneValue();
  //this->SetNumberOfThreads(1);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( m_Kernel.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is outside largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::BeforeThreadedGenerateData()
{
  if ( Math::ExactlyEquals(m_ObjectValue, NumericTraits< typename TInputImage::PixelType >::ZeroValue()) )
    {
    this->GetOutput()->FillBuffer(1);
    }
  else
    {
    this->GetOutput()->FillBuffer(0);
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  ImageRegionConstIterator< TInputImage > iRegIter;
  ImageRegionIterator< TOutputImage >     oRegIter;
  iRegIter = ImageRegionConstIterator< InputImageType >(this->GetInput(),
                                                        outputRegionForThread);
  oRegIter = ImageRegionIterator< OutputImageType >(this->GetOutput(),
                                                    outputRegionForThread);
  /* Copy the input image to the output image - then only boundary pixels
   * need to be changed in the output image */
  iRegIter.GoToBegin();
  oRegIter.GoToBegin();
  while ( !oRegIter.IsAtEnd() )
    {
    if ( Math::NotExactlyEquals(oRegIter.Get(), m_ObjectValue) )
      {
      oRegIter.Set( iRegIter.Get() );
      }
    ++oRegIter;
    ++iRegIter;
    }

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >
  ::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > fC;
  faceList = fC( this->GetInput(), outputRegionForThread, m_Kernel.GetRadius() );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >
  ::FaceListType::iterator fit;

  // Setup the kernel that spans the immediate neighbors of the current
  // input pixel - used to determine if that pixel abuts a non-object
  // pixel, i.e., is a boundary pixel
  RadiusType bKernelSize;
  bKernelSize.Fill(1);

  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  OutputNeighborhoodIteratorType oSNIter;
  InputNeighborhoodIteratorType  iSNIter;
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    oSNIter = OutputNeighborhoodIteratorType(m_Kernel.GetRadius(),
                                             this->GetOutput(), *fit);
    // No need to overwrite on output...and m_BoundaryCondition is
    // templated over inputImageType - and cannot be applied to the
    // output image
    //oSNIter.OverrideBoundaryCondition(m_BoundaryCondition);
    oSNIter.GoToBegin();

    iSNIter = InputNeighborhoodIteratorType(bKernelSize,
                                            this->GetInput(), *fit);
    iSNIter.OverrideBoundaryCondition(m_BoundaryCondition);
    iSNIter.GoToBegin();

    while ( !iSNIter.IsAtEnd() )
      {
      if ( Math::ExactlyEquals(iSNIter.GetCenterPixel(), m_ObjectValue) )
        {
        if ( this->IsObjectPixelOnBoundary(iSNIter) )
          {
          this->Evaluate(oSNIter, m_Kernel);
          }
        }
      ++iSNIter;
      ++oSNIter;
      progress.CompletedPixel();
      }
    }
}

// Use neighborhood iter to determine if pixel touches a non-object pixel
template< typename TInputImage, typename TOutputImage, typename TKernel >
bool
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::IsObjectPixelOnBoundary(const InputNeighborhoodIteratorType & iNIter)
{
  static const unsigned int s =
    (unsigned int)std::pow( (double)3.0,
                           (double)( ImageDimension ) );

  PixelType    tf;
  unsigned int i;
  bool         isInside = true;

  if ( m_UseBoundaryCondition )
    {
    for ( i = 0; i < s; i++ )
      {
      tf = iNIter.GetPixel(i);
      if ( Math::NotExactlyEquals(tf, m_ObjectValue) )
        {
        return true;
        }
      }
    }
  else
    {
    for ( i = 0; i < s; i++ )
      {
      tf = iNIter.GetPixel(i, isInside);
      if ( Math::NotExactlyEquals(tf, m_ObjectValue) && isInside )
        {
        return true;
        }
      }
    }

  return false;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Boundary condition: "
     << typeid( *m_BoundaryCondition ).name() << std::endl;
  os << indent << "Use boundary condition: "
     << m_UseBoundaryCondition << std::endl;

  os << indent << "ObjectValue: " << m_ObjectValue << std::endl;
  os << indent << "Kernel: " << m_Kernel << std::endl;
}
} // end namespace itk
#endif
