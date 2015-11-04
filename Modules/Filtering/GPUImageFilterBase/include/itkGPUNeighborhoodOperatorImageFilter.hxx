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
#ifndef itkGPUNeighborhoodOperatorImageFilter_hxx
#define itkGPUNeighborhoodOperatorImageFilter_hxx

#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkProgressReporter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"

namespace itk
{
/*
template< typename TInputImage, typename TOutputImage, typename TOperatorValueType >
void
GPUNeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr =
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
  inputRequestedRegion.PadByRadius( m_Operator.GetRadius() );

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
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}
*/

template< typename TInputImage, typename TOutputImage, typename TOperatorValueType, typename TParentImageFilter >
GPUNeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType, TParentImageFilter >
::GPUNeighborhoodOperatorImageFilter()
{
  // Create GPU buffer to store neighborhood coefficient.
  // This will be used as __constant memory in the GPU kernel.
  m_NeighborhoodGPUBuffer = NeighborhoodGPUBufferType::New();

  std::ostringstream defines;

  if(TInputImage::ImageDimension > 3 || TInputImage::ImageDimension < 1)
    {
    itkExceptionMacro("GPUneighborhoodOperatorImageFilter supports 1/2/3D image.");
    }

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";

  defines << "#define INTYPE ";
  GetTypenameInString( typeid ( typename TInputImage::PixelType ), defines );

  defines << "#define OUTTYPE ";
  GetTypenameInString( typeid ( typename TOutputImage::PixelType ), defines );

  defines << "#define OPTYPE ";
  GetTypenameInString( typeid ( TOperatorValueType ), defines );

  std::cout << "Defines: " << defines.str() << std::endl;

  const char* GPUSource = GPUNeighborhoodOperatorImageFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  m_NeighborhoodOperatorFilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("NeighborOperatorFilter");
}

template< typename TInputImage, typename TOutputImage, typename TOperatorValueType, typename TParentImageFilter >
void
GPUNeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType, TParentImageFilter >
::SetOperator(const OutputNeighborhoodType & p)
{
  /** Call CPU SetOperator */
  CPUSuperclass::SetOperator( p );

  /** Create GPU memory for operator coefficients */
  m_NeighborhoodGPUBuffer->Initialize();

  typename NeighborhoodGPUBufferType::IndexType  index;
  typename NeighborhoodGPUBufferType::SizeType   size;
  typename NeighborhoodGPUBufferType::RegionType region;

  for(unsigned int i=0; i<ImageDimension; i++)
    {
    index[i] = 0;
    size[i]  = (unsigned int)(p.GetSize(i) );
    }
  region.SetSize( size );
  region.SetIndex( index );

  m_NeighborhoodGPUBuffer->SetRegions( region );
  m_NeighborhoodGPUBuffer->Allocate();

  /** Copy coefficients */
  ImageRegionIterator<NeighborhoodGPUBufferType> iit(m_NeighborhoodGPUBuffer,
                                                     m_NeighborhoodGPUBuffer->GetLargestPossibleRegion() );

  typename OutputNeighborhoodType::ConstIterator nit = p.Begin();

  for(iit.GoToBegin(); !iit.IsAtEnd(); ++iit, ++nit)
    {
    iit.Set( static_cast< typename NeighborhoodGPUBufferType::PixelType >( *nit ) );
    }

  /** Mark GPU dirty */
  m_NeighborhoodGPUBuffer->GetGPUDataManager()->SetGPUBufferDirty();
}

template< typename TInputImage, typename TOutputImage, typename TOperatorValueType, typename TParentImageFilter >
void
GPUNeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType, TParentImageFilter >
::GPUGenerateData()
{
  int kHd = m_NeighborhoodOperatorFilterGPUKernelHandle;

  typedef typename itk::GPUTraits< TInputImage >::Type  GPUInputImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type GPUOutputImage;
  typedef GPUImageDataManager<GPUInputImage>            GPUInputManagerType;
  typedef GPUImageDataManager<GPUOutputImage>           GPUOutputManagerType;

  typename GPUInputImage::Pointer  inPtr =  dynamic_cast< GPUInputImage * >( this->ProcessObject::GetInput(0) );
  typename GPUOutputImage::Pointer otPtr =  dynamic_cast< GPUOutputImage * >( this->ProcessObject::GetOutput(0) );

  //typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();
  typename GPUOutputImage::SizeType outSize = otPtr->GetBufferedRegion().GetSize();

  int radius[3];
  int imgSize[3];

  radius[0] = radius[1] = radius[2] = 0;
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)TInputImage::ImageDimension;

  for(int i=0; i<ImageDim; i++)
    {
    radius[i]  = (this->GetOperator() ).GetRadius(i);
    imgSize[i] = outSize[i];
    }

  size_t localSize[3], globalSize[3];
  localSize[0] = localSize[1] = localSize[2] = OpenCLGetLocalBlockSize(ImageDim);
  for(int i=0; i<ImageDim; i++)
    {
    globalSize[i] = localSize[i]*(unsigned int)ceil( (float)outSize[i]/(float)localSize[i]); //
                                                                                             // total
                                                                                             // #
                                                                                             // of
                                                                                             // threads
    }

  // arguments set up
  cl_uint argidx = 0;
  this->m_GPUKernelManager->template SetKernelArgWithImageAndBufferedRegion<GPUInputManagerType>
    (kHd, argidx, inPtr->GetDataManager() );
  this->m_GPUKernelManager->template SetKernelArgWithImageAndBufferedRegion<GPUOutputManagerType>
    (kHd, argidx, otPtr->GetDataManager() );
  this->m_GPUKernelManager->SetKernelArgWithImage(kHd, argidx++, m_NeighborhoodGPUBuffer->GetGPUDataManager() );

  for(int i=0; i<(int)TInputImage::ImageDimension; i++)
    {
    this->m_GPUKernelManager->SetKernelArg(kHd, argidx++, sizeof(int), &(radius[i]) );
    }

  //for(int i=0; i<(int)TInputImage::ImageDimension; i++)
  //  {
  //  this->m_GPUKernelManager->SetKernelArg(kHd, argidx++, sizeof(int), &(imgSize[i]) );
  //  }

  // launch kernel
  this->m_GPUKernelManager->LaunchKernel(kHd, ImageDim, globalSize, localSize);

  /*
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >
  BFC;

  typedef typename BFC::FaceListType FaceListType;

  NeighborhoodInnerProduct< InputImageType, OperatorValueType, ComputingPixelType > smartInnerProduct;
  BFC                                                           faceCalculator;
  FaceListType                                                  faceList;

  // This filter can only operate on data types that are signed.
   if ( !NumericTraits< typename NumericTraits< OutputPixelType  >::ValueType >::is_signed )
    {
    itkExceptionMacro(<< "This filter can only create images of signed data type.");
    }

  // Allocate output
  OutputImageType *output = this->GetOutput();

  const InputImageType *input   = this->GetInput();

  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels.
  faceList = faceCalculator( input, outputRegionForThread,
                             m_Operator.GetRadius() );

  typename FaceListType::iterator fit;
  ImageRegionIterator< OutputImageType > it;

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process non-boundary region and each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  ConstNeighborhoodIterator< InputImageType > bit;
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit =
      ConstNeighborhoodIterator< InputImageType >(m_Operator.GetRadius(),
                                                  input, *fit);
    bit.OverrideBoundaryCondition(m_BoundsCondition);
    it = ImageRegionIterator< OutputImageType >(output, *fit);
    bit.GoToBegin();
    while ( !bit.IsAtEnd() )
      {
      it.Value() = static_cast< typename OutputImageType::PixelType >( smartInnerProduct(bit, m_Operator) );
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
    */
}

} // end namespace itk

#endif
