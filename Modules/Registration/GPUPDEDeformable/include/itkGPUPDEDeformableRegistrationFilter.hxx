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
#ifndef itkGPUPDEDeformableRegistrationFilter_hxx
#define itkGPUPDEDeformableRegistrationFilter_hxx

#include "itkGPUPDEDeformableRegistrationFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkDataObject.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

#include "itkMath.h"

// #define NOT_REORDER_GPU_MEMORY

namespace itk
{
/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::GPUPDEDeformableRegistrationFilter()
{
  this->SetNumberOfIterations(10);

  this->SetStandardDeviations(1.0);
  this->SetUpdateFieldStandardDeviations(1.0);

  m_TempField = DisplacementFieldType::New();

  /** Build GPU Program */

  std::ostringstream defines;

  if( TFixedImage::ImageDimension > 3 || TFixedImage::ImageDimension < 1 )
    {
    itkExceptionMacro("GPUDenseFiniteDifferenceImageFilter supports 1/2/3D image.");
    }

  defines << "#define DIM_" << TDisplacementField::ImageDimension << "\n";

  // PixelType is a Vector
  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString( typeid ( DeformationScalarType ), defines );

  std::cout << "Defines: " << defines.str() << std::endl;

  typedef const char * GPUCodeType;
  GPUCodeType GPUSource = GPUPDEDeformableRegistrationFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
#ifdef NOT_REORDER_GPU_MEMORY
  m_SmoothDisplacementFieldGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("SmoothingFilter");
#else
  m_SmoothDisplacementFieldGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("SmoothingFilterReorder");
#endif

}

/*
 * Standard PrintSelf method.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::PrintSelf(std::ostream & os, Indent indent) const
{
  GPUSuperclass::PrintSelf(os, indent);
}

/*
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::InitializeIteration()
{
  MovingImageConstPointer movingPtr = this->GetMovingImage();
  FixedImageConstPointer  fixedPtr = this->GetFixedImage();

  if( !movingPtr || !fixedPtr )
    {
    itkExceptionMacro(<< "Fixed and/or moving image not set");
    }

  // update variables in the equation object
  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast<GPUPDEDeformableRegistrationFunctionType *>
    ( this->GetDifferenceFunction().GetPointer() );

  if( !f )
    {
    itkExceptionMacro(<< "FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
    }

  f->SetFixedImage(fixedPtr);
  f->SetMovingImage(movingPtr);

  this->GPUSuperclass::InitializeIteration();
}

/*
 * Override the default implementation for the case when the
 * initial deformation is not set.
 * If the initial deformation is not set, the output is
 * fill with zero vectors.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::CopyInputToOutput()
{
  typename GPUSuperclass::InputImageType::ConstPointer inputPtr  = this->GetInput();

  if( inputPtr )
    {
    this->GPUSuperclass::CopyInputToOutput();
    }
  else
    {
    typename GPUSuperclass::PixelType zeros;
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      zeros[j] = 0;
      }

    typedef typename itk::GPUTraits<TDisplacementField>::Type GPUOutputImage;
    typename GPUOutputImage::Pointer output = dynamic_cast<GPUOutputImage *>(this->GetOutput() );

    ImageRegionIterator<OutputImageType> out( output, output->GetRequestedRegion() );

    while( !out.IsAtEnd() )
      {
      out.Value() =  zeros;
      ++out;
      }

    // copy the deformation output to gpu
    output->GetGPUDataManager()->SetGPUDirtyFlag(true);
    }
}

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::GenerateOutputInformation()
{
  typename DataObject::Pointer output;
//   typename OutputImageType::Pointer output;

  if( this->GetInput(0) )
    {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->GPUSuperclass::GenerateOutputInformation();
    }
  else if( this->GetFixedImage() )
    {
    // Initial deforamtion field is not set.
    // Copy information from the fixed image.
    for( unsigned int idx = 0; idx <
         this->GetNumberOfOutputs(); ++idx )
      {
      output = this->GetOutput(idx);
      if( output )
        {
        output->CopyInformation( this->GetFixedImage() );
        }
      }
    }
}

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::GenerateInputRequestedRegion()
{
  // call the GPUSuperclass's implementation
  GPUSuperclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  MovingImagePointer movingPtr =
    const_cast<MovingImageType *>( this->GetMovingImage() );
  if( movingPtr )
    {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  DisplacementFieldPointer inputPtr =
    const_cast<DisplacementFieldType *>( this->GetInput() );
  DisplacementFieldPointer outputPtr = this->GetOutput();
  FixedImagePointer       fixedPtr =
    const_cast<FixedImageType *>( this->GetFixedImage() );

  if( inputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

  if( fixedPtr )
    {
    fixedPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }
}

/*
 * Release memory of internal buffers
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::PostProcessOutput()
{
  // Call GPUFiniteDifferenceImageFilter::PostProcessOutput().
  // This is similar to the CPU version where
  // PDEDeformableRegistrationFilter::PostProcessOutput() calls
  // FiniteDifferenceImageFilter::PostProcessOutput().
  this->GPUSuperclass::PostProcessOutput();

  // Release memory for smoothing buffer
  m_TempField->Initialize();
  // Release memory for smoothing kernels
  for( unsigned int dir = 0; dir < ImageDimension; dir++ )
    {
    m_GPUSmoothingKernels[dir]->Initialize();
    delete m_SmoothingKernels[dir];
    m_SmoothingKernels[dir] = ITK_NULLPTR;

    m_UpdateFieldGPUSmoothingKernels[dir]->Initialize();
    delete m_UpdateFieldSmoothingKernels[dir];
    m_UpdateFieldSmoothingKernels[dir] = ITK_NULLPTR;
    }

  m_GPUImageSizes->Initialize();
  delete m_ImageSizes;
  m_ImageSizes = ITK_NULLPTR;

  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast<GPUPDEDeformableRegistrationFunctionType *>
    ( this->GetDifferenceFunction().GetPointer() );
  f->GPUReleaseMetricData();

  // update the cpu buffer from gpu
  this->GetOutput()->GetBufferPointer();
}

/*
 * Initialize flags
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::Initialize()
{
  this->GPUSuperclass::Initialize();

  this->AllocateSmoothingBuffer();

  FixedImageConstPointer fixedImage = this->GetFixedImage();
  unsigned int           numPixels = (unsigned int)
    (fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() );

  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast<GPUPDEDeformableRegistrationFunctionType *>
    ( this->GetDifferenceFunction().GetPointer() );
  f->GPUAllocateMetricData(numPixels);
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::GPUSmoothVectorField(DisplacementFieldPointer field,
                       typename GPUDataManager::Pointer GPUSmoothingKernels[],
                       int GPUSmoothingKernelSizes[])
{
  typedef typename itk::GPUTraits<TDisplacementField>::Type GPUBufferImage;
  typedef typename itk::GPUTraits<TDisplacementField>::Type GPUOutputImage;

  typename GPUBufferImage::Pointer  bfPtr =  dynamic_cast<GPUBufferImage *>( m_TempField.GetPointer() );
  typename GPUOutputImage::Pointer  otPtr =  dynamic_cast<GPUOutputImage *>( field.GetPointer() );

  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)TDisplacementField::ImageDimension;
  if( ImageDim > 3 )
    {
    itkExceptionMacro("GPUSmoothDisplacementField supports 1/2/3D images.");
    return;
    }
  for( int i = 0; i < ImageDim; i++ )
    {
    imgSize[i] = outSize[i];
    }

  size_t       localSize[3], globalSize[3];
  size_t       blockSize;
  int          indir, outdir;                                 // direction for
                                                              // smoothing
                                                              // and/or storage
  size_t kernelWorkGroupSize;
  this->m_GPUKernelManager->GetKernelWorkGroupInfo(
    m_SmoothDisplacementFieldGPUKernelHandle,
    CL_KERNEL_WORK_GROUP_SIZE,
    &kernelWorkGroupSize);

  size_t deviceWorkItemSizes[3];
  this->m_GPUKernelManager->GetDeviceInfo(
    CL_DEVICE_MAX_WORK_ITEM_SIZES,
    sizeof(deviceWorkItemSizes), deviceWorkItemSizes);

  for( indir = 0; indir < ImageDim; indir++ )
    {
    // If blockSize is too big, there will be lot of wasted threads at image
    // boundary, and each work group requires a lot of resources.
    // If blockSize is too small, threads in a work group won't share much
    // memory loading.
    blockSize = kernelWorkGroupSize <= deviceWorkItemSizes[indir] ?
                kernelWorkGroupSize : deviceWorkItemSizes[indir];
    blockSize = blockSize <= outSize[indir] ? blockSize : outSize[indir];
    blockSize = blockSize <= 64 ? blockSize : 64;
    //std::cout << "indir=" << indir << " blockSize=" << blockSize << std::endl;

    for( int i = 0; i < 3; i++ )
      {
      localSize[i] = 1;
      }
    localSize[indir] = blockSize;

    for( int i = 0; i < ImageDim; i++ )
      {
      globalSize[i] = localSize[i] * (unsigned int)ceil( (float)outSize[i] / (float)localSize[i]); //
                                                                                                   //
                                                                                                   // total
                                                                                                   //
                                                                                                   // #
                                                                                                   //
                                                                                                   // of
                                                                                                   //
                                                                                                   // threads
      }

    outdir = (indir + 1) % ImageDim;

    // arguments set up
    int argidx = 0;
    if( indir % 2 == 0 )
      {
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle, argidx++,
                                                      otPtr->GetGPUDataManager() );
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle, argidx++,
                                                      bfPtr->GetGPUDataManager() );
      }
    else // swapping input and output
      {
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle, argidx++,
                                                      bfPtr->GetGPUDataManager() );
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle, argidx++,
                                                      otPtr->GetGPUDataManager() );
      }

    // image size
    this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle, argidx++, m_GPUImageSizes);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle, argidx++, sizeof(int), &(ImageDim) );

    // smoothing kernel
    this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDisplacementFieldGPUKernelHandle,
                                                    argidx++, GPUSmoothingKernels[indir]);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle,
                                           argidx++, sizeof(int), &(GPUSmoothingKernelSizes[indir]) );

    // indir and outdir
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle, argidx++, sizeof(int), &(indir) );
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle, argidx++, sizeof(int), &(outdir) );

    // shared memory below
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle,
                                           argidx++, sizeof(DeformationScalarType) * GPUSmoothingKernelSizes[indir],
                                           ITK_NULLPTR);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDisplacementFieldGPUKernelHandle,
                                           argidx++, sizeof(DeformationScalarType)
                                           * (localSize[indir] + GPUSmoothingKernelSizes[indir] - 1), ITK_NULLPTR);

    // launch kernel
    this->m_GPUKernelManager->LaunchKernel(m_SmoothDisplacementFieldGPUKernelHandle,
                                           (int)TDisplacementField::ImageDimension, globalSize,
                                           localSize );
    } // for smoothing direction indir

  if( ImageDim % 2 != 0 ) // swap the final result if ImageDim is odd
    {
    DisplacementFieldPointer swapPtr = DisplacementFieldType::New();
    swapPtr->Graft(otPtr);
    otPtr->Graft(bfPtr);
    bfPtr->Graft(swapPtr);
    }
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::SmoothDisplacementField()
{
  this->m_SmoothFieldTime.Start();
  this->GPUSmoothVectorField(this->GetDisplacementField(),
                             this->m_GPUSmoothingKernels,
                             this->m_SmoothingKernelSizes);
  this->m_SmoothFieldTime.Stop();
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::SmoothUpdateField()
{
  this->m_SmoothFieldTime.Start();
  this->GPUSmoothVectorField(this->GetUpdateBuffer(),
                             this->m_UpdateFieldGPUSmoothingKernels,
                             this->m_UpdateFieldSmoothingKernelSizes);
  this->m_SmoothFieldTime.Stop();
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter>
void
GPUPDEDeformableRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter>
::AllocateSmoothingBuffer()
{
  DisplacementFieldPointer field = this->GetOutput();

  // allocate smoothing buffer
  m_TempField->SetOrigin( field->GetOrigin() );
  m_TempField->SetSpacing( field->GetSpacing() );
  m_TempField->SetDirection( field->GetDirection() );
  m_TempField->SetLargestPossibleRegion(
    field->GetLargestPossibleRegion() );
  m_TempField->SetRequestedRegion(
    field->GetRequestedRegion() );
  m_TempField->SetBufferedRegion( field->GetBufferedRegion() );
  m_TempField->Allocate();

  typedef GaussianOperator<DeformationScalarType, ImageDimension> OperatorType;
  OperatorType oper;
  // Automatically create the smoothing kernels in advance.
  // Therefore, we will avoid the data copy to GPU at every
  // call to the smoothing function.
  //
  // Allocate smoothing kernel for displacement field
  for( unsigned int dir = 0; dir < ImageDimension; dir++ )
    {
    // for each smoothing direction
    oper.SetDirection(dir);
    double variance = itk::Math::sqr(this->GetStandardDeviations()[dir]);
    oper.SetVariance(variance);
    oper.SetMaximumError(this->GetMaximumError() );
    oper.SetMaximumKernelWidth(this->GetMaximumKernelWidth() );
    oper.CreateDirectional();
    int ksize = oper.GetSize()[dir];

    m_SmoothingKernelSizes[dir] = ksize;
    m_SmoothingKernels[dir] = new DeformationScalarType[ksize];
    for( int i = 0; i < ksize; i++ )
      {
      m_SmoothingKernels[dir][i] = (DeformationScalarType) (oper[i]);
      }

    // convolution kernel buffer on GPU
    m_GPUSmoothingKernels[dir] = GPUDataManager::New();
    m_GPUSmoothingKernels[dir]->SetBufferSize( sizeof(DeformationScalarType) * ksize );
    m_GPUSmoothingKernels[dir]->SetCPUBufferPointer( m_SmoothingKernels[dir] );
    m_GPUSmoothingKernels[dir]->SetBufferFlag( CL_MEM_READ_ONLY );
    m_GPUSmoothingKernels[dir]->Allocate();

    m_GPUSmoothingKernels[dir]->SetGPUDirtyFlag(true);
    }
  // Allocate smoothing kernel for update field
  for( unsigned int dir = 0; dir < ImageDimension; dir++ )
    {
    // for each smoothing direction
    oper.SetDirection(dir);
    double variance = itk::Math::sqr(this->GetUpdateFieldStandardDeviations()[dir]);
    oper.SetVariance(variance);
    oper.SetMaximumError(this->GetMaximumError() );
    oper.SetMaximumKernelWidth(this->GetMaximumKernelWidth() );
    oper.CreateDirectional();
    int ksize = oper.GetSize()[dir];

    m_UpdateFieldSmoothingKernelSizes[dir] = ksize;
    m_UpdateFieldSmoothingKernels[dir] = new DeformationScalarType[ksize];
    for( int i = 0; i < ksize; i++ )
      {
      m_UpdateFieldSmoothingKernels[dir][i] = (DeformationScalarType) (oper[i]);
      }

    // convolution kernel buffer on GPU
    m_UpdateFieldGPUSmoothingKernels[dir] = GPUDataManager::New();
    m_UpdateFieldGPUSmoothingKernels[dir]->SetBufferSize( sizeof(DeformationScalarType) * ksize );
    m_UpdateFieldGPUSmoothingKernels[dir]->SetCPUBufferPointer( m_UpdateFieldSmoothingKernels[dir] );
    m_UpdateFieldGPUSmoothingKernels[dir]->SetBufferFlag( CL_MEM_READ_ONLY );
    m_UpdateFieldGPUSmoothingKernels[dir]->Allocate();

    m_UpdateFieldGPUSmoothingKernels[dir]->SetGPUDirtyFlag(true);
    }

  // allocate image dimension array
  typename TDisplacementField::SizeType outSize = field->GetLargestPossibleRegion().GetSize();
  m_ImageSizes = new int[3];
  m_ImageSizes[0] = m_ImageSizes[1] = m_ImageSizes[2] = 1;

  int ImageDim = (int)TDisplacementField::ImageDimension;
  if( ImageDim > 3 )
    {
    itkExceptionMacro("GPUSmoothDisplacementField supports 1/2/3D images.");
    return;
    }
  for( int i = 0; i < ImageDim; i++ )
    {
    m_ImageSizes[i] = outSize[i];
    }

  m_GPUImageSizes = GPUDataManager::New();
  m_GPUImageSizes->SetBufferSize( sizeof(int) * 3 );
  m_GPUImageSizes->SetCPUBufferPointer( m_ImageSizes );
  m_GPUImageSizes->SetBufferFlag( CL_MEM_READ_ONLY );
  m_GPUImageSizes->Allocate();

  m_GPUImageSizes->SetGPUDirtyFlag(true);
}

} // end namespace itk

#endif
