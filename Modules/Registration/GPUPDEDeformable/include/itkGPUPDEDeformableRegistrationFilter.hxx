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
#ifndef __itkGPUPDEDeformableRegistrationFilter_hxx
#define __itkGPUPDEDeformableRegistrationFilter_hxx

#include "itkGPUPDEDeformableRegistrationFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkDataObject.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

#include "vnl/vnl_math.h"

//#define NOT_REORDER_GPU_MEMORY

namespace itk
{
/**
 * Default constructor
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GPUPDEDeformableRegistrationFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->SetNumberOfIterations(10);

  this->SetStandardDeviations(1.0);
  this->SetUpdateFieldStandardDeviations(1.0);

  m_TempField = DeformationFieldType::New();

  /** Build GPU Program */

  std::ostringstream defines;

  if(TFixedImage::ImageDimension > 3 || TFixedImage::ImageDimension < 1)
  {
    itkExceptionMacro("GPUDenseFiniteDifferenceImageFilter supports 1/2/3D image.");
  }

  defines << "#define DIM_" << TDeformationField::ImageDimension << "\n";

  //PixelType is a Vector
  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString( typeid ( typename TDeformationField::PixelType::ValueType ), defines );

  std::cout << "Defines: " << defines.str() << std::endl;

  const char* GPUSource = GPUPDEDeformableRegistrationFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
#ifdef NOT_REORDER_GPU_MEMORY
  m_SmoothDeformationFieldGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("SmoothingFilter");
#else
  m_SmoothDeformationFieldGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("SmoothingFilterReorder");
#endif

}

/*
 * Set the fixed image.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::SetFixedImage(
  const FixedImageType *ptr)
{
  this->ProcessObject::SetNthInput( 1, const_cast< FixedImageType * >( ptr ) );
}

/*
 * Get the fixed image.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
const typename GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::FixedImageType *
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GetFixedImage() const
{
  return dynamic_cast< const FixedImageType * >
         ( this->ProcessObject::GetInput(1) );
}

/*
 * Set the moving image.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::SetMovingImage(
  const MovingImageType *ptr)
{
  this->ProcessObject::SetNthInput( 2, const_cast< MovingImageType * >( ptr ) );
}

/*
 * Get the moving image.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
const typename GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::MovingImageType *
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GetMovingImage() const
{
  return dynamic_cast< const MovingImageType * >
         ( this->ProcessObject::GetInput(2) );
}

/*
 *
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
std::vector< SmartPointer< DataObject > >::size_type
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GetNumberOfValidRequiredInputs() const
{
  typename std::vector< SmartPointer< DataObject > >::size_type num = 0;

  if ( this->GetFixedImage() )
    {
    num++;
    }

  if ( this->GetMovingImage() )
    {
    num++;
    }

  return num;
}

/*
 * Standard PrintSelf method.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::PrintSelf(std::ostream & os, Indent indent) const
{
  GPUSuperclass::PrintSelf(os, indent);
}

/*
 * Set the function state values before each iteration
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::InitializeIteration()
{
  MovingImageConstPointer movingPtr = this->GetMovingImage();
  FixedImageConstPointer  fixedPtr = this->GetFixedImage();

  if ( !movingPtr || !fixedPtr )
    {
    itkExceptionMacro(<< "Fixed and/or moving image not set");
    }

  // update variables in the equation object
  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast< GPUPDEDeformableRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !f )
    {
    itkExceptionMacro(<< "FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
    }

  f->SetFixedImage(fixedPtr);
  f->SetMovingImage(movingPtr);

  this->GPUSuperclass::InitializeIteration();
}

/*
 * Override the default implemenation for the case when the
 * initial deformation is not set.
 * If the initial deformation is not set, the output is
 * fill with zero vectors.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::CopyInputToOutput()
{
  typename GPUSuperclass::InputImageType::ConstPointer inputPtr  = this->GetInput();

  if ( inputPtr )
    {
    this->GPUSuperclass::CopyInputToOutput();
    }
  else
    {
    typename GPUSuperclass::PixelType zeros;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      zeros[j] = 0;
      }

    typedef typename itk::GPUTraits< TDeformationField >::Type       GPUOutputImage;
    typename GPUOutputImage::Pointer output = dynamic_cast<GPUOutputImage *>(this->GetOutput());

    ImageRegionIterator< OutputImageType > out( output, output->GetRequestedRegion() );

    while ( !out.IsAtEnd() )
      {
      out.Value() =  zeros;
      ++out;
      }

    //copy the deformation output to gpu
    output->GetGPUDataManager()->SetGPUDirtyFlag(true);
    }
}

template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GenerateOutputInformation()
{
  typename DataObject::Pointer output;
//   typename OutputImageType::Pointer output;

  if ( this->GetInput(0) )
    {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->GPUSuperclass::GenerateOutputInformation();
    }
  else if ( this->GetFixedImage() )
    {
    // Initial deforamtion field is not set.
    // Copy information from the fixed image.
    for ( unsigned int idx = 0; idx <
          this->GetNumberOfOutputs(); ++idx )
      {
      output = this->GetOutput(idx);
      if ( output )
        {
        output->CopyInformation( this->GetFixedImage() );
        }
      }
    }
}

template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GenerateInputRequestedRegion()
{
  // call the GPUSuperclass's implementation
  GPUSuperclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  MovingImagePointer movingPtr =
    const_cast< MovingImageType * >( this->GetMovingImage() );
  if ( movingPtr )
    {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  DeformationFieldPointer inputPtr =
    const_cast< DeformationFieldType * >( this->GetInput() );
  DeformationFieldPointer outputPtr = this->GetOutput();
  FixedImagePointer       fixedPtr =
    const_cast< FixedImageType * >( this->GetFixedImage() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

  if ( fixedPtr )
    {
    fixedPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }
}

/*
 * Release memory of internal buffers
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::PostProcessOutput()
{
  this->GPUSuperclass::PostProcessOutput();

  // release memory
  m_TempField->Initialize();
  m_GPUSmoothingKernel->Initialize();
  delete m_SmoothingKernel;
  m_SmoothingKernel = NULL;

  m_GPUImageSizes->Initialize();
  delete m_ImageSizes;
  m_ImageSizes = NULL;

  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast< GPUPDEDeformableRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );
  f->GPUReleaseMetricData();

  // update the cpu buffer from gpu
  this->GetOutput()->GetBufferPointer();
}

/*
 * Initialize flags
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::Initialize()
{
  this->GPUSuperclass::Initialize();

  this->AllocateSmoothingBuffer();

  FixedImageConstPointer fixedImage = this->GetFixedImage();
  unsigned int numPixels = (unsigned int)
    (fixedImage->GetLargestPossibleRegion().GetNumberOfPixels());

  GPUPDEDeformableRegistrationFunctionType *f =
    dynamic_cast< GPUPDEDeformableRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );
  f->GPUAllocateMetricData(numPixels);
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::GPUSmoothDisplacementField()
{
  typedef typename DeformationFieldType::PixelType       VectorType;
  typedef typename VectorType::ValueType                 ScalarType;
  typedef GaussianOperator< ScalarType, ImageDimension > OperatorType;

  // image arguments
  typedef typename itk::GPUTraits< TDeformationField >::Type GPUBufferImage;
  typedef typename itk::GPUTraits< TDeformationField >::Type GPUOutputImage;

  typename GPUBufferImage::Pointer  bfPtr =  dynamic_cast< GPUBufferImage * >( m_TempField.GetPointer() );
  typename GPUOutputImage::Pointer  otPtr =  dynamic_cast< GPUOutputImage * >( this->GetOutput() ); //this->ProcessObject::GetOutput(0)
                                                                                                    // );
  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)TDeformationField::ImageDimension;
  if (ImageDim > 3)
    {
    itkExceptionMacro("GPUSmoothDisplacementField supports 1/2/3D images.");
    return;
    }
  for(int i=0; i<ImageDim; i++)
  {
    imgSize[i] = outSize[i];
  }

  size_t localSize[3], globalSize[3];
  size_t blockSize = OpenCLGetLocalBlockSize(ImageDim);
  int indir, outdir; // direction for smoothing and/or storage
  typedef typename TDeformationField::PixelType::ValueType ValueType;

  for ( indir = 0; indir < ImageDim; indir++ )
    {
    for (int i=0; i<3; i++)
      {
      localSize[i] = 1;
      }
    localSize[indir] = vnl_math_min(blockSize, outSize[indir]);

    for(int i=0; i<ImageDim; i++)
      {
      globalSize[i] = localSize[i]*(unsigned int)ceil((float)outSize[i]/(float)localSize[i]); // total # of threads
      }

    outdir = (indir+1) % ImageDim;

    // arguments set up
    int argidx = 0;
    if (indir % 2 == 0)
      {
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, otPtr->GetGPUDataManager());
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, bfPtr->GetGPUDataManager());
      }
    else //swapping input and output
      {
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, bfPtr->GetGPUDataManager());
      this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, otPtr->GetGPUDataManager());
      }

    //image size
    this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, m_GPUImageSizes);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(int), &(ImageDim));

    //smoothing kernel
    this->m_GPUKernelManager->SetKernelArgWithImage(m_SmoothDeformationFieldGPUKernelHandle, argidx++, m_GPUSmoothingKernel);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(int), &(m_SmoothingKernelSize));

    //indir and outdir
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(int), &(indir));
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(int), &(outdir));

    //shared memory below
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(ValueType)*m_SmoothingKernelSize, NULL);
    this->m_GPUKernelManager->SetKernelArg(m_SmoothDeformationFieldGPUKernelHandle, argidx++, sizeof(ValueType)*(blockSize+m_SmoothingKernelSize-1), NULL);

    // launch kernel
    this->m_GPUKernelManager->LaunchKernel(m_SmoothDeformationFieldGPUKernelHandle, (int)TDeformationField::ImageDimension, globalSize, localSize );
    }

  if (ImageDim % 2 != 0) //swap the final result if ImageDim is odd
    {
    DeformationFieldPointer swapPtr = DeformationFieldType::New();
    swapPtr->Graft(otPtr);
    otPtr  ->Graft(bfPtr);
    bfPtr  ->Graft(swapPtr);
    }
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::SmoothDisplacementField()
{
  this->m_SmoothFieldTime.Start();
  this->GPUSmoothDisplacementField();
  this->m_SmoothFieldTime.Stop();
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::SmoothUpdateField()
{
  // The update buffer will be overwritten with new data.
  DeformationFieldPointer field = this->GetUpdateBuffer();

  typedef typename DeformationFieldType::PixelType       VectorType;
  typedef typename VectorType::ValueType                 ScalarType;
  typedef GaussianOperator< ScalarType, ImageDimension > OperatorType;
  typedef VectorNeighborhoodOperatorImageFilter<
    DeformationFieldType,
    DeformationFieldType >                             SmootherType;

  OperatorType                   opers[ImageDimension];
  typename SmootherType::Pointer smoothers[ImageDimension];

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    // smooth along this dimension
    opers[j].SetDirection(j);
    double variance = vnl_math_sqr(this->GetUpdateFieldStandardDeviations()[j]);
    //double variance = vnl_math_sqr( 1.0 );
    opers[j].SetVariance(variance);
    opers[j].SetMaximumError( this->GetMaximumError() );
    opers[j].SetMaximumKernelWidth( this->GetMaximumKernelWidth() );
    opers[j].CreateDirectional();

    smoothers[j] = SmootherType::New();
    smoothers[j]->SetOperator(opers[j]);
    smoothers[j]->ReleaseDataFlagOn();

    if ( j > 0 )
      {
      smoothers[j]->SetInput( smoothers[j - 1]->GetOutput() );
      }
    }
  smoothers[0]->SetInput(field);
  smoothers[ImageDimension - 1]->GetOutput()
  ->SetRequestedRegion( field->GetBufferedRegion() );

  smoothers[ImageDimension - 1]->Update();

  // field to contain the final smoothed data, do the equivalent of a graft
  field->SetPixelContainer( smoothers[ImageDimension - 1]->GetOutput()
                            ->GetPixelContainer() );
  field->SetRequestedRegion( smoothers[ImageDimension - 1]->GetOutput()
                             ->GetRequestedRegion() );
  field->SetBufferedRegion( smoothers[ImageDimension - 1]->GetOutput()
                            ->GetBufferedRegion() );
  field->SetLargestPossibleRegion( smoothers[ImageDimension - 1]->GetOutput()
                                   ->GetLargestPossibleRegion() );
  field->CopyInformation( smoothers[ImageDimension - 1]->GetOutput() );
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< class TFixedImage, class TMovingImage, class TDeformationField, class TParentImageFilter >
void
GPUPDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDeformationField, TParentImageFilter >
::AllocateSmoothingBuffer()
{
  DeformationFieldPointer field = this->GetOutput();

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

  typedef typename TDeformationField::PixelType::ValueType ValueType;

  // allocate smoothing kernel
  ValueType coefficients[5] = {0.050882235450215044,
    0.21183832469709751, 0.47455887970537486,
    0.21183832469709751, 0.050882235450215044};

  m_SmoothingKernelSize = 5;
  m_SmoothingKernel     = new ValueType[m_SmoothingKernelSize];

  // kernel may be changed later
  for (int i=0; i<m_SmoothingKernelSize; i++)
    {
    m_SmoothingKernel[i] = (ValueType) coefficients[i];
    }

  // convolution kernel for GPU
  m_GPUSmoothingKernel = GPUDataManager::New();
  m_GPUSmoothingKernel->SetBufferSize( sizeof(ValueType)*m_SmoothingKernelSize );
  m_GPUSmoothingKernel->SetCPUBufferPointer( m_SmoothingKernel );
  m_GPUSmoothingKernel->SetBufferFlag( CL_MEM_READ_ONLY );
  m_GPUSmoothingKernel->Allocate();

  m_GPUSmoothingKernel->SetGPUDirtyFlag(true);

  // allocate image dimension array
  typename TDeformationField::SizeType outSize = field->GetLargestPossibleRegion().GetSize();
  m_ImageSizes = new int[3];
  m_ImageSizes[0] = m_ImageSizes[1] = m_ImageSizes[2] = 1;

  int ImageDim = (int)TDeformationField::ImageDimension;
  if (ImageDim > 3)
    {
    itkExceptionMacro("GPUSmoothDeformationField supports 1/2/3D images.");
    return;
    }
  for(int i=0; i<ImageDim; i++)
  {
    m_ImageSizes[i] = outSize[i];
  }

  m_GPUImageSizes = GPUDataManager::New();
  m_GPUImageSizes->SetBufferSize( sizeof(int)*3 );
  m_GPUImageSizes->SetCPUBufferPointer( m_ImageSizes );
  m_GPUImageSizes->SetBufferFlag( CL_MEM_READ_ONLY );
  m_GPUImageSizes->Allocate();

  m_GPUImageSizes->SetGPUDirtyFlag(true);
}

} // end namespace itk

#endif
