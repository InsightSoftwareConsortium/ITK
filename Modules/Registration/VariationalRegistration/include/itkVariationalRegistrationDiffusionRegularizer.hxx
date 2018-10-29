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
#ifndef itkVariationalRegistrationDiffusionRegularizer_hxx
#define itkVariationalRegistrationDiffusionRegularizer_hxx
#include "itkVariationalRegistrationDiffusionRegularizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMultiThreaderBase.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TDisplacementField>
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::VariationalRegistrationDiffusionRegularizer()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_Size[i] = 0;
    m_Spacing[i] = 1.0;
  }

  // Initialize regularization weight alpha.
  m_Alpha = 1.0;
}

/**
 * Generate data by regularizing each component of the field independently
 */
template <typename TDisplacementField>
void
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::GenerateData()
{
  // Allocate the output image
  this->AllocateOutputs();

  // Initialize and allocate data
  this->Initialize();

  // Separately regularize each component of the vector field
  for (unsigned int component = 0; component < ImageDimension; ++component)
  {
    this->RegularizeComponent(component);
  }
}

/*
 * Initialize flags
 */
template <typename TDisplacementField>
void
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::Initialize()
{
  this->Superclass::Initialize();

  DisplacementFieldPointer DisplacementField = this->GetOutput();

  typename DisplacementFieldType::SizeType    size = DisplacementField->GetRequestedRegion().GetSize();
  typename DisplacementFieldType::SpacingType spacing = DisplacementField->GetSpacing();

  // Only reinitialize if size or spacing have changed since last Initialize()
  if (size != m_Size || spacing != m_Spacing)
  {
    m_Size = size;
    m_Spacing = spacing;

    // Allocate m_pBufferImage.
    m_BufferImage = BufferImageType::New();
    m_BufferImage->CopyInformation(DisplacementField);
    m_BufferImage->SetRequestedRegion(DisplacementField->GetRequestedRegion());
    m_BufferImage->SetBufferedRegion(DisplacementField->GetBufferedRegion());
    m_BufferImage->Allocate();

    // Initialize Matrices for AOS scheme
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      // Allocate all m_V.
      m_V[dim] = BufferImageType::New();
      m_V[dim]->CopyInformation(DisplacementField);
      m_V[dim]->SetRequestedRegion(DisplacementField->GetRequestedRegion());
      m_V[dim]->SetBufferedRegion(DisplacementField->GetBufferedRegion());
      m_V[dim]->Allocate();

      this->InitLUMatrices(&m_MatrixAlpha[dim], &m_MatrixBeta[dim], &m_MatrixGamma[dim], m_Size[dim], dim);
    }
  }
}

/**
 * Initialize the matrices for the LU decomposition
 */
template <typename TDisplacementField>
void
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::InitLUMatrices(ValueType ** alphaI,
                                                                                ValueType ** betaI,
                                                                                ValueType ** gammaI,
                                                                                int          n,
                                                                                int          dim)
{
  auto * alpha = new ValueType[n];
  auto * beta = new ValueType[n - 1];
  auto * gamma = new ValueType[n - 1];

  ValueType weight = this->GetAlpha();
  if (this->GetUseImageSpacing())
  {
    ValueType meanSquaredSpacing = NumericTraits<ValueType>::Zero;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      meanSquaredSpacing += m_Spacing[i] * m_Spacing[i];
    }
    meanSquaredSpacing /= ImageDimension;

    weight *= meanSquaredSpacing / (m_Spacing[dim] * m_Spacing[dim]);
  }

  // Initialization of tridiagonal matrix I - 3*alpha*B_l.
  // Alpha is the diagonal, beta the subdiagonal and gamma the superdiagonal
  // of the matrix. Kernel is [1 -2 1] and [-1 1] at the boundary.
  ValueType valA = 1 + 2 * ImageDimension * weight;
  ValueType valBG = 0 - ImageDimension * weight;
  for (int i = 0; i < n - 1; ++i)
  {
    *(alpha + i) = valA;
    *(beta + i) = valBG;
    *(gamma + i) = valBG;
  }

  // Set boundary conditions.
  *(alpha) = 1 + ImageDimension * weight;
  *(gamma) = 0 - ImageDimension * weight;
  *(beta + n - 2) = 0 - ImageDimension * weight;
  *(alpha + n - 1) = 1 + ImageDimension * weight;

  // LU decomposition of the tridiagonal matrix. Alpha becomes diagonal of L
  for (int i = 0; i < n - 1; ++i)
  {
    *(gamma + i) /= *(alpha + i);
    *(alpha + i + 1) -= *(gamma + i) * *(beta + i);
  }

  *alphaI = alpha;
  *betaI = beta;
  *gammaI = gamma;
}

/**
 * Regularize one component of the field using AOS
 */
template <typename TDisplacementField>
void
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::RegularizeComponent(const int component)
{
  typename TDisplacementField::ConstPointer DisplacementField = this->GetOutput();

  // ==========================================
  // Calculate image to be smoothed from vector components.
  CalcBufferThreadStruct calcBufferStr;

  // Initializing thread parameters.
  calcBufferStr.Filter = this;
  calcBufferStr.component = component;
  calcBufferStr.bPtr = m_BufferImage;

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->SetSingleMethod(this->CalcBufferCallback, &calcBufferStr);

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // ==========================================
  // Execute regularization in each direction.
  RegularizeThreadStruct regularizeStr;
  for (unsigned int direction = 0; direction < ImageDimension; ++direction)
  {
    regularizeStr.Filter = this;
    regularizeStr.direction = direction;
    regularizeStr.bPtr = m_BufferImage;
    regularizeStr.alpha = m_MatrixAlpha[direction];
    regularizeStr.beta = m_MatrixBeta[direction];
    regularizeStr.gamma = m_MatrixGamma[direction];
    regularizeStr.vPtr = m_V[direction];

    // Setup MultiThreader
    this->GetMultiThreader()->SetSingleMethod(this->RegularizeDirectionCallback, &regularizeStr);

    // Execute MultiThreader
    this->GetMultiThreader()->SingleMethodExecute();
  }

  // ==========================================
  // Write result in deformation field.
  MergeDirectionsThreadStruct mergeDirectionsStr;

  // Initializing thread parameters.
  mergeDirectionsStr.Filter = this;
  mergeDirectionsStr.vPtr = m_V;
  mergeDirectionsStr.component = component;

  // Setup MultiThreader
  this->GetMultiThreader()->SetSingleMethod(this->MergeDirectionsCallback, &mergeDirectionsStr);

  // Execute MultiThreader
  this->GetMultiThreader()->SingleMethodExecute();
}

/**
 * Callback function for threaded copying of one field component into the
 * image buffer as a preparation for AOS
 */
template <typename TDisplacementField>
ITK_THREAD_RETURN_TYPE
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::CalcBufferCallback(void * arg)
{
  // Get MultiThreader struct
  auto * threadStruct = (MultiThreaderBase::WorkUnitInfo *)arg;
  int    threadId = threadStruct->WorkUnitID;
  int    threadCount = threadStruct->NumberOfWorkUnits;

  // Get user struct
  auto * userStruct = (CalcBufferThreadStruct *)threadStruct->UserData;

  // Calculate region for current thread
  typename BufferImageType::RegionType splitRegion;
  int total = userStruct->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if (threadId < total)
  {
    // Get user data
    unsigned int                  component = userStruct->component;
    DisplacementFieldConstPointer uPtr = userStruct->Filter->GetInput();
    BufferImagePointer            bPtr = userStruct->bPtr;

    // Define iterator for current region
    ImageRegionConstIterator<DisplacementFieldType> uIt =
      ImageRegionConstIterator<DisplacementFieldType>(uPtr, splitRegion);
    ImageRegionIterator<BufferImageType> bIt = ImageRegionIterator<BufferImageType>(bPtr, splitRegion);

    // Copy value of vector component to buffer image
    for (uIt.GoToBegin(), bIt.GoToBegin(); !uIt.IsAtEnd(); ++uIt, ++bIt)
    {
      bIt.Set(uIt.Get()[component]);
    }
  }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/**
 * Callback function for threaded regularization of the buffered image
 * in a given direction.
 *
 * For efficiency reasons, this method operates directly on the image buffers.
 */
template <typename TDisplacementField>
ITK_THREAD_RETURN_TYPE
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::RegularizeDirectionCallback(void * arg)
{
  // Get MultiThreader struct
  auto * threadStruct = (MultiThreaderBase::WorkUnitInfo *)(arg);
  int    threadId = threadStruct->WorkUnitID;
  int    threadCount = threadStruct->NumberOfWorkUnits;

  // Get user struct
  auto * userStruct = (RegularizeThreadStruct *)threadStruct->UserData;

  // Split the face into sub-region for current thread
  int direction = userStruct->direction; // Direction in which we will regularize

  typename BufferImageType::RegionType splitRegion;
  int total = userStruct->Filter->SplitBoundaryFaceRegion(threadId, threadCount, direction, splitRegion);

  if (threadId < total)
  {
    // Get data from struct
    ValueType * alpha = userStruct->alpha;
    ValueType * beta = userStruct->beta;
    ValueType * gamma = userStruct->gamma;

    ValueType * fStart = userStruct->bPtr->GetBufferPointer();
    ValueType * vStart = userStruct->vPtr->GetBufferPointer();

    // Calc strides for buffer operations
    typename BufferImageType::SizeType  imageSize = userStruct->bPtr->GetLargestPossibleRegion().GetSize();
    typename BufferImageType::IndexType stride;
    stride[0] = 1;
    for (unsigned int i = 1; i < ImageDimension; ++i)
    {
      stride[i] = stride[i - 1] * imageSize[i - 1];
    }

    // Get data for row calculation
    int offset = stride[direction]; // Stride for next voxel in row
    int n = imageSize[direction];   // Number of pixels in row

    // Define iterator for current region
    ImageRegionIteratorWithIndex<BufferImageType> regionIt =
      ImageRegionIteratorWithIndex<BufferImageType>(userStruct->bPtr, splitRegion);

    // For each pixel on the current face, sample the corresponding row f and
    // solve A v = f with respect to v.
    for (regionIt.GoToBegin(); !regionIt.IsAtEnd(); ++regionIt)
    {
      // Get starting offset from current index
      int startOffset = 0;
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        startOffset += regionIt.GetIndex()[i] * stride[i];
      }

      // Get buffer pointer for current row
      ValueType * f = fStart + startOffset;
      ValueType * v = vStart + startOffset;

      // Set totalOffset to offset (first voxel in row)
      int totalOffset = offset;

      // Forward substitution (solve Lv=b).
      *v = *f;
      for (int i = 1; i < n; ++i)
      {
        *(v + totalOffset) = *(f + totalOffset) - *(v + totalOffset - offset) * *(gamma + i - 1);
        totalOffset += offset;
      }
      // Backward substitution (solve Rx=u, overwrite u with x).
      totalOffset = (n - 1) * offset;
      *(v + totalOffset) /= *(alpha + n - 1);

      for (int i = n - 2; i >= 0; --i)
      {
        totalOffset -= offset;
        *(v + totalOffset) = (*(v + totalOffset) - *(v + totalOffset + offset) * *(beta + i)) / (*(alpha + i));
      }
    }
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/**
 * Callback function for the threaded adding of the regularization in
 * each spatial direction
 */
template <typename TDisplacementField>
ITK_THREAD_RETURN_TYPE
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::MergeDirectionsCallback(void * arg)
{
  // Get MultiThreader struct
  auto * threadStruct = (MultiThreaderBase::WorkUnitInfo *)(arg);
  int    threadId = threadStruct->WorkUnitID;
  int    threadCount = threadStruct->NumberOfWorkUnits;

  // Get user struct
  auto * userStruct = (MergeDirectionsThreadStruct *)threadStruct->UserData;

  // Calculate region for current thread
  typename BufferImageType::RegionType splitRegion;
  int total = userStruct->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if (threadId < total)
  {
    // Define iterator for current region
    unsigned int             component = userStruct->component;
    BufferImagePointer *     vPtr = userStruct->vPtr;
    DisplacementFieldPointer outPtr = userStruct->Filter->GetOutput();

    ImageRegionIteratorWithIndex<DisplacementFieldType> outIt =
      ImageRegionIteratorWithIndex<DisplacementFieldType>(outPtr, splitRegion);

    // For each pixel on the current face, sample the corresponding row f and
    // solve A v = f with respect to v.
    for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
    {
      // Get current index and vector
      PixelType                                 vector = outIt.Get();
      typename DisplacementFieldType::IndexType index = outIt.GetIndex();

      // Sum up values from each buffer image v.
      ValueType sum = NumericTraits<ValueType>::Zero;
      for (unsigned int dimDirection = 0; dimDirection < ImageDimension; ++dimDirection)
      {
        sum += vPtr[dimDirection]->GetPixel(index);
      }

      // Set value to field
      vector[component] = sum / ImageDimension;
      outIt.Set(vector);
    }
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/**
 * Split the regions for multithreading. This is used instead of the standard
 * version because the split is performed differently for each direction of
 * regularization.
 */
template <typename TDisplacementField>
int
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::SplitBoundaryFaceRegion(
  int                     i,
  int                     num,
  int                     inDir,
  BufferImageRegionType & splitRegion)
{
  int                                 splitAxis;
  typename BufferImageType::IndexType splitIndex;
  typename BufferImageType::SizeType  splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = m_BufferImage->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // Restrain region to boundary face.
  splitSize[inDir] = 1;

  // Split on the outermost dimension available that doesn't equal 1
  splitAxis = this->GetOutput()->GetImageDimension() - 1;
  while (splitSize[splitAxis] == 1)
  {
    --splitAxis;
    if (splitAxis < 0)
    { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
    }
  }

  // Determine the actual number of pieces that will be generated
  typename BufferImageType::SizeType::SizeValueType range = splitSize[splitAxis];
  auto                                              valuesPerThread = Math::Ceil<int>(range / (double)num);
  int maxThreadIdUsed = Math::Ceil<int>(range / (double)valuesPerThread) - 1;

  // Split the region
  if (i < maxThreadIdUsed)
  {
    splitIndex[splitAxis] += i * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
  }
  if (i == maxThreadIdUsed)
  {
    splitIndex[splitAxis] += i * valuesPerThread;
    // Last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerThread;
  }

  // Set the split region size and index
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("  Split Piece: " << splitRegion);

  return maxThreadIdUsed + 1;
}

/*
 * Print status information
 */
template <typename TDisplacementField>
void
VariationalRegistrationDiffusionRegularizer<TDisplacementField>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha: ";
  os << m_Alpha << std::endl;
  os << indent << "Size: ";
  os << m_Size << std::endl;
  os << indent << "Spacing: ";
  os << m_Spacing << std::endl;
}

} // end namespace itk

#endif
