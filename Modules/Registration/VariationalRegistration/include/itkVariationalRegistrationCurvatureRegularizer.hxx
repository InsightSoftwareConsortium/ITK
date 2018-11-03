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
#ifndef itkVariationalRegistrationCurvatureRegularizer_hxx
#define itkVariationalRegistrationCurvatureRegularizer_hxx

#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)

#  include "itkVariationalRegistrationCurvatureRegularizer.h"

#  include "itkImageRegionConstIterator.h"
#  include "itkImageRegionConstIteratorWithIndex.h"
#  include "itkNeighborhoodAlgorithm.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TDisplacementField>
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::VariationalRegistrationCurvatureRegularizer()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_Size[i] = 0;
    m_Spacing[i] = 1.0;
    m_OffsetTable[i] = 0;
  }
  m_TotalSize = 0;

  // Initialize regularization weights
  m_Alpha = 1.0;

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    this->m_DiagonalMatrix[i] = nullptr;
  }

  this->m_PlanForward = nullptr;
  this->m_PlanBackward = nullptr;
  this->m_VectorFieldComponentBuffer = nullptr;
  this->m_DCTVectorFieldComponentBuffer = nullptr;
}

/**
 * Default destructor
 */
template <typename TDisplacementField>
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::~VariationalRegistrationCurvatureRegularizer()
{
  //
  // Free old data, if already allocated
  //
  if (this->m_VectorFieldComponentBuffer != nullptr)
    delete[] this->m_VectorFieldComponentBuffer;
  if (this->m_DCTVectorFieldComponentBuffer != nullptr)
    delete[] this->m_DCTVectorFieldComponentBuffer;

  if (this->m_PlanForward != nullptr)
    FFTWProxyType::DestroyPlan(this->m_PlanForward);
  if (this->m_PlanBackward != nullptr)
    FFTWProxyType::DestroyPlan(this->m_PlanBackward);

  //
  // Free old data, if already allocated
  //
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (this->m_DiagonalMatrix[i] != nullptr)
      delete[] this->m_DiagonalMatrix[i];
  }
}

/**
 * Generate data
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::GenerateData()
{
  // Allocate the output image
  this->AllocateOutputs();

  // Initialize and allocate data
  this->Initialize();

  // Execute regularization
  this->Regularize();
}

/*
 * Initialize flags
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::Initialize()
{
  this->Superclass::Initialize();
  DisplacementFieldPointer DisplacementField = this->GetOutput();

  this->m_Spacing = DisplacementField->GetSpacing();

  typename DisplacementFieldType::SizeType size = DisplacementField->GetRequestedRegion().GetSize();

  // Only reinitialize FFT plans if size has changed since last Initialize()
  if (size != this->m_Size)
  {
    // Set new image size and complex buffer size including total sizes.
    // According to the FFTW manual, the complex buffer has the size
    // [n_0/2+1 , n_1, ..., n_d].
    this->m_Size = size;

    // Calculate offset table for complex image
    // Compute total number of pixels
    this->m_TotalSize = this->m_Size[0];
    this->m_OffsetTable[0] = 1;
    for (unsigned int j = 1; j < ImageDimension; j++)
    {
      this->m_OffsetTable[j] = this->m_OffsetTable[j - 1] * this->m_Size[j - 1];
      this->m_TotalSize *= this->m_Size[j];
    }

    // initialize matrix and FFTW plans
    if (!InitializeCurvatureDiagonalMatrix())
    {
      itkExceptionMacro(<< "Initializing Curvature Matrix failed!");
      return;
    }

    if (!InitializeCurvatureFFTPlans())
    {
      itkExceptionMacro(<< "Initializing Curvature Plans for FFT failed!");
      return;
    }
  }
}

/**
 * Initialize FFT plans
 */
template <typename TDisplacementField>
bool
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::InitializeCurvatureFFTPlans()
{
  itkDebugMacro(<< "Initializing curvature plans for FFT...");

  //
  // Free old data, if already allocated
  //
  if (this->m_VectorFieldComponentBuffer != nullptr)
    delete[] this->m_VectorFieldComponentBuffer;
  if (this->m_DCTVectorFieldComponentBuffer != nullptr)
    delete[] this->m_DCTVectorFieldComponentBuffer;

  if (this->m_PlanForward != nullptr)
    FFTWProxyType::DestroyPlan(this->m_PlanForward);
  if (this->m_PlanBackward != nullptr)
    FFTWProxyType::DestroyPlan(this->m_PlanBackward);

  // Allocate input and output buffers for DCT
  this->m_VectorFieldComponentBuffer = new FFTWProxyType::PixelType[this->m_TotalSize];
  this->m_DCTVectorFieldComponentBuffer = new FFTWProxyType::PixelType[this->m_TotalSize];

  //
  // different methods for the DCT are available in FFTW
  // (look here: http://www.fftw.org/doc/Real_002dto_002dReal-Transforms.html)
  //
  // We use the FFTW_REDFT01 and FFTW_REDFT10 transform (fast), unsure what the correct choice is
  //

  fftw_r2r_kind fftForwardKind[ImageDimension];
  fftw_r2r_kind fftBackwardKind[ImageDimension];
  int           size[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    fftForwardKind[i] = FFTW_REDFT01;
    fftBackwardKind[i] = FFTW_REDFT10;
    // Get image size in reverse order for FFTW
    size[(ImageDimension - 1) - i] = this->m_Size[i];
  }

  // Create the plan for the FFT
  // We need only one plan forward and backward because we reuse the input and output buffers
  // fftw_plan_r2r transforms are not available in FFTWProxyType, so we have to call FFTW functions
  // directly

  // first set multi-threading
  fftw_plan_with_nthreads(this->GetNumberOfWorkUnits());

  this->m_PlanForward = fftw_plan_r2r(ImageDimension,
                                      size,
                                      this->m_VectorFieldComponentBuffer,
                                      this->m_DCTVectorFieldComponentBuffer,
                                      fftForwardKind,
                                      FFTW_MEASURE | FFTW_DESTROY_INPUT);
  if (this->m_PlanForward == nullptr)
  {
    return false;
  }

  this->m_PlanBackward = fftw_plan_r2r(ImageDimension,
                                       size,
                                       this->m_DCTVectorFieldComponentBuffer,
                                       this->m_VectorFieldComponentBuffer,
                                       fftBackwardKind,
                                       FFTW_MEASURE | FFTW_DESTROY_INPUT);
  if (this->m_PlanBackward == nullptr)
  {
    return false;
  }

  return true;
}

/**
 * Initialize elastic matrix
 */
template <typename TDisplacementField>
bool
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::InitializeCurvatureDiagonalMatrix()
{
  itkDebugMacro(<< "Initializing curvature matrix for FFT...");

  //
  // Free old data, if already allocated
  //
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (this->m_DiagonalMatrix[i] != nullptr)
      delete[] this->m_DiagonalMatrix[i];
  }

  //
  // Compute diagonal matrix elements
  //
  // compute components of diagonal matrix elements
  // see method ThreadedSolveCurvatureLES for explanation

  //
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    m_DiagonalMatrix[dim] = new RealTypeFFT[m_Size[dim]];
    for (unsigned int idx = 0; idx < m_Size[dim]; ++idx)
    {
      // im not sure whether pi * ( idx + 1 ) / n is correct or pi * idx / n
      // This is implemented according to class CurvatureRegistrationFilter by
      // T. Rohlfing.

      const double a = (itk::Math::pi * (idx + 1)) / static_cast<double>(m_Size[dim]);
      //      const double a = (itk::Math::pi * idx ) / static_cast<double>(m_Size[dim]);

      m_DiagonalMatrix[dim][idx] = -2.0 + 2.0 * std::cos(a);
    }
  }

  return true;
}

/**
 * Execute regularization
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::Regularize()
{
  DisplacementFieldConstPointer inputField = this->GetInput();

  if (!inputField)
  {
    itkExceptionMacro(<< "input displacement field is NULL!");
    return;
  }

  using ConstIteratorType = ImageRegionConstIterator<DisplacementFieldType>;
  ConstIteratorType inputIt(inputField, inputField->GetLargestPossibleRegion());

  DisplacementFieldPointer outField = this->GetOutput();
  if (!outField)
  {
    itkExceptionMacro(<< "output displacement field is NULL!");
    return;
  }

  using IteratorType = ImageRegionIterator<DisplacementFieldType>;
  IteratorType outIt(outField, outField->GetRequestedRegion());

  //
  // Perform regularization for each vector component in three steps:
  //   1. Forward DCT
  //   2. Regularization in frequency space
  //   3. Backward DCT

  //
  // Compute normalization factor of DCT
  // see (http://www.fftw.org/doc/1d-Real_002deven-DFTs-_0028DCTs_0029.html)
  RealTypeFFT normalizationFactor = 1.0 / m_TotalSize;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    normalizationFactor *= 0.5;
  }

  unsigned int n;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    // Copy vector component into input buffer for FFT
    for (n = 0, inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++n, ++inputIt)
    {
      m_VectorFieldComponentBuffer[n] = inputIt.Get()[dim];
    }

    // Perform Forward FFT for input field
    itkDebugMacro(<< "Performing Forward FFT of dimension " << dim << "...");

    // Execute FFT for component
    FFTWProxyType::Execute(this->m_PlanForward);

    // Solve the LES in Fourier domain
    itkDebugMacro(<< "Solving Curvature LES in frequency space (dimension " << dim << ")...");
    this->SolveCurvatureLES(dim);

    // Perform Backward FFT for the result in the complex domain
    itkDebugMacro(<< "Performing Backward FFT of dimension " << dim << "...");
    //  Execute FFT for component
    FFTWProxyType::Execute(this->m_PlanBackward);

    // Copy buffer from inverse DCT to component of field
    for (n = 0, outIt.GoToBegin(); !outIt.IsAtEnd(); ++n, ++outIt)
    {
      PixelType vec = outIt.Get();
      vec[dim] = m_VectorFieldComponentBuffer[n] * normalizationFactor;
      outIt.Set(vec);
    }
  }

  outField->Modified();
}

/**
 * Solve elastic LES
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::SolveCurvatureLES(unsigned int currentDimension)
{
  // Declare thread data struct and set filter
  CurvatureFFTThreadStruct curvatureThreadParameters;
  curvatureThreadParameters.Filter = this;
  curvatureThreadParameters.totalSize = m_TotalSize;
  curvatureThreadParameters.currentDimension = currentDimension;

  // Setup MultiThreader
  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->SetSingleMethod(this->SolveCurvatureLESThreaderCallback, &curvatureThreadParameters);

  // Execute MultiThreader
  this->GetMultiThreader()->SingleMethodExecute();
}

/**
 * Solve elastic LES
 */
template <typename TDisplacementField>
ITK_THREAD_RETURN_TYPE
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::SolveCurvatureLESThreaderCallback(void * arg)
{
  // Get MultiThreader struct
  auto * threadStruct = (MultiThreaderBase::WorkUnitInfo *)arg;
  int    threadId = threadStruct->WorkUnitID;
  int    threadCount = threadStruct->NumberOfWorkUnits;

  // Calculate region for current thread
  auto * userStruct = (CurvatureFFTThreadStruct *)threadStruct->UserData;

  // Calculate the range in the m_ComplexBuffer of the thread
  OffsetValueType threadRange = userStruct->totalSize / threadCount;
  OffsetValueType from = threadId * threadRange;
  OffsetValueType to = (threadId == threadCount - 1) ? userStruct->totalSize : (threadId + 1) * threadRange;

  // Solve LES for thread
  userStruct->Filter->ThreadedSolveCurvatureLES(userStruct->currentDimension, from, to);

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/**
 * Solve elastic LES
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::ThreadedSolveCurvatureLES(
  unsigned int    currentDimension,
  OffsetValueType from,
  OffsetValueType to)
{

  // The following code is according to Fischer and Modersitzki. Linear Algebra and its applications 380 (2004)
  //
  // Let U be a lexicographic ordering of the vector field u, than we have to compute
  // U^{l+1}=(I + \tau\alpha A)^{-1} U^l with a A = d_{i,j}^2
  // and with
  //    in the 2D case d_{i,j}=-4 + 2cos( (i-1)/n_1 ) + 2cos( (j-1)/n_2 )
  // or
  //    in the 3D case d_{i,j,k}=-6 + 2cos( (i-1)/n_1 ) + 2cos( (j-1)/n_2 ) + 2cos( (k-1)/n_2 )
  // where (i,j,k) are the original image indizes before lexicographic ordering
  //
  //  (I + \tau\alpha A) is a diagonal matrix, so inversion can efficiently be solved.
  //

  ValueType meanSquaredSpacing = 0.0;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    meanSquaredSpacing += itk::Math::sqr(m_Spacing[i]);
  }
  meanSquaredSpacing /= ImageDimension;

  // compute weight including the spacing of this dimension
  const double weight = m_Alpha * meanSquaredSpacing / itk::Math::sqr(m_Spacing[currentDimension]);

  // Iterate over each pixel in thread range
  double                                    diagValue = 0;
  typename DisplacementFieldType::IndexType index;

  for (OffsetValueType i = from; i < to; ++i)
  {
    // Get image index according to offset
    index = this->CalculateImageIndex(i);

    // compute the d values
    diagValue = 0;
    // Compute d_{i,j,k} for the current voxel, we have pre-computed  -2 + 2cos( (i-1)/n_1 ) in
    // m_DiagonalMatrix
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      diagValue += m_DiagonalMatrix[dim][index[dim]];
    }
    // compute alpha*d^2
    diagValue *= (diagValue * weight);
    diagValue += 1.0; // add identity matrix
    // multiply with inverse of the diagonal matrix
    this->m_DCTVectorFieldComponentBuffer[i] /= diagValue;
  }
}

/*
 * Calculate the index in the complex image for a given offset.
 */
template <typename TDisplacementField>
typename VariationalRegistrationCurvatureRegularizer<TDisplacementField>::DisplacementFieldType::IndexType
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::CalculateImageIndex(OffsetValueType offset)
{
  typename DisplacementFieldType::IndexType index;
  typename DisplacementFieldType::IndexType requestedRegionIndex = this->GetOutput()->GetRequestedRegion().GetIndex();

  for (int j = ImageDimension - 1; j > 0; j--)
  {
    index[j] = static_cast<typename DisplacementFieldType::IndexValueType>(offset / this->m_OffsetTable[j]);
    offset -= (index[j] * this->m_OffsetTable[j]);
    index[j] += requestedRegionIndex[j];
  }
  index[0] = requestedRegionIndex[0] + static_cast<typename DisplacementFieldType::IndexValueType>(offset);

  return index;
}

/*
 * Print status information
 */
template <typename TDisplacementField>
void
VariationalRegistrationCurvatureRegularizer<TDisplacementField>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Size: ";
  os << m_Size << std::endl;
  os << indent << "Spacing: ";
  os << m_Spacing << std::endl;
}

} // end namespace itk

#endif

#endif
