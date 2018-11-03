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
#ifndef itkVariationalRegistrationElasticRegularizer_hxx
#define itkVariationalRegistrationElasticRegularizer_hxx

#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)

#  include "itkVariationalRegistrationElasticRegularizer.h"

#  include "itkImageRegionConstIterator.h"
#  include "itkImageRegionConstIteratorWithIndex.h"
#  include "itkNeighborhoodAlgorithm.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TDisplacementField>
VariationalRegistrationElasticRegularizer<TDisplacementField>::VariationalRegistrationElasticRegularizer()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_Size[i] = 0;
    m_ComplexSize[i] = 0;
    m_Spacing[i] = 1.0;
    m_ComplexOffsetTable[i] = 0;
  }
  m_TotalComplexSize = 0;
  m_TotalSize = 0;

  // Initialize regularization weights
  m_Lambda = 1.0;
  m_Mu = 1.0;

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    this->m_MatrixCos[i] = nullptr;
    this->m_MatrixSin[i] = nullptr;
    this->m_ComplexBuffer[i] = nullptr;
    this->m_PlanForward[i] = nullptr;
    this->m_PlanBackward[i] = nullptr;
  }
  this->m_InputBuffer = nullptr;
  this->m_OutputBuffer = nullptr;
}

/**
 * Generate data
 */
template <typename TDisplacementField>
void
VariationalRegistrationElasticRegularizer<TDisplacementField>::GenerateData()
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
VariationalRegistrationElasticRegularizer<TDisplacementField>::Initialize()
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
    this->m_ComplexSize[0] = static_cast<unsigned int>(size[0]) / 2 + 1;
    for (unsigned int i = 1; i < ImageDimension; ++i)
    {
      this->m_ComplexSize[i] = size[i];
    }

    // Calculate offset table for complex image
    this->m_ComplexOffsetTable[0] = 1;
    for (unsigned int j = 0; j + 1 < ImageDimension; j++)
    {
      this->m_ComplexOffsetTable[j + 1] = this->m_ComplexOffsetTable[j] * this->m_ComplexSize[j];
    }

    // Compute total number of pixels
    this->m_TotalSize = 1;
    this->m_TotalComplexSize = 1;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      this->m_TotalSize *= this->m_Size[i];
      this->m_TotalComplexSize *= this->m_ComplexSize[i];
    }

    // Reset old data
    FreeData();

    // initialize matrix and FFTW plans
    if (!InitializeElasticMatrix())
    {
      itkExceptionMacro(<< "Initializing Elastic Matrix failed!");
      return;
    }

    if (!InitializeElasticFFTPlans())
    {
      itkExceptionMacro(<< "Initializing Elastic Plans for FFT failed!");
      return;
    }
  }
}

/*
 * Reset data
 */
template <typename TDisplacementField>
void
VariationalRegistrationElasticRegularizer<TDisplacementField>::FreeData()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (this->m_MatrixCos[i] != nullptr)
      delete[] this->m_MatrixCos[i];
    if (this->m_MatrixSin[i] != nullptr)
      delete[] this->m_MatrixSin[i];

    if (this->m_PlanForward[i] != nullptr)
      FFTWProxyType::DestroyPlan(this->m_PlanForward[i]);
    if (this->m_PlanBackward[i] != nullptr)
      FFTWProxyType::DestroyPlan(this->m_PlanBackward[i]);

    if (this->m_ComplexBuffer[i] != nullptr)
      delete[] this->m_ComplexBuffer[i];
  }
  if (this->m_InputBuffer != nullptr)
    delete[] this->m_InputBuffer;
  if (this->m_OutputBuffer != nullptr)
    delete[] this->m_OutputBuffer;
}

/**
 * Initialize FFT plans
 */
template <typename TDisplacementField>
bool
VariationalRegistrationElasticRegularizer<TDisplacementField>::InitializeElasticFFTPlans()
{
  itkDebugMacro(<< "Initializing elastic plans for FFT...");

  // Get image size in reverse order for FFTW
  auto * n = new int[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    n[(ImageDimension - 1) - i] = this->m_Size[i];
  }

  // Allocate buffers
  this->m_InputBuffer = new FFTWProxyType::PixelType[this->m_TotalSize];
  this->m_OutputBuffer = new FFTWProxyType::PixelType[this->m_TotalSize];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    this->m_ComplexBuffer[i] = new typename FFTWProxyType::ComplexType[this->m_TotalComplexSize];
  }

  // Create the plans for the FFT
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    this->m_PlanForward[i] = FFTWProxyType::Plan_dft_r2c(
      ImageDimension, n, this->m_InputBuffer, this->m_ComplexBuffer[i], FFTW_MEASURE, this->GetNumberOfWorkUnits());

    this->m_PlanBackward[i] = FFTWProxyType::Plan_dft_c2r(
      ImageDimension, n, this->m_ComplexBuffer[i], this->m_OutputBuffer, FFTW_MEASURE, this->GetNumberOfWorkUnits());
  }

  // delete n
  delete[] n;

  return true;
}

/**
 * Initialize elastic matrix
 */
template <typename TDisplacementField>
bool
VariationalRegistrationElasticRegularizer<TDisplacementField>::InitializeElasticMatrix()
{
  itkDebugMacro(<< "Initializing elastic matrix for FFT...");

  // Calculate cosine and sine values for faster calculation
  double a = 0.0;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    this->m_MatrixCos[i] = new double[this->m_ComplexSize[i]];
    this->m_MatrixSin[i] = new double[this->m_ComplexSize[i]];

    for (unsigned int n = 0; n < this->m_ComplexSize[i]; n++)
    {
      a = (2.0 * itk::Math::pi * n) / static_cast<double>(this->m_Size[i]);

      this->m_MatrixCos[i][n] = 2.0 * std::cos(a);
      this->m_MatrixSin[i][n] = std::sin(a);
    }
  }

  return true;
}

/**
 * Execute regularization
 */
template <typename TDisplacementField>
void
VariationalRegistrationElasticRegularizer<TDisplacementField>::Regularize()
{
  DisplacementFieldConstPointer inputField = this->GetInput();

  if (!inputField)
  {
    itkExceptionMacro(<< "input displacement field is NULL!");
    return;
  }

  // Perform Forward FFT for input field
  itkDebugMacro(<< "Performing Forward FFT...");
  using ConstIteratorType = ImageRegionConstIterator<DisplacementFieldType>;
  ConstIteratorType inputIt(inputField, inputField->GetRequestedRegion());

  unsigned int n; // Counter for field copying
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    // Copy vector component into input buffer for FFT
    for (n = 0, inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++n, ++inputIt)
    {
      m_InputBuffer[n] = inputIt.Get()[i];
    }

    // Execute FFT for component
    FFTWProxyType::Execute(this->m_PlanForward[i]);
  }

  // Solve the LES in Fourier domain
  itkDebugMacro(<< "Solving Elastic LES...");
  this->SolveElasticLES();

  // Perform Backward FFT for the result in the complex domain
  itkDebugMacro(<< "Performing Backward FFT...");
  DisplacementFieldPointer outField = this->GetOutput();

  using IteratorType = ImageRegionIterator<DisplacementFieldType>;
  IteratorType outIt(outField, outField->GetRequestedRegion());

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    //  Execute FFT for component
    FFTWProxyType::Execute(this->m_PlanBackward[i]);

    // Copy complex buffer for component to component of field
    for (n = 0, outIt.GoToBegin(); !outIt.IsAtEnd(); ++n, ++outIt)
    {
      PixelType vec = outIt.Get();
      vec[i] = m_OutputBuffer[n] / static_cast<double>(this->m_TotalSize);
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
VariationalRegistrationElasticRegularizer<TDisplacementField>::SolveElasticLES()
{
  // Declare thread data struct and set filter
  ElasticFFTThreadStruct elasticLESStr;
  elasticLESStr.Filter = this;
  elasticLESStr.totalComplexSize = this->m_TotalComplexSize;

  // Setup MultiThreader
  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->SetSingleMethod(this->SolveElasticLESThreaderCallback, &elasticLESStr);

  // Execute MultiThreader
  this->GetMultiThreader()->SingleMethodExecute();
}

/**
 * Solve elastic LES
 */
template <typename TDisplacementField>
ITK_THREAD_RETURN_TYPE
VariationalRegistrationElasticRegularizer<TDisplacementField>::SolveElasticLESThreaderCallback(void * arg)
{
  // Get MultiThreader struct
  auto * threadStruct = (MultiThreaderBase::WorkUnitInfo *)arg;
  int    threadId = threadStruct->WorkUnitID;
  int    threadCount = threadStruct->NumberOfWorkUnits;

  // Calculate region for current thread
  auto * userStruct = (ElasticFFTThreadStruct *)threadStruct->UserData;

  // Calculate the range in the m_ComplexBuffer of the thread
  OffsetValueType threadRange = userStruct->totalComplexSize / threadCount;
  OffsetValueType from = threadId * threadRange;
  OffsetValueType to = (threadId == threadCount - 1) ? userStruct->totalComplexSize : (threadId + 1) * threadRange;

  // Solve LES for thread
  userStruct->Filter->ThreadedSolveElasticLES(from, to);

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/**
 * Solve elastic LES
 */
template <typename TDisplacementField>
void
VariationalRegistrationElasticRegularizer<TDisplacementField>::ThreadedSolveElasticLES(OffsetValueType from,
                                                                                       OffsetValueType to)
{
  // Only implemented for Imagedimension 2 and 3 - throw exception otherwise
  if (ImageDimension == 3)
  {
    // Get parameters from struct
    const double * Cosj = m_MatrixCos[0];
    const double * Cosk = m_MatrixCos[1];
    const double * Cosl = m_MatrixCos[2];
    const double * Sinj = m_MatrixSin[0];
    const double * Sink = m_MatrixSin[1];
    const double * Sinl = m_MatrixSin[2];

    const double m2lp4m = -2 * (m_Lambda + 4 * m_Mu);
    const double lp2m = m_Lambda + 2 * m_Mu;
    const double lpm = m_Lambda + m_Mu;

    double fftInX[2];
    double fftInY[2];
    double fftInZ[2];
    double detD;
    double d11, d12, d13, d22, d23, d33;
    double invD11, invD12, invD13, invD22, invD23, invD33;

    const double mu_hx2 = m_Mu / itk::Math::sqr(m_Spacing[0]);
    const double mu_hy2 = m_Mu / itk::Math::sqr(m_Spacing[1]);
    const double mu_hz2 = m_Mu / itk::Math::sqr(m_Spacing[2]);

    const double lambdaPlus2mu_hx2 = (m_Lambda + 2 * m_Mu) / itk::Math::sqr(m_Spacing[0]);
    const double lambdaPlus2mu_hy2 = (m_Lambda + 2 * m_Mu) / itk::Math::sqr(m_Spacing[1]);
    const double lambdaPlus2mu_hz2 = (m_Lambda + 2 * m_Mu) / itk::Math::sqr(m_Spacing[2]);

    const double lambdaPlusmu_hxhy = (m_Lambda + m_Mu) / (m_Spacing[0] * m_Spacing[1]);
    const double lambdaPlusmu_hxhz = (m_Lambda + m_Mu) / (m_Spacing[0] * m_Spacing[2]);
    const double lambdaPlusmu_hyhz = (m_Lambda + m_Mu) / (m_Spacing[1] * m_Spacing[2]);

    ValueType meanSquaredSpacing = 0.0;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      meanSquaredSpacing += itk::Math::sqr(m_Spacing[i]);
    }
    meanSquaredSpacing /= 3.0;

    // Iterate over each pixel in thread range
    for (OffsetValueType i = from; i < to; ++i)
    {
      // Get image index according to offset
      typename DisplacementFieldType::IndexType index = this->CalculateComplexImageIndex(i);

      // Save values of forward FFT X,Y,Z for this pixel
      // to be able to overwrite the array
      fftInX[0] = m_ComplexBuffer[0][i][0];
      fftInX[1] = m_ComplexBuffer[0][i][1];
      fftInY[0] = m_ComplexBuffer[1][i][0];
      fftInY[1] = m_ComplexBuffer[1][i][1];
      fftInZ[0] = m_ComplexBuffer[2][i][0];
      fftInZ[1] = m_ComplexBuffer[2][i][1];

      // Calculate the matrix values for current pixel position using the
      // precomputed sine and cosine values.
      if (this->GetUseImageSpacing())
      {
        // Calculate matrix M
        d11 = -2 * (lambdaPlus2mu_hx2 + mu_hy2 + mu_hz2) + lambdaPlus2mu_hx2 * Cosj[index[0]] +
              mu_hy2 * Cosk[index[1]] + mu_hz2 * Cosl[index[2]];
        d12 = -lambdaPlusmu_hxhy * Sinj[index[0]] * Sink[index[1]];
        d13 = -lambdaPlusmu_hxhz * Sinj[index[0]] * Sinl[index[2]];
        d22 = -2 * (lambdaPlus2mu_hy2 + mu_hx2 + mu_hz2) + lambdaPlus2mu_hy2 * Cosk[index[1]] +
              mu_hx2 * Cosj[index[0]] + mu_hz2 * Cosl[index[2]];
        d23 = -lambdaPlusmu_hyhz * Sink[index[1]] * Sinl[index[2]];
        d33 = -2 * (lambdaPlus2mu_hz2 + mu_hx2 + mu_hy2) + lambdaPlus2mu_hz2 * Cosl[index[2]] +
              mu_hx2 * Cosj[index[0]] + mu_hy2 * Cosk[index[1]];

        // Calculate Id - h^2 * M.
        d11 = 1 - meanSquaredSpacing * d11;
        d12 = -meanSquaredSpacing * d12;
        d13 = -meanSquaredSpacing * d13;
        d22 = 1 - meanSquaredSpacing * d22;
        d23 = -meanSquaredSpacing * d23;
        d33 = 1 - meanSquaredSpacing * d33;
      }
      else
      {
        // Calculate Id - h^2 * M.
        d11 = 1 - m2lp4m - lp2m * Cosj[index[0]] - m_Mu * (Cosk[index[1]] + Cosl[index[2]]);
        d12 = lpm * Sinj[index[0]] * Sink[index[1]];
        d13 = lpm * Sinj[index[0]] * Sinl[index[2]];
        d22 = 1 - m2lp4m - lp2m * Cosk[index[1]] - m_Mu * (Cosj[index[0]] + Cosl[index[2]]);
        d23 = lpm * Sink[index[1]] * Sinl[index[2]];
        d33 = 1 - m2lp4m - lp2m * Cosl[index[2]] - m_Mu * (Cosj[index[0]] + Cosk[index[1]]);
      }

      // Calculate determinant
      detD = d11 * d22 * d33 - d11 * d23 * d23 - d12 * d12 * d33 + d12 * d13 * d23 + d12 * d13 * d23 - d13 * d13 * d22;

      // Calculate the inverse values
      if (fabs(detD) < 1e-15)
      {
        // If determinant is (close to) zero, inverse is zero
        m_ComplexBuffer[0][i][0] = 0;
        m_ComplexBuffer[0][i][1] = 0;

        m_ComplexBuffer[1][i][0] = 0;
        m_ComplexBuffer[1][i][1] = 0;

        m_ComplexBuffer[2][i][0] = 0;
        m_ComplexBuffer[2][i][1] = 0;
      }
      else
      {
        // Calculate inverse of the 3x3 matrix
        invD11 = (d22 * d33 - d23 * d23) / detD;
        invD12 = (d13 * d23 - d12 * d33) / detD;
        invD13 = (d12 * d23 - d13 * d22) / detD;
        invD22 = (d11 * d33 - d13 * d13) / detD;
        invD23 = (d12 * d13 - d11 * d23) / detD;
        invD33 = (d11 * d22 - d12 * d12) / detD;

        // Calculate du1 = invD11.*fft3(in1) + invD12.*fft3(in2) + invD13.*fft3(in3)
        m_ComplexBuffer[0][i][0] = invD11 * fftInX[0] + invD12 * fftInY[0] + invD13 * fftInZ[0];
        m_ComplexBuffer[0][i][1] = invD11 * fftInX[1] + invD12 * fftInY[1] + invD13 * fftInZ[1];

        // Calculate du2 = invD12.*fft3(in1) + invD22.*fft3(in2) + invD23.*fft3(in3)
        m_ComplexBuffer[1][i][0] = invD12 * fftInX[0] + invD22 * fftInY[0] + invD23 * fftInZ[0];
        m_ComplexBuffer[1][i][1] = invD12 * fftInX[1] + invD22 * fftInY[1] + invD23 * fftInZ[1];

        // Calculate du3 = invD13.*fft3(in1) + invD23.*fft3(in2) + invD33.*fft3(in3)
        m_ComplexBuffer[2][i][0] = invD13 * fftInX[0] + invD23 * fftInY[0] + invD33 * fftInZ[0];
        m_ComplexBuffer[2][i][1] = invD13 * fftInX[1] + invD23 * fftInY[1] + invD33 * fftInZ[1];
      }
    }
  }
  else if (ImageDimension == 2)
  {
    // Get parameters from struct
    const double * Cosj = m_MatrixCos[0];
    const double * Cosk = m_MatrixCos[1];
    const double * Sinj = m_MatrixSin[0];
    const double * Sink = m_MatrixSin[1];

    const double m2lp3m = -2 * (m_Lambda + 3 * m_Mu);
    const double lp2m = m_Lambda + 2 * m_Mu;
    const double lpm = m_Lambda + m_Mu;

    double fftInX[2];
    double fftInY[2];
    double detD;
    double d11, d12, d22;
    double invD11, invD12, invD22;

    const double mu_hx2 = m_Mu / itk::Math::sqr(m_Spacing[0]);
    const double mu_hy2 = m_Mu / itk::Math::sqr(m_Spacing[1]);

    const double lambdaPlus2mu_hx2 = (m_Lambda + 2 * m_Mu) / itk::Math::sqr(m_Spacing[0]);
    const double lambdaPlus2mu_hy2 = (m_Lambda + 2 * m_Mu) / itk::Math::sqr(m_Spacing[1]);

    const double lambdaPlusmu_hxhy = (m_Lambda + m_Mu) / (m_Spacing[0] * m_Spacing[1]);

    ValueType meanSquaredSpacing = 0.0;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      meanSquaredSpacing += itk::Math::sqr(m_Spacing[i]);
    }
    meanSquaredSpacing /= ImageDimension;

    // Iterate over each pixel in thread range
    for (OffsetValueType i = from; i < to; ++i)
    {
      // Get image index according to offset
      typename DisplacementFieldType::IndexType index = this->CalculateComplexImageIndex(i);

      // Save values of forward FFT X,Y for this pixel
      // to be able to overwrite the array
      fftInX[0] = m_ComplexBuffer[0][i][0];
      fftInX[1] = m_ComplexBuffer[0][i][1];
      fftInY[0] = m_ComplexBuffer[1][i][0];
      fftInY[1] = m_ComplexBuffer[1][i][1];

      // Calculate the matrix values for current pixel position using the
      // precomputed sine and cosine values.
      if (this->GetUseImageSpacing())
      {
        // Calculate matrix M
        d11 = -2 * (lambdaPlus2mu_hx2 + mu_hy2) + lambdaPlus2mu_hx2 * Cosj[index[0]] + mu_hy2 * Cosk[index[1]];
        d12 = -lambdaPlusmu_hxhy * Sinj[index[0]] * Sink[index[1]];
        d22 = -2 * (lambdaPlus2mu_hy2 + mu_hx2) + lambdaPlus2mu_hy2 * Cosk[index[1]] + mu_hx2 * Cosj[index[0]];

        // Calculate Id - h^2 * M.
        d11 = 1 - meanSquaredSpacing * d11;
        d12 = -meanSquaredSpacing * d12;
        d22 = 1 - meanSquaredSpacing * d22;
      }
      else
      {
        // Calculate Id - M.
        d11 = 1 - m2lp3m - lp2m * Cosj[index[0]] - m_Mu * (Cosk[index[1]]);
        d12 = lpm * Sinj[index[0]] * Sink[index[1]];
        d22 = 1 - m2lp3m - lp2m * Cosk[index[1]] - m_Mu * (Cosj[index[0]]);
      }

      // Calculate determinant
      detD = d11 * d22 - d12 * d12;

      // Calculate the inverse values
      if (fabs(detD) < 1e-15)
      {
        // If determinant is (close to) zero, inverse is zero
        m_ComplexBuffer[0][i][0] = 0;
        m_ComplexBuffer[0][i][1] = 0;

        m_ComplexBuffer[1][i][0] = 0;
        m_ComplexBuffer[1][i][1] = 0;
      }
      else
      {
        // Calculate inverse of the 2x2 matrix
        invD11 = d22 / detD;
        invD12 = -d12 / detD;
        invD22 = d11 / detD;

        // Calculate du1 = invD11.*fft2(in1) + invD12.*fft2(in2)
        m_ComplexBuffer[0][i][0] = invD11 * fftInX[0] + invD12 * fftInY[0];
        m_ComplexBuffer[0][i][1] = invD11 * fftInX[1] + invD12 * fftInY[1];

        // Calculate du2 = invD12.*fft2(in1) + invD22.*fft2(in2)
        m_ComplexBuffer[1][i][0] = invD12 * fftInX[0] + invD22 * fftInY[0];
        m_ComplexBuffer[1][i][1] = invD12 * fftInX[1] + invD22 * fftInY[1];
      }
    }
  }
  else
  {
    itkExceptionMacro(<< "Elastic regularizer implemented only for ImageDimension = 2 or 3!");
  }
}

/*
 * Calculate the index in the complex image for a given offset.
 */
template <typename TDisplacementField>
typename VariationalRegistrationElasticRegularizer<TDisplacementField>::DisplacementFieldType::IndexType
VariationalRegistrationElasticRegularizer<TDisplacementField>::CalculateComplexImageIndex(OffsetValueType offset)
{
  typename DisplacementFieldType::IndexType index;
  typename DisplacementFieldType::IndexType requestedRegionIndex = this->GetOutput()->GetRequestedRegion().GetIndex();

  for (int j = ImageDimension - 1; j > 0; j--)
  {
    index[j] = static_cast<typename DisplacementFieldType::IndexValueType>(offset / this->m_ComplexOffsetTable[j]);
    offset -= (index[j] * this->m_ComplexOffsetTable[j]);
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
VariationalRegistrationElasticRegularizer<TDisplacementField>::PrintSelf(std::ostream & os, Indent indent) const
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
