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

#ifndef itkRecursiveLineYvvGaussianImageFilter_hxx
#define itkRecursiveLineYvvGaussianImageFilter_hxx

#include "itkRecursiveLineYvvGaussianImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::RecursiveLineYvvGaussianImageFilter()
{
  m_Direction = 0;
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  this->InPlaceOff();
  this->DynamicMultiThreadingOff();

  m_ImageRegionSplitter = ImageRegionSplitterDirection::New();

  if (this->GetDebug())
  {
    std::cout << "-----------Line filter TYPES\n";

    if (typeid(typename TInputImage::PixelType) == typeid(double))
    {
      std::cout << "InputPixelType double\n";
    }
    if (typeid(typename TOutputImage::PixelType) == typeid(double))
    {
      std::cout << "OutputPixelType double\n";
    }

    if (typeid(ScalarRealType) == typeid(double))
    {
      std::cout << "ScalarRealType double\n";
    }

    if (typeid(RealType) == typeid(double))
    {
      std::cout << "RealType double\n";
    }
  }
}

/**
 * Set Input Image
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::SetInputImage(const TInputImage * input)
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput(0, const_cast<TInputImage *>(input));
}


template <typename TInputImage, typename TOutputImage>
const TInputImage *
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::GetInputImage(void)
{
  return dynamic_cast<const TInputImage *>((ProcessObject::GetInput(0)));
}

template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::SetUp(ScalarRealType spacing)
{
  const ScalarRealType sigmad = m_Sigma / spacing;

  // Compute q according to 16 in Young et al on Gabor filering
  ScalarRealType q = 0;

  if (sigmad >= 3.556)
  {
    q = 0.9804 * (sigmad - 3.556) + 2.5091;
  }
  else
  {
    if (sigmad < 0.5)
    {
      std::cerr << "Too low sigma value (< 0.5), computation will not be precise." << std::endl;
    }

    q = 0.0561 * sigmad * sigmad + 0.5784 * sigmad - 0.2568;
  }

  // Compute B and B1 to B3 according to Young et al 2003
  ScalarRealType m0 = 1.16680;
  ScalarRealType m1 = 1.10783;
  ScalarRealType m2 = 1.40586;
  ScalarRealType scale = (m0 + q) * (m1 * m1 + m2 * m2 + 2 * m1 * q + q * q);

  m_B1 = q * (2 * m0 * m1 + m1 * m1 + m2 * m2 + (2 * m0 + 4 * m1) * q + 3 * q * q) / scale;
  m_B2 = -q * q * (m0 + 2 * m1 + 3 * q) / scale;
  m_B3 = q * q * q / scale;

  ScalarRealType baseB = (m0 * (m1 * m1 + m2 * m2)) / scale;
  m_B = baseB * baseB;

  // M Matrix for initialization on backward pass, from Triggs and Sdika, IEEE
  // TSP
  m_MMatrix = vnl_matrix<ScalarRealType>(3, 3);

  m_MMatrix(0, 0) = -m_B3 * m_B1 + 1 - m_B3 * m_B3 - m_B2;
  m_MMatrix(0, 1) = (m_B3 + m_B1) * (m_B2 + m_B3 * m_B1);
  m_MMatrix(0, 2) = m_B3 * (m_B1 + m_B3 * m_B2);

  m_MMatrix(1, 0) = m_B1 + m_B3 * m_B2;
  m_MMatrix(1, 1) = (1 - m_B2) * (m_B2 + m_B3 * m_B1);
  m_MMatrix(1, 2) = -m_B3 * (m_B3 * m_B1 + m_B3 * m_B3 + m_B2 - 1);

  m_MMatrix(2, 0) = m_B3 * m_B1 + m_B2 + m_B1 * m_B1 - m_B2 * m_B2;
  m_MMatrix(2, 1) = m_B1 * m_B2 + m_B3 * m_B2 * m_B2 - m_B1 * m_B3 * m_B3 - m_B3 * m_B3 * m_B3 - m_B3 * m_B2 + m_B3;
  m_MMatrix(2, 2) = m_B3 * (m_B1 + m_B3 * m_B2);

  m_MMatrix /= (1 + m_B1 - m_B2 + m_B3) * (1 - m_B1 - m_B2 - m_B3) * (1 + m_B2 + (m_B1 - m_B3) * m_B3);

  if (this->GetDebug())
  {
    std::cout << "cB   " << m_B << std::endl;
    std::cout << "cB1  " << m_B1 << std::endl;
    std::cout << "cB2  " << m_B2 << std::endl;
    std::cout << "cB3  " << m_B3 << std::endl;

    for (int i = 0; i < 3; ++i)
    {
      for (int j = 0; j < 3; ++j)
      {
        std::cout << "cM(" << i << "," << j << ")  " << m_MMatrix(i, j) << std::endl;
      }
    }
  }
}


// Apply Recursive Filter
template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::FilterDataArray(RealType *       outs,
                                                                                const RealType * data,
                                                                                RealType *       scratch,
                                                                                unsigned int     ln)
{
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV1 = data[0] / (1.0 - m_B1 - m_B2 - m_B3);
  RealType       sV0 = outV1, sV1 = outV1, sV2 = outV1;

  /**
   * Recursively filter the rest
   */

  for (unsigned int i = 0; i < ln; i++)
  {
    scratch[i] = RealType(data[i] + sV0 * m_B1 + sV1 * m_B2 + sV2 * m_B3);
    sV2 = sV1;
    sV1 = sV0;
    sV0 = scratch[i];
  }
  /**
   * Store the causal result
   */

  for (unsigned int i = 0; i < ln; ++i)
  {
    outs[i] = scratch[i];
  }

  // AntiCausal direction pass

  // Handle outside values according to Triggs and Sdika
  const RealType u_p = data[ln - 1] / (1.0 - m_B1 - m_B2 - m_B3);
  const RealType v_p = u_p / (1.0 - m_B1 - m_B2 - m_B3);

  RealType Vn0 = v_p;
  RealType Vn1 = v_p;
  RealType Vn2 = v_p;
  for (unsigned int i = 0; i < 3; ++i)
  {
    Vn0 += (outs[ln - 1 - i] - u_p) * m_MMatrix(0, i);
    Vn1 += (outs[ln - 1 - i] - u_p) * m_MMatrix(1, i);
    Vn2 += (outs[ln - 1 - i] - u_p) * m_MMatrix(2, i);
  }

  // This was not in the 2006 Triggs paper but sounds quite logical since m_B is
  // not one
  Vn0 *= m_B;
  Vn1 *= m_B;
  Vn2 *= m_B;

  scratch[ln - 1] = Vn0;

  // Recursively filter the rest

  for (int i = ln - 2; i >= 0; i--)
  {
    scratch[i] = RealType(outs[i] * m_B + Vn0 * m_B1 + Vn1 * m_B2 + Vn2 * m_B3);
    Vn2 = Vn1;
    Vn1 = Vn0;
    Vn0 = scratch[i];
  }

  /**
   * Roll the antiCausal part into the output
   */
  for (unsigned int i = 0; i < ln; ++i)
  {
    outs[i] = scratch[i];
  }
}

// we need all of the image in just the "Direction" we are separated into
template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    OutputImageRegionType         outputRegion = out->GetRequestedRegion();
    const OutputImageRegionType & largestOutputRegion = out->GetLargestPossibleRegion();

    // verify sane parameter
    if (this->m_Direction >= outputRegion.GetImageDimension())
    {
      itkExceptionMacro("Direction selected for filtering is greater than ImageDimension")
    }

    // expand output region to match largest in the "Direction" dimension
    outputRegion.SetIndex(m_Direction, largestOutputRegion.GetIndex(m_Direction));
    outputRegion.SetSize(m_Direction, largestOutputRegion.GetSize(m_Direction));

    out->SetRequestedRegion(outputRegion);
  }
}

template <typename TInputImage, typename TOutputImage>
const ImageRegionSplitterBase *
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::GetImageRegionSplitter(void) const
{
  return this->m_ImageRegionSplitter;
}

template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  using RegionType = ImageRegion<TInputImage::ImageDimension>;

  typename TInputImage::ConstPointer inputImage(this->GetInputImage());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  const unsigned int imageDimension = inputImage->GetImageDimension();

  if (this->m_Direction >= imageDimension)
  {
    itkExceptionMacro("Direction selected for filtering is greater than ImageDimension");
  }

  const typename InputImageType::SpacingType & pixelSize = inputImage->GetSpacing();

  this->m_ImageRegionSplitter->SetDirection(m_Direction);
  this->SetUp(pixelSize[m_Direction]);

  RegionType region = outputImage->GetRequestedRegion();

  const unsigned int ln = region.GetSize()[this->m_Direction];

  if (ln < 4)
  {
    itkExceptionMacro(
      "The number of pixels along direction "
      << this->m_Direction
      << " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
  }
}

/**
 * Compute Recursive filter
 * line by line in one of the dimensions
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  using OutputPixelType = typename TOutputImage::PixelType;

  using InputConstIteratorType = ImageLinearConstIteratorWithIndex<TInputImage>;
  using OutputIteratorType = ImageLinearIteratorWithIndex<TOutputImage>;

  using RegionType = ImageRegion<TInputImage::ImageDimension>;

  typename TInputImage::ConstPointer inputImage(this->GetInputImage());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  RegionType region = outputRegionForThread;

  InputConstIteratorType inputIterator(inputImage, region);
  OutputIteratorType     outputIterator(outputImage, region);

  inputIterator.SetDirection(this->m_Direction);
  outputIterator.SetDirection(this->m_Direction);

  const unsigned int ln = region.GetSize()[this->m_Direction];

  if (ln < 4)
  {
    itkExceptionMacro("The number of pixels along direction "
                      << this->m_Direction << " in this work-unit image region is less than 4."
                      << " Note: TBBMultiThreader can divide small regions into really small pieces.");
  }

  RealType * inps = nullptr;
  RealType * outs = nullptr;
  RealType * scratch = nullptr;

  try
  {
    inps = new RealType[ln];
  }
  catch (std::bad_alloc &)
  {
    itkExceptionMacro("Problem allocating memory for internal computations");
  }

  try
  {
    outs = new RealType[ln];
  }
  catch (std::bad_alloc &)
  {
    delete[] inps;
    itkExceptionMacro("Problem allocating memory for internal computations");
  }

  try
  {
    scratch = new RealType[ln];
  }
  catch (std::bad_alloc &)
  {
    delete[] inps;
    delete[] outs;
    itkExceptionMacro("Problem allocating memory for internal computations");
  }

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  const unsigned int numberOfLinesToProcess =
    outputRegionForThread.GetNumberOfPixels() / outputRegionForThread.GetSize(this->m_Direction);
  ProgressReporter progress(this, threadId, numberOfLinesToProcess, 10);

  try // this try is intended to catch an eventual AbortException.
  {
    while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
    {
      unsigned int i = 0;
      while (!inputIterator.IsAtEndOfLine())
      {
        inps[i++] = inputIterator.Get();
        ++inputIterator;
      }

      this->FilterDataArray(outs, inps, scratch, ln);

      unsigned int j = 0;
      while (!outputIterator.IsAtEndOfLine())
      {
        outputIterator.Set(static_cast<OutputPixelType>(outs[j++]));
        ++outputIterator;
      }

      inputIterator.NextLine();
      outputIterator.NextLine();

      // Although the method name is CompletedPixel(),
      // this is being called after each line is processed
      progress.CompletedPixel();
    }
  }
  catch (ProcessAborted &)
  {
    // User aborted filter excecution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.
    // release locally allocated memory
    delete[] outs;
    delete[] inps;
    delete[] scratch;
    // Throw the final exception.
    ProcessAborted e(__FILE__, __LINE__);
    e.SetDescription("Process aborted.");
    e.SetLocation(ITK_LOCATION);
    throw e;
  }

  delete[] outs;
  delete[] inps;
  delete[] scratch;
}

template <typename TInputImage, typename TOutputImage>
void
RecursiveLineYvvGaussianImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}
} // end namespace itk

#endif
