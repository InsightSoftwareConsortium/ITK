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
#ifndef itkRecursiveSeparableImageFilter_h
#define itkRecursiveSeparableImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkNumericTraits.h"
#include "itkImageRegionSplitterDirection.h"
#include "itkVariableLengthVector.h"

namespace itk
{
/** \class RecursiveSeparableImageFilter
 * \brief Base class for recursive convolution with a kernel.
 *
 * RecursiveSeparableImageFilter is the base class for recursive
 * filters that are applied in each dimension separately. If multi-component
 * images are specified, the filtering operation works on each component
 * independently.
 *
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 *
 * Details of the implementation are described in the technical report:
 * R. Deriche, "Recursively Implementing The Gaussian and Its Derivatives",
 * INRIA, 1993, ftp://ftp.inria.fr/INRIA/tech-reports/RR/RR-1893.ps.gz
 *
 * Further improvements of the algorithm are described in:
 * G. Farneback & C.-F. Westin, "On Implementation of Recursive Gaussian
 * Filters", so far unpublished.
 *
 * \ingroup ImageFilters
 * \ingroup ITKImageFilterBase
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT RecursiveSeparableImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RecursiveSeparableImageFilter                   Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Type macro that defines a name for this class. */
  itkTypeMacro(RecursiveSeparableImageFilter, InPlaceImageFilter);

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Real type to be used in internal computations. RealType in general is
   * templated over the pixel type. (For example for vector or tensor pixels,
   * RealType is a vector or a tensor of doubles.) ScalarRealType is a type
   * meant for scalars.
   */
  typedef typename TInputImage::PixelType                          InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType       RealType;
  typedef typename NumericTraits< InputPixelType >::ScalarRealType ScalarRealType;

  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Type of the input image */
  typedef TInputImage InputImageType;

  /** Type of the output image */
  typedef TOutputImage OutputImageType;

  /** Get the direction in which the filter is to be applied. */
  itkGetConstMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */
  itkSetMacro(Direction, unsigned int);

  /** Set Input Image. */
  void SetInputImage(const TInputImage *);

  /** Get Input Image. */
  const TInputImage * GetInputImage();

protected:
  RecursiveSeparableImageFilter();
  virtual ~RecursiveSeparableImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** GenerateData (apply) the filter. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;


  virtual const ImageRegionSplitterBase* GetImageRegionSplitter(void) const ITK_OVERRIDE;

  /** RecursiveSeparableImageFilter needs all of the input only in the
   *  "Direction" dimension. Therefore we enlarge the output's
   *  RequestedRegion to this. Then the superclass's
   *  GenerateInputRequestedRegion method will copy the output region
   *  to the input.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * Typically it can be used to approximate a Gaussian or one of its
   * derivatives. Parameter is the spacing along the dimension to
   * filter. */
  virtual void SetUp(ScalarRealType spacing) = 0;

  /** Apply the Recursive Filter to an array of data.  This method is called
   * for each line of the volume. Parameter "scratch" is a scratch
   * area used for internal computations that is the same size as the
   * parameters "outs" and "data". The scratch area must be allocated
   * outside of this routine (this avoids memory allocation and
   * deallocation in the inner loop of the overall algorithm. */
  void FilterDataArray(RealType *outs, const RealType *data, RealType *scratch,
                       SizeValueType ln);

protected:
  /** Causal coefficients that multiply the input data. */
  ScalarRealType m_N0;
  ScalarRealType m_N1;
  ScalarRealType m_N2;
  ScalarRealType m_N3;

  /** Recursive coefficients that multiply previously computed values
   * at the output. These are the same for the causal and
   * anti-causal parts of the filter. */
  ScalarRealType m_D1;
  ScalarRealType m_D2;
  ScalarRealType m_D3;
  ScalarRealType m_D4;

  /** Anti-causal coefficients that multiply the input data. */
  ScalarRealType m_M1;
  ScalarRealType m_M2;
  ScalarRealType m_M3;
  ScalarRealType m_M4;

  /** Recursive coefficients to be used at the boundaries to simulate
   * edge extension boundary conditions. */
  ScalarRealType m_BN1;
  ScalarRealType m_BN2;
  ScalarRealType m_BN3;
  ScalarRealType m_BN4;

  ScalarRealType m_BM1;
  ScalarRealType m_BM2;
  ScalarRealType m_BM3;
  ScalarRealType m_BM4;


  template <typename T1, typename T2>
  inline void MathEMAMAMAM(T1 &out,
                           const T1 &a1, const T2 &b1,
                           const T1 &a2, const T2 &b2,
                           const T1 &a3, const T2 &b3,
                           const T1 &a4, const T2 &b4 )
    {
      out = a1*b1 + a2*b2 + a3*b3 + a4*b4;
    }


  template <typename T1, typename T2>
  inline void MathEMAMAMAM(VariableLengthVector<T1> &out,
                           const VariableLengthVector<T1> &a1, const T2 &b1,
                           const VariableLengthVector<T1> &a2, const T2 &b2,
                           const VariableLengthVector<T1> &a3, const T2 &b3,
                           const VariableLengthVector<T1> &a4, const T2 &b4 )
    {
      const unsigned int sz  = a1.GetSize();
      if (sz != out.GetSize() )
        {
        out.SetSize(sz);
        }
      for ( unsigned int i = 0; i < sz; ++i)
        {
        out[i] = a1[i]*b1 + a2[i]*b2 + a3[i]*b3 + a4[i]*b4;
        }
    }

  template <typename T1, typename T2>
  inline void MathSMAMAMAM(T1 &out,
                           const T1 &a1, const T2 &b1,
                           const T1 &a2, const T2 &b2,
                           const T1 &a3, const T2 &b3,
                           const T1 &a4, const T2 &b4 )
    {
      out -= a1*b1 + a2*b2 + a3*b3 + a4*b4;
    }

  template <typename T1, typename T2>
  inline void MathSMAMAMAM(VariableLengthVector<T1> &out,
                           const VariableLengthVector<T1> &a1, const T2 &b1,
                           const VariableLengthVector<T1> &a2, const T2 &b2,
                           const VariableLengthVector<T1> &a3, const T2 &b3,
                           const VariableLengthVector<T1> &a4, const T2 &b4 )
    {
      const unsigned int sz  = a1.GetSize();
      if (sz != out.GetSize() )
        {
        out.SetSize(sz);
        }
      for ( unsigned int i = 0; i < sz; ++i)
        {
        out[i] -= a1[i]*b1 + a2[i]*b2 + a3[i]*b3 + a4[i]*b4;
        }
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RecursiveSeparableImageFilter);

  /** Direction in which the filter is to be applied
   * this should be in the range [0,ImageDimension-1]. */
  unsigned int m_Direction;

  ImageRegionSplitterDirection::Pointer m_ImageRegionSplitter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveSeparableImageFilter.hxx"
#endif

#endif
