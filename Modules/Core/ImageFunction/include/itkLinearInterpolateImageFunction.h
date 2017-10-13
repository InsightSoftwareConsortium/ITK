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
#ifndef itkLinearInterpolateImageFunction_h
#define itkLinearInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"
#include "itkVariableLengthVector.h"

namespace itk
{
/** \class LinearInterpolateImageFunction
 * \brief Linearly interpolate an image at specified positions.
 *
 * LinearInterpolateImageFunction linearly interpolates image intensity at
 * a non-integer pixel position. This class is templated
 * over the input image type and the coordinate representation type
 * (e.g. float or double).
 *
 * This function works for N-dimensional images.
 *
 * This function works for images with scalar and vector pixel
 * types, and for images of type VectorImage.
 *
 * \sa VectorLinearInterpolateImageFunction
 *
 * \ingroup ImageFunctions ImageInterpolators
 * \ingroup ITKImageFunction
 *
 * \wiki
 * \wikiexample{ImageProcessing/LinearInterpolateImageFunction,Linearly interpolate a position in an image}
 * \endwiki
 */
template< typename TInputImage, typename TCoordRep = double >
class ITK_TEMPLATE_EXPORT LinearInterpolateImageFunction:
  public InterpolateImageFunction< TInputImage, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef LinearInterpolateImageFunction                     Self;
  typedef InterpolateImageFunction< TInputImage, TCoordRep > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LinearInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** InputPixelType typedef support. */
  typedef typename Superclass::InputPixelType InputPixelType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType      IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;
  typedef typename ContinuousIndexType::ValueType  InternalComputationType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex(const
                                                ContinuousIndexType &
                                                index) const ITK_OVERRIDE
  {
    return this->EvaluateOptimized(Dispatch< ImageDimension >(), index);
  }

protected:
  LinearInterpolateImageFunction();
  ~LinearInterpolateImageFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LinearInterpolateImageFunction);

  /** Number of neighbors used in the interpolation */
  static const unsigned long m_Neighbors;

  struct DispatchBase {};
  template< unsigned int >
  struct Dispatch: public DispatchBase {};

  inline OutputType EvaluateOptimized(const Dispatch< 0 > &,
                                      const ContinuousIndexType & ) const
  {
    return 0;
  }

  inline OutputType EvaluateOptimized(const Dispatch< 1 > &,
                                      const ContinuousIndexType & index) const
  {
    IndexType basei;
    basei[0] = Math::Floor< IndexValueType >(index[0]);
    if ( basei[0] < this->m_StartIndex[0] )
      {
      basei[0] = this->m_StartIndex[0];
      }

    const InternalComputationType & distance = index[0] - static_cast< InternalComputationType >( basei[0] );

    const TInputImage * const inputImagePtr = this->GetInputImage();
    const RealType & val0 = inputImagePtr->GetPixel(basei);
    if ( distance <= 0. )
      {
      return ( static_cast< OutputType >( val0 ) );
      }

    ++basei[0];
    if ( basei[0] > this->m_EndIndex[0] )
      {
      return ( static_cast< OutputType >( val0 ) );
      }
    const RealType & val1 = inputImagePtr->GetPixel(basei);

    return ( static_cast< OutputType >( val0 + ( val1 - val0 ) * distance ) );
  }

  inline OutputType EvaluateOptimized(const Dispatch< 2 > &,
                                      const ContinuousIndexType & index) const
  {
    IndexType basei;

    basei[0] = Math::Floor< IndexValueType >(index[0]);
    if ( basei[0] < this->m_StartIndex[0] )
      {
      basei[0] = this->m_StartIndex[0];
      }
    const InternalComputationType & distance0 = index[0] - static_cast< InternalComputationType >( basei[0] );

    basei[1] = Math::Floor< IndexValueType >(index[1]);
    if ( basei[1] < this->m_StartIndex[1] )
      {
      basei[1] = this->m_StartIndex[1];
      }
    const InternalComputationType & distance1 = index[1] - static_cast< InternalComputationType >( basei[1] );

    const TInputImage * const inputImagePtr = this->GetInputImage();
    const RealType & val00 = inputImagePtr->GetPixel(basei);
    if ( distance0 <= 0. && distance1 <= 0. )
      {
      return ( static_cast< OutputType >( val00 ) );
      }
    else if ( distance1 <= 0. ) // if they have the same "y"
      {
      ++basei[0];  // then interpolate across "x"
      if ( basei[0] > this->m_EndIndex[0] )
        {
        return ( static_cast< OutputType >( val00 ) );
        }
      const RealType & val10 = inputImagePtr->GetPixel(basei);
      return ( static_cast< OutputType >( val00 + ( val10 - val00 ) * distance0 ) );
      }
    else if ( distance0 <= 0. ) // if they have the same "x"
      {
      ++basei[1];  // then interpolate across "y"
      if ( basei[1] > this->m_EndIndex[1] )
        {
        return ( static_cast< OutputType >( val00 ) );
        }
      const RealType & val01 = inputImagePtr->GetPixel(basei);
      return ( static_cast< OutputType >( val00 + ( val01 - val00 ) * distance1 ) );
      }
    // fall-through case:
    // interpolate across "xy"
    ++basei[0];
    if ( basei[0] > this->m_EndIndex[0] ) // interpolate across "y"
      {
      --basei[0];
      ++basei[1];
      if ( basei[1] > this->m_EndIndex[1] )
        {
        return ( static_cast< OutputType >( val00 ) );
        }
      const RealType & val01 = inputImagePtr->GetPixel(basei);
      return ( static_cast< OutputType >( val00 + ( val01 - val00 ) * distance1 ) );
      }
    const RealType & val10 = inputImagePtr->GetPixel(basei);

    const RealType & valx0 = val00 + ( val10 - val00 ) * distance0;

    ++basei[1];
    if ( basei[1] > this->m_EndIndex[1] ) // interpolate across "x"
      {
      return ( static_cast< OutputType >( valx0 ) );
      }
    const RealType & val11 = inputImagePtr->GetPixel(basei);
    --basei[0];
    const RealType & val01 = inputImagePtr->GetPixel(basei);

    const RealType & valx1 = val01 + ( val11 - val01 ) * distance0;

    return ( static_cast< OutputType >( valx0 + ( valx1 - valx0 ) * distance1 ) );
  }

  inline OutputType EvaluateOptimized(const Dispatch< 3 > &,
                                      const ContinuousIndexType & index) const
  {
    IndexType basei;
    basei[0] = Math::Floor< IndexValueType >(index[0]);
    if ( basei[0] < this->m_StartIndex[0] )
      {
      basei[0] = this->m_StartIndex[0];
      }
    const InternalComputationType & distance0 = index[0] - static_cast< InternalComputationType >( basei[0] );

    basei[1] = Math::Floor< IndexValueType >(index[1]);
    if ( basei[1] < this->m_StartIndex[1] )
      {
      basei[1] = this->m_StartIndex[1];
      }
    const InternalComputationType & distance1 = index[1] - static_cast< InternalComputationType >( basei[1] );

    basei[2] = Math::Floor< IndexValueType >(index[2]);
    if ( basei[2] < this->m_StartIndex[2] )
      {
      basei[2] = this->m_StartIndex[2];
      }
    const InternalComputationType & distance2 = index[2] - static_cast< InternalComputationType >( basei[2] );

    const TInputImage * const inputImagePtr = this->GetInputImage();
    const RealType & val000 = inputImagePtr->GetPixel(basei);
    if ( distance0 <= 0. && distance1 <= 0. && distance2 <= 0. )
      {
      return ( static_cast< OutputType >( val000 ) );
      }

    if ( distance2 <= 0. )
      {
      if ( distance1 <= 0. ) // interpolate across "x"
        {
        ++basei[0];
        if ( basei[0] > this->m_EndIndex[0] )
          {
          return ( static_cast< OutputType >( val000 ) );
          }
        const RealType & val100 = inputImagePtr->GetPixel(basei);

        return static_cast< OutputType >( val000 + ( val100 - val000 ) * distance0 );
        }
      else if ( distance0 <= 0. ) // interpolate across "y"
        {
        ++basei[1];
        if ( basei[1] > this->m_EndIndex[1] )
          {
          return ( static_cast< OutputType >( val000 ) );
          }
        const RealType & val010 = inputImagePtr->GetPixel(basei);

        return static_cast< OutputType >( val000 + ( val010 - val000 ) * distance1 );
        }
      else  // interpolate across "xy"
        {
        ++basei[0];
        if ( basei[0] > this->m_EndIndex[0] ) // interpolate across "y"
          {
          --basei[0];
          ++basei[1];
          if ( basei[1] > this->m_EndIndex[1] )
            {
            return ( static_cast< OutputType >( val000 ) );
            }
          const RealType & val010 = inputImagePtr->GetPixel(basei);
          return static_cast< OutputType >( val000 + ( val010 - val000 ) * distance1 );
          }
        const RealType & val100 = inputImagePtr->GetPixel(basei);
        const RealType & valx00 = val000 + ( val100 - val000 ) * distance0;

        ++basei[1];
        if ( basei[1] > this->m_EndIndex[1] ) // interpolate across "x"
          {
          return ( static_cast< OutputType >( valx00 ) );
          }
        const RealType & val110 = inputImagePtr->GetPixel(basei);

        --basei[0];
        const RealType & val010 = inputImagePtr->GetPixel(basei);
        const RealType & valx10 = val010 + ( val110 - val010 ) * distance0;

        return static_cast< OutputType >( valx00 + ( valx10 - valx00 ) * distance1 );
        }
      }
    else
      {
      if ( distance1 <= 0. )
        {
        if ( distance0 <= 0. ) // interpolate across "z"
          {
          ++basei[2];
          if ( basei[2] > this->m_EndIndex[2] )
            {
            return ( static_cast< OutputType >( val000 ) );
            }
          const RealType & val001 = inputImagePtr->GetPixel(basei);

          return static_cast< OutputType >( val000 + ( val001 - val000 ) * distance2 );
          }
        else // interpolate across "xz"
          {
          ++basei[0];
          if ( basei[0] > this->m_EndIndex[0] ) // interpolate across "z"
            {
            --basei[0];
            ++basei[2];
            if ( basei[2] > this->m_EndIndex[2] )
              {
              return ( static_cast< OutputType >( val000 ) );
              }
            const RealType & val001 = inputImagePtr->GetPixel(basei);

            return static_cast< OutputType >( val000 + ( val001 - val000 ) * distance2 );
            }
          const RealType & val100 = inputImagePtr->GetPixel(basei);

          const RealType & valx00 = val000 + ( val100 - val000 ) * distance0;

          ++basei[2];
          if ( basei[2] > this->m_EndIndex[2] ) // interpolate across "x"
            {
            return ( static_cast< OutputType >( valx00 ) );
            }
          const RealType & val101 = inputImagePtr->GetPixel(basei);

          --basei[0];
          const RealType & val001 = inputImagePtr->GetPixel(basei);

          const RealType & valx01 = val001 + ( val101 - val001 ) * distance0;

          return static_cast< OutputType >( valx00 + ( valx01 - valx00 ) * distance2 );
          }
        }
      else if ( distance0 <= 0. ) // interpolate across "yz"
        {
        ++basei[1];
        if ( basei[1] > this->m_EndIndex[1] ) // interpolate across "z"
          {
          --basei[1];
          ++basei[2];
          if ( basei[2] > this->m_EndIndex[2] )
            {
            return ( static_cast< OutputType >( val000 ) );
            }
          const RealType & val001 = inputImagePtr->GetPixel(basei);

          return static_cast< OutputType >( val000 + ( val001 - val000 ) * distance2 );
          }
        const RealType & val010 = inputImagePtr->GetPixel(basei);

        const RealType & val0x0 = val000 + ( val010 - val000 ) * distance1;

        ++basei[2];
        if ( basei[2] > this->m_EndIndex[2] ) // interpolate across "y"
          {
          return ( static_cast< OutputType >( val0x0 ) );
          }
        const RealType & val011 = inputImagePtr->GetPixel(basei);

        --basei[1];
        const RealType & val001 = inputImagePtr->GetPixel(basei);

        const RealType & val0x1 = val001 + ( val011 - val001 ) * distance1;

        return static_cast< OutputType >( val0x0 + ( val0x1 - val0x0 ) * distance2 );
        }
      else // interpolate across "xyz"
        {
        ++basei[0];
        if ( basei[0] > this->m_EndIndex[0] ) // interpolate across "yz"
          {
          --basei[0];
          ++basei[1];
          if ( basei[1] > this->m_EndIndex[1] )  // interpolate across "z"
            {
            --basei[1];
            ++basei[2];
            if ( basei[2] > this->m_EndIndex[2] )
              {
              return ( static_cast< OutputType >( val000 ) );
              }
            const RealType & val001 = inputImagePtr->GetPixel(basei);

            return static_cast< OutputType >( val000 + ( val001 - val000 ) * distance2 );
            }
          const RealType & val010 = inputImagePtr->GetPixel(basei);
          const RealType & val0x0 = val000 + ( val010 - val000 ) * distance1;

          ++basei[2];
          if ( basei[2] > this->m_EndIndex[2] ) // interpolate across "y"
            {
            return ( static_cast< OutputType >( val0x0 ) );
            }
          const RealType & val011 = inputImagePtr->GetPixel(basei);

          --basei[1];
          const RealType & val001 = inputImagePtr->GetPixel(basei);

          const RealType & val0x1 = val001 + ( val011 - val001 ) * distance1;

          return static_cast< OutputType >( val0x0 + ( val0x1 - val0x0 ) * distance2 );
          }
        const RealType & val100 = inputImagePtr->GetPixel(basei);

        const RealType & valx00 = val000 + ( val100 - val000 ) * distance0;

        ++basei[1];
        if ( basei[1] > this->m_EndIndex[1] ) // interpolate across "xz"
          {
          --basei[1];
          ++basei[2];
          if ( basei[2] > this->m_EndIndex[2] ) // interpolate across "x"
            {
            return ( static_cast< OutputType >( valx00 ) );
            }
          const RealType & val101 = inputImagePtr->GetPixel(basei);

          --basei[0];
          const RealType & val001 = inputImagePtr->GetPixel(basei);

          const RealType & valx01 = val001 + ( val101 - val001 ) * distance0;

          return static_cast< OutputType >( valx00 + ( valx01 - valx00 ) * distance2 );
          }
        const RealType & val110 = inputImagePtr->GetPixel(basei);

        --basei[0];
        const RealType & val010 = inputImagePtr->GetPixel(basei);

        const RealType & valx10 = val010 + ( val110 - val010 ) * distance0;

        const RealType & valxx0 = valx00 + ( valx10 - valx00 ) * distance1;

        ++basei[2];
        if ( basei[2] > this->m_EndIndex[2] ) // interpolate across "xy"
          {
          return ( static_cast< OutputType >( valxx0 ) );
          }
        const RealType & val011 = inputImagePtr->GetPixel(basei);

        ++basei[0];
        const RealType & val111 = inputImagePtr->GetPixel(basei);

        --basei[1];
        const RealType & val101 = inputImagePtr->GetPixel(basei);

        --basei[0];
        const RealType & val001 = inputImagePtr->GetPixel(basei);

        const RealType & valx01 = val001 + ( val101 - val001 ) * distance0;
        const RealType & valx11 = val011 + ( val111 - val011 ) * distance0;
        const RealType & valxx1 = valx01 + ( valx11 - valx01 ) * distance1;

        return ( static_cast< OutputType >( valxx0 + ( valxx1 - valxx0 ) * distance2 ) );
        }
      }
  }

  inline OutputType EvaluateOptimized(const DispatchBase &,
                                      const ContinuousIndexType & index) const
  {
    return this->EvaluateUnoptimized(index);
  }

  /** Evaluate interpolator at image index position. */
  virtual inline OutputType EvaluateUnoptimized(
    const ContinuousIndexType & index) const;

  /** \brief A method to generically set all components to zero
   */
  template<typename RealTypeScalarRealType>
    void
    MakeZeroInitializer(const TInputImage * const inputImagePtr,
      VariableLengthVector<RealTypeScalarRealType> & tempZeros) const
      {
      // Variable length vector version to get the size of the pixel correct.
      typename TInputImage::IndexType idx;
      idx.Fill(0);
      const typename TInputImage::PixelType & tempPixel = inputImagePtr->GetPixel(idx);
      const unsigned int sizeOfVarLengthVector = tempPixel.GetSize();
      tempZeros.SetSize(sizeOfVarLengthVector);
      tempZeros.Fill(NumericTraits< RealTypeScalarRealType >::ZeroValue());
      }

  template<typename RealTypeScalarRealType>
    void
    MakeZeroInitializer(const TInputImage * const itkNotUsed( inputImagePtr ),
      RealTypeScalarRealType & tempZeros) const
      {
      // All other cases
      tempZeros = NumericTraits< RealTypeScalarRealType >::ZeroValue();
      }

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearInterpolateImageFunction.hxx"
#endif

#endif
