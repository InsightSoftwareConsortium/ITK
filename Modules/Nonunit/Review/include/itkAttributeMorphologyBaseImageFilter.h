/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkAttributeMorphologyBaseImageFilter_h
#define itkAttributeMorphologyBaseImageFilter_h

#include "itkImageToImageFilter.h"
#include <vector>

#define PAMI

namespace itk
{
/**
 * \class AttributeMorphologyBaseImageFilter
 * \brief Morphological opening by attributes
 *
 * This is the base class for morphology attribute
 * operations. Attribute openings remove blobs according to criteria
 * such as area. When applied to grayscale images it has the effect of
 * trimming peaks based on area while leaving the rest of the image
 * unchanged. It is possible to use attributes besides area, but no
 * others are implemented yet. This filter uses some dodgy coding
 * practices - most notably copying the image data to a linear buffer
 * to allow direct implementation of the published algorithm. It
 * should therefore be quite a good candidate to carry out tests of
 * itk iterator performance with randomish access patterns.
 *
 * This filter is implemented using the method of Wilkinson, "A
 * comparison of algorithms for Connected set openings and Closings",
 * A. Meijster and M. H. Wilkinson, PAMI, vol 24, no. 4, April 2002.
 * Attempts at implementing the method from ISMM 2000 are also
 * included, but operation appears incorrect. Check the ifdefs if you
 * are interested.
 *
 * This code was contributed in the Insight Journal paper
 *
 * "Grayscale morphological attribute operations"
 * by Beare R.
 * https://hdl.handle.net/1926/1316
 * http://www.insight-journal.org/browse/publication/203
 *
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \ingroup ITKReview
 */

template <typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction>
class ITK_TEMPLATE_EXPORT AttributeMorphologyBaseImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = AttributeMorphologyBaseImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  /**
   * Types from the Superclass
   */
  using InputImagePointer = typename Superclass::InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using IndexType = typename TInputImage::IndexType;
  using OffsetType = typename TInputImage::OffsetType;
  using SizeType = typename TInputImage::SizeType;

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  //   using IndexType = typename TInputImage::IndexType;
  //   using SizeType = typename TInputImage::SizeType;
  using RegionType = typename TOutputImage::RegionType;
  using ListType = std::list<IndexType>;
  using AttributeType = TAttribute;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(AttributeMorphologyBaseImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get the threshold value used to select the connected components.
   * The connected components greater or equal to Lambda are kept, the others
   * are removed. Lambda defaults to 0.
   */
  itkSetMacro(Lambda, AttributeType);
  itkGetConstMacro(Lambda, AttributeType);

protected:
  AttributeMorphologyBaseImageFilter()
  {
    m_FullyConnected = false;
    m_AttributeValuePerPixel = 1;
    m_Lambda = 0;
  }

  ~AttributeMorphologyBaseImageFilter() override = default;
  AttributeMorphologyBaseImageFilter(const Self &) {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Standard pipeline method.
   */
  void
  GenerateData() override;

  /** AttributeMorphologyBaseImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** AttributeMorphologyBaseImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  AttributeType m_AttributeValuePerPixel;

private:
  bool          m_FullyConnected;
  AttributeType m_Lambda;

  // some constants used several times in the code
  static constexpr OffsetValueType INACTIVE = -1;
  static constexpr OffsetValueType ACTIVE = -2;
  static constexpr OffsetValueType ROOT = -3;

  // Just used for area/volume openings at the moment
  AttributeType * m_AuxData;

  using OffsetVecType = std::vector<OffsetType>;
  // offset in the linear array.
  using OffsetDirectVecType = std::vector<OffsetValueType>;

  void
  SetupOffsetVec(OffsetDirectVecType & PosOffsets, OffsetVecType & Offsets);

  class GreyAndPos
  {
  public:
    InputPixelType  Val;
    OffsetValueType Pos;
  };

  GreyAndPos *      m_SortPixels;
  OffsetValueType * m_Parent;
#ifndef PAMI
  bool * m_Processed;
#endif
  // This is a bit ugly, but I can't see an easy way around
  InputPixelType * m_Raw;

  class ComparePixStruct
  {
  public:
    TFunction m_TFunction;
    bool
    operator()(GreyAndPos const & l, GreyAndPos const & r) const
    {
      if (m_TFunction(l.Val, r.Val))
      {
        return true;
      }
      if (l.Val == r.Val)
      {
        return (l.Pos < r.Pos);
      }
      return false;
    }
  };

#ifdef PAMI
  // version from PAMI. Note - using the AuxData array rather than the
  // parent array to store area
  void
  MakeSet(OffsetValueType x)
  {
    m_Parent[x] = ACTIVE;
    m_AuxData[x] = m_AttributeValuePerPixel;
  }

  OffsetValueType
  FindRoot(OffsetValueType x)
  {
    if (m_Parent[x] >= 0)
    {
      m_Parent[x] = FindRoot(m_Parent[x]);
      return (m_Parent[x]);
    }
    else
    {
      return (x);
    }
  }

  bool
  Criterion(OffsetValueType x, OffsetValueType y)
  {
    return ((m_Raw[x] == m_Raw[y]) || (m_AuxData[x] < m_Lambda));
  }

  void
  Union(OffsetValueType n, OffsetValueType p)
  {
    OffsetValueType r = FindRoot(n);

    if (r != p)
    {
      if (Criterion(r, p))
      {
        m_AuxData[p] += m_AuxData[r];
        m_Parent[r] = p;
      }
      else
      {
        m_AuxData[p] = m_Lambda;
      }
    }
  }

#else
  // version from ISMM paper
  void
  MakeSet(OffsetValueType x)
  {
    m_Parent[x] = ACTIVE;
    m_AuxData[x] = m_AttributeValuePerPixel;
  }

  void
  Link(OffsetValueType x, OffsetValueType y)
  {
    if ((m_Parent[y] == ACTIVE) && (m_Parent[x] == ACTIVE))
    {
      // should be a call to MergeAuxData
      m_AuxData[y] = m_AuxData[x] + m_AuxData[y];
      m_AuxData[x] = -m_AttributeValuePerPixel;
    }
    else if (m_Parent[x] == ACTIVE)
    {
      m_AuxData[x] = -m_AttributeValuePerPixel;
    }
    else
    {
      m_AuxData[y] = -m_AttributeValuePerPixel;
      m_Parent[y] = INACTIVE;
    }
    m_Parent[x] = y;
  }

  OffsetValueType
  FindRoot(OffsetValueType x)
  {
    if (m_Parent[x] >= 0)
    {
      m_Parent[x] = FindRoot(m_Parent[x]);
      return (m_Parent[x]);
    }
    else
    {
      return (x);
    }
  }

  bool
  Equiv(OffsetValueType x, OffsetValueType y)
  {
    return ((m_Raw[x] == m_Raw[y]) || (m_Parent[x] == ACTIVE));
  }

  void
  Union(OffsetValueType n, OffsetValueType p)
  {
    OffsetValueType r = FindRoot(n);

    if (r != p)
    {
      if (Equiv(r, p))
      {
        Link(r, p);
      }
      else if (m_Parent[p] == ACTIVE)
      {
        m_Parent[p] = INACTIVE;
        m_AuxData[p] = -m_AttributeValuePerPixel;
      }
    }
  }

#endif
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAttributeMorphologyBaseImageFilter.hxx"
#endif

#endif
