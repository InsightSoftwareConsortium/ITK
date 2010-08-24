/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAttributeMorphologyBaseImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright ( c ) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAttributeMorphologyBaseImageFilter_h
#define __itkAttributeMorphologyBaseImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
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
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 */

template< class TInputImage, class TOutputImage, class TAttribute, class TFunction >
class ITK_EXPORT AttributeMorphologyBaseImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef AttributeMorphologyBaseImageFilter              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Types from the Superclass
   */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;
  typedef typename TInputImage::IndexType          IndexType;
  typedef typename TInputImage::OffsetType         OffsetType;
  typedef typename TInputImage::SizeType           SizeType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
//   typedef   typename TInputImage::IndexType       IndexType;
//   typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType RegionType;
  typedef   std::list< IndexType >            ListType;
  typedef TAttribute                          AttributeType;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

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

  virtual ~AttributeMorphologyBaseImageFilter() {}
  AttributeMorphologyBaseImageFilter(const Self &) {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /**
   * Standard pipeline method.
   */
  void GenerateData();

  /** AttributeMorphologyBaseImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** AttributeMorphologyBaseImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) );

  AttributeType m_AttributeValuePerPixel;
private:

  bool          m_FullyConnected;
  AttributeType m_Lambda;

  // some constants used several times in the code
  itkStaticConstMacro(INACTIVE, long, -1);
  itkStaticConstMacro(ACTIVE, long, -2);
  itkStaticConstMacro(ROOT, long, -3);

  // Just used for area/volume openings at the moment
  AttributeType *m_AuxData;

  typedef std::vector< OffsetType > OffsetVecType;
  // offset in the linear array.
  typedef std::vector< long > OffsetDirectVecType;

  void SetupOffsetVec(OffsetDirectVecType & PosOffsets, OffsetVecType & Offsets);

  class GreyAndPos
  {
public:
    InputPixelType Val;
    long           Pos;
  };

  GreyAndPos *m_SortPixels;
  long *      m_Parent;
#ifndef PAMI
  bool *m_Processed;
#endif
  // This is a bit ugly, but I can't see an easy way around
  InputPixelType *m_Raw;

  class ComparePixStruct
  {
public:
    TFunction m_TFunction;
    bool operator()(GreyAndPos const & l, GreyAndPos const & r) const
    {
      if ( m_TFunction(l.Val, r.Val) )
        {
        return true;
        }
      if ( l.Val == r.Val )
        {
        return ( l.Pos < r.Pos );
        }
      return false;
    }
  };

#ifdef PAMI
  // version from PAMI. Note - using the AuxData array rather than the
  // parent array to store area
  void MakeSet(long x)
  {
    m_Parent[x] = ACTIVE;
    m_AuxData[x] = m_AttributeValuePerPixel;
  }

  long FindRoot(long x)
  {
    if ( m_Parent[x] >= 0 )
      {
      m_Parent[x] = FindRoot(m_Parent[x]);
      return ( m_Parent[x] );
      }
    else
      {
      return ( x );
      }
  }

  bool Criterion(long x, long y)
  {
    return ( ( m_Raw[x] == m_Raw[y] ) || ( m_AuxData[x] < m_Lambda ) );
  }

  void Union(long n, long p)
  {
    long r = FindRoot(n);

    if ( r != p )
      {
      if ( Criterion(r, p) )
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
  void MakeSet(long x)
  {
    m_Parent[x] = ACTIVE;
    m_AuxData[x] = m_AttributeValuePerPixel;
  }

  void Link(long x, long y)
  {
    if ( ( m_Parent[y] == ACTIVE ) && ( m_Parent[x] == ACTIVE ) )
      {
      // should be a call to MergeAuxData
      m_AuxData[y] = m_AuxData[x] + m_AuxData[y];
      m_AuxData[x] = -m_AttributeValuePerPixel;
      }
    else if ( m_Parent[x] == ACTIVE )
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

  long FindRoot(long x)
  {
    if ( m_Parent[x] >= 0 )
      {
      m_Parent[x] = FindRoot(m_Parent[x]);
      return ( m_Parent[x] );
      }
    else
      {
      return ( x );
      }
  }

  bool Equiv(long x, long y)
  {
    return ( ( m_Raw[x] == m_Raw[y] ) || ( m_Parent[x] == ACTIVE ) );
  }

  void Union(long n, long p)
  {
    long r = FindRoot(n);

    if ( r != p )
      {
      if ( Equiv(r, p) )
        {
        Link(r, p);
        }
      else if ( m_Parent[p] == ACTIVE )
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
#include "itkAttributeMorphologyBaseImageFilter.txx"
#endif

#endif
