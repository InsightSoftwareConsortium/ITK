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
#ifndef itkRelabelComponentImageFilter_h
#define itkRelabelComponentImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImage.h"
#include <vector>

namespace itk
{
/**
 * \class RelabelComponentImageFilter
 * \brief Relabel the components in an image such that consecutive labels are used.
 *
 * RelabelComponentImageFilter remaps the labels associated with the
 * objects in an image (as from the output of
 * ConnectedComponentImageFilter) such that the label numbers are
 * consecutive with no gaps between the label numbers used. By
 * default, the relabeling will also sort the labels based on the size
 * of the object: the largest object will have label #1, the second
 * largest will have label #2, etc. If two labels have the same size
 * their initial order is kept. The sorting by size can be disabled using
 * SetSortByObjectSize.
 *
 * Label #0 is assumed to be the background and is left unaltered by the
 * relabeling.
 *
 * RelabelComponentImageFilter is typically used on the output of the
 * ConnectedComponentImageFilter for those applications that want to
 * extract the largest object or the "k" largest objects. Any
 * particular object can be extracted from the relabeled output using
 * a BinaryThresholdImageFilter. A group of objects can be extracted
 * from the relabled output using a ThresholdImageFilter.
 *
 * Once all the objects are relabeled, the application can query the
 * number of objects and the size of each object. Object sizes are
 * returned in a vector. The size of the background is not
 * calculated. So the size of object #1 is
 * GetSizeOfObjectsInPixels()[0], the size of object #2 is
 * GetSizeOfObjectsInPixels()[1], etc.
 *
 * If user sets a minimum object size, all objects with fewer pixels
 * than the minimum will be discarded, so that the number of objects
 * reported will be only those remaining. The
 * GetOriginalNumberOfObjects method can be called to find out how
 * many objects were present before the small ones were discarded.
 *
 * RelabelComponentImageFilter can be run as an "in place" filter,
 * where it will overwrite its output. The default is run out of
 * place (or generate a separate output). "In place" operation can be
 * controlled via methods in the superclass,
 * InPlaceImageFilter::InPlaceOn() and InPlaceImageFilter::InPlaceOff().
 *
 * \sa ConnectedComponentImageFilter, BinaryThresholdImageFilter, ThresholdImageFilter
 *
 * \ingroup SingleThreaded
 * \ingroup ITKConnectedComponents
 *
 * \wiki
 * \wikiexample{ImageProcessing/RelabelComponentImageFilter,Assign contiguous labels to connected regions of an image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT RelabelComponentImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef RelabelComponentImageFilter                     Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;

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
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage                         InputImageType;
  typedef TOutputImage                        OutputImageType;
  typedef   typename TInputImage::IndexType   IndexType;
  typedef   typename TInputImage::SizeType    SizeType;
  typedef   typename TOutputImage::RegionType RegionType;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(RelabelComponentImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Type used as identifier for the different component lables. */
  typedef IdentifierType LabelType;

  /** Type used to count number of pixels in objects. */
  typedef SizeValueType ObjectSizeType;

  /** Get the number of objects in the image. This information is only
   * valid after the filter has executed. */
  itkGetConstMacro(NumberOfObjects, LabelType);

  typedef std::vector< ObjectSizeType >   ObjectSizeInPixelsContainerType;
  typedef std::vector< float >            ObjectSizeInPhysicalUnitsContainerType;

  /** Get the original number of objects in the image before small
   * objects were discarded. This information is only valid after
   * the filter has executed. If the caller has not specified a
   * minimum object size, OriginalNumberOfObjects is the same as
   * NumberOfObjects. */
  itkGetConstMacro(OriginalNumberOfObjects, LabelType);

  /** Get/Set the number of objects enumerated and described when the
   * filter is printed. */
  itkSetMacro(NumberOfObjectsToPrint, LabelType);
  itkGetConstReferenceMacro(NumberOfObjectsToPrint, LabelType);

  /** Set the minimum size in pixels for an object. All objects
   * smaller than this size will be discarded and will not appear
   * in the output label map. NumberOfObjects will count only the
   * objects whose pixel counts are greater than or equal to the
   * minimum size. Call GetOriginalNumberOfObjects to find out how
   * many objects were present in the original label map. */
  itkSetMacro(MinimumObjectSize, ObjectSizeType);

  /** Get the caller-defined minimum size of an object in pixels.
   * If the caller has not set the minimum, 0 will be returned,
   * which is to be interpreted as meaning that no minimum exists,
   * and all objects in the original label map will be passed
   * through to the output. */
  itkGetConstMacro(MinimumObjectSize, ObjectSizeType);

  /** Controls whether the object labels are sorted by size.
  * If false, initial order of labels is kept. */
  itkSetMacro(SortByObjectSize, bool);
  itkGetConstMacro(SortByObjectSize, bool);
  itkBooleanMacro(SortByObjectSize);

  /** Get the size of each object in pixels. This information is only
   * valid after the filter has executed.  Size of the background is
   * not calculated.  Size of object #1 is
   * GetSizeOfObjectsInPixels()[0]. Size of object #2 is
   * GetSizeOfObjectsInPixels()[1]. Etc. */
  const ObjectSizeInPixelsContainerType & GetSizeOfObjectsInPixels() const
    {
    // The GetConstReferenceMacro can't be used here becase this container
    // doesn't have an ostream<< operator overloaded.
    return this->m_SizeOfObjectsInPixels;
    }

  /** Get the size of each object in physical space (in units of pixel
   * size). This information is only valid after the filter has
   * executed. Size of the background is not calculated.  Size of
   * object #1 is GetSizeOfObjectsInPhysicalUnits()[0]. Size of object
   * #2 is GetSizeOfObjectsInPhysicalUnits()[1]. Etc. */
  const ObjectSizeInPhysicalUnitsContainerType & GetSizeOfObjectsInPhysicalUnits() const
    {
    // The GetConstReferenceMacro can't be used here becase this container
    // doesn't have an ostream<< operator overloaded.
    return this->m_SizeOfObjectsInPhysicalUnits;
    }

  /** Get the size of a particular object in pixels. This information is only
   * valid after the filter has executed.  Size of the background
   * (object #0) is not calculated.  */
  ObjectSizeType GetSizeOfObjectInPixels(LabelType obj) const
  {
    if ( obj > 0 && obj <= m_NumberOfObjects )
      {
      return m_SizeOfObjectsInPixels[obj - 1];
      }
    else
      {
      return 0;
      }
  }

  /** Get the size of a particular object in physical space (in units of pixel
   * size). This information is only valid after the filter has
   * executed. Size of the background (object #0) is not calculated.  */
  float GetSizeOfObjectInPhysicalUnits(LabelType obj) const
  {
    if ( obj > 0 && obj <= m_NumberOfObjects )
      {
      return m_SizeOfObjectsInPhysicalUnits[obj - 1];
      }
    else
      {
      return 0;
      }
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputPixelType > ) );
  itkConceptMacro( UnsignedLongConvertibleToInputCheck,
                   ( Concept::Convertible< LabelType, InputPixelType > ) );
  itkConceptMacro( OutputLongConvertibleToUnsignedLongCheck,
                   ( Concept::Convertible< OutputPixelType, LabelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputPixelType, OutputPixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, ImageDimension > ) );
  // End concept checking
#endif

protected:

  RelabelComponentImageFilter():
    m_NumberOfObjects(0), m_NumberOfObjectsToPrint(10),
    m_OriginalNumberOfObjects(0), m_MinimumObjectSize(0),
    m_SortByObjectSize(true)
  { this->InPlaceOff(); }
  virtual ~RelabelComponentImageFilter() ITK_OVERRIDE {}

  /**
   * Standard pipeline method.
   */
  void GenerateData() ITK_OVERRIDE;

  /** RelabelComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Standard printself method */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  struct RelabelComponentObjectType {
    LabelType m_ObjectNumber;
    ObjectSizeType m_SizeInPixels;
    float m_SizeInPhysicalUnits;
  };

  // put the function objects here for sorting in descending order
  class RelabelComponentSizeInPixelsComparator
  {
  public:
    bool operator()(const RelabelComponentObjectType & a,
                    const RelabelComponentObjectType & b)
    {
      if ( a.m_SizeInPixels > b.m_SizeInPixels )
        {
        return true;
        }
      else if ( a.m_SizeInPixels < b.m_SizeInPixels )
        {
        return false;
        }
      // size in pixels and physical units are the same, sort based on
      // original object number
      else if ( a.m_ObjectNumber < b.m_ObjectNumber )
        {
        return true;
        }
      else
        {
        return false;
        }
    }
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RelabelComponentImageFilter);

  LabelType      m_NumberOfObjects;
  LabelType      m_NumberOfObjectsToPrint;
  LabelType      m_OriginalNumberOfObjects;
  ObjectSizeType m_MinimumObjectSize;
  bool           m_SortByObjectSize;

  ObjectSizeInPixelsContainerType         m_SizeOfObjectsInPixels;
  ObjectSizeInPhysicalUnitsContainerType  m_SizeOfObjectsInPhysicalUnits;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRelabelComponentImageFilter.hxx"
#endif

#endif
