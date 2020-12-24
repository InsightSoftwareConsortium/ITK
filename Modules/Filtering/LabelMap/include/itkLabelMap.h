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
#ifndef itkLabelMap_h
#define itkLabelMap_h

#include "itkImageBase.h"
#include "itkWeakPointer.h"
#include <map>

namespace itk
{
/**
 *\class LabelMap
 *  \brief Templated n-dimensional image to store labeled objects.
 *
 * LabelMap is an image class specialized in storing the labeled
 * images. It represent the image in a different way than itk::Image.
 * Instead of storing the content of the image in an array of pixels values,
 * it store the a collection of labeled objects, and a background
 * value.
 * This way of storing the content of the image allow an easy and efficient
 * manipulation of the objects in the image.
 *
 * The LabelMap shares a lot of methods with the itk::Image class.
 * it make it usable as input or output of the itk::ImageToImageFilter for example.
 * However the methods don't have the same complexity in the 2 classes, because
 * of the different way to store the data. GetPixel() is run in constant time
 * for example in itk::Image, but have a worst case complexity of O(L), where
 * L is the number of lines in the image (imageSize[1] * imageSize[2] for a 3D
 * image).
 *
 * To iterate over the LabelObjects in the map, use:
   \code
   for(unsigned int i = 0; i < filter->GetOutput()->GetNumberOfLabelObjects(); ++i)
     {
     FilterType::OutputImageType::LabelObjectType* labelObject =
       filter->GetOutput()->GetNthLabelObject(i);
     }
   \endcode
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageObjects
 * \ingroup LabeledImageObject
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/RemoveLabelsFromLabelMap,Remove Labels From Label Map}
 * \endsphinx
 */
template <typename TLabelObject>
class ITK_TEMPLATE_EXPORT LabelMap : public ImageBase<TLabelObject::ImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelMap);

  /** Standard class type aliases */
  using Self = LabelMap;
  using Superclass = ImageBase<TLabelObject::ImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelMap, ImageBase);

  using LabelObjectType = TLabelObject;

  using LabelObjectPointerType = typename LabelObjectType::Pointer;

  using SizeValueType = typename Superclass::SizeValueType;
  using LengthType = SizeValueType;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  static constexpr unsigned int ImageDimension = LabelObjectType::ImageDimension;

  /** Label type alias support */
  using LabelType = typename LabelObjectType::LabelType;
  using PixelType = LabelType;

  /** types used to expose labels only and label objects only */
  using LabelVectorType = std::vector<LabelType>;
  using LabelObjectVectorType = std::vector<LabelObjectPointerType>;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;

  /** Offset type alias support An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;

  /** Direction type alias support A matrix of direction cosines. */
  using DirectionType = typename Superclass::DirectionType;

  /** Region type alias support A region is used to specify a subset of an image.
   */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  using SpacingType = typename Superclass::SpacingType;

  /** Origin type alias support  The origin is the geometric coordinates
   * of the index (0,0). */
  using PointType = typename Superclass::PointType;

  /** Offset type alias (relative position between indices) */
  using OffsetValueType = typename Superclass::OffsetValueType;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  void
  Initialize() override;

  /**  */
  void
  Allocate(bool initialize = false) override;

  virtual void
  Graft(const Self * imgData);

  /**
   * Return the LabelObject with the label given in parameter.
   * This method throws an exception if the label doesn't exist in this image,
   * or if the label is the background one.
   */
  LabelObjectType *
  GetLabelObject(const LabelType & label);
  const LabelObjectType *
  GetLabelObject(const LabelType & label) const;

  /**
   * Return true is the image contains the label given in parameter and false
   * otherwise. If the label is the background one, true is also returned, so
   * this method may not be a good enough test before calling GetLabelObject().
   */
  bool
  HasLabel(const LabelType label) const;

  /**
   * Return the LabelObject with at the position given in parameter.
   * This method can be useful when the labels are not consecutives, but is quite
   * inefficient.
   * This method throws an exception if the index doesn't exist in this image.
   */
  LabelObjectType *
  GetNthLabelObject(const SizeValueType & pos);
  const LabelObjectType *
  GetNthLabelObject(const SizeValueType & pos) const;

  /**
   * Return the pixel value at a given index in the image. If the given index
   * is contained in several objects, only the smallest label of those objects
   * is returned. This method
   * has a worst case complexity of O(L) where L is the number of lines in the
   * image - use it with care.
   */
  const LabelType &
  GetPixel(const IndexType & idx) const;

  /**
   * \brief Set the pixel value at a given index in the image.
   *
   * As for itk::Image, this method ensure that the pixel at the position \c idx
   * has a unique value.
   *
   * The complexity of this method is at best O(L) where L is the number of lines
   * in the image - usit with care.
   */
  void
  SetPixel(const IndexType & idx, const LabelType & iLabel);

  /**
   * Add index \c idx to the label object whose label is \c label. If no label object
   * has the label \c label, the corresponding label object is created.
   * The worst case complexity of this method is O(L) where L is the number of
   * lines in the image. However, the execution time will be quite low if the
   * pixels are set in the image in raster mode.
   */
  void
  AddPixel(const IndexType & idx, const LabelType & label);

  /**
   * Remove index \c idx from the label object which has the label \c label.
   * If the label object gets empty, it is being removed from the container.
   */
  void
  RemovePixel(const IndexType & idx, const LabelType & label);

  /**
   * Set a full line in the image. If no label object has this label in the image,
   * a new object is created. If a label object already exist with that label, the
   * line is added to it WITHOUT any check - it means that if the label object may
   * contain several time the same pixel after have run that method.
   * This method runs in constant time.
   */
  void
  SetLine(const IndexType & idx, const LengthType & length, const LabelType & label);

  /**
   * Return the label object at a given index. This method
   * has a worst case complexity of O(L) where L is the number of lines in the
   * image - use it with care.
   */
  LabelObjectType *
  GetLabelObject(const IndexType & idx) const;

  /**
   * Add a label object to the image. If a label object already has the label,
   * it is overriden.
   */
  void
  AddLabelObject(LabelObjectType * labelObject);

  /**
   * Add a label object to the image. The label of the label object is
   * ignored, and a new label is given to the label object.
   */
  void
  PushLabelObject(LabelObjectType * labelObject);

  /**
   * Remove a label object.
   */
  void
  RemoveLabelObject(LabelObjectType * labelObject);

  /**
   * Remove a label object.
   */
  void
  RemoveLabel(const LabelType & label);

  /**
   * Remove all the labels in the image
   */
  void
  ClearLabels();

  /**
   * Return the number of label objects in the image
   */
  typename Self::SizeValueType
  GetNumberOfLabelObjects() const;

  /**
   * Return the labels of the label objects available in the label map
   */
  LabelVectorType
  GetLabels() const;

  /**
   * Return the the label objects available in the label map
   */
  LabelObjectVectorType
  GetLabelObjects() const;

  /**
   * Set/Get the background label
   */
  itkGetConstMacro(BackgroundValue, LabelType);
  itkSetMacro(BackgroundValue, LabelType);

  /**
   * Print all the objects stored in that collection - a convenient method
   * for prototyping.
   */
  void
  PrintLabelObjects(std::ostream & os) const;

  void
  PrintLabelObjects() const
  {
    this->PrintLabelObjects(std::cerr);
  }

  /**
   * Optimize the line representation of all the label objects referenced in the LabelMap
   */
  void
  Optimize();

  /**
   *\class ConstIterator
   * \brief A forward iterator over the LabelObjects of a LabelMap
   * \ingroup ITKLabelMap
   */
  class ConstIterator
  {
  public:
    ConstIterator() = default;

    ConstIterator(const Self * lm)
    {
      m_Begin = lm->m_LabelObjectContainer.begin();
      m_End = lm->m_LabelObjectContainer.end();
      m_Iterator = m_Begin;
    }

    ConstIterator(const ConstIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
    }

    ConstIterator &
    operator=(const ConstIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
      return *this;
    }

    const LabelObjectType *
    GetLabelObject() const
    {
      return m_Iterator->second;
    }

    const LabelType &
    GetLabel() const
    {
      return m_Iterator->first;
    }

    ConstIterator
    operator++(int)
    {
      ConstIterator tmp = *this;
      ++(*this);
      return tmp;
    }

    ConstIterator &
    operator++()
    {
      ++m_Iterator;
      return *this;
    }

    bool
    operator==(const ConstIterator & iter) const
    {
      return m_Iterator == iter.m_Iterator && m_Begin == iter.m_Begin && m_End == iter.m_End;
    }

    bool
    operator!=(const ConstIterator & iter) const
    {
      return !(*this == iter);
    }

    void
    GoToBegin()
    {
      m_Iterator = m_Begin;
    }

    bool
    IsAtEnd() const
    {
      return m_Iterator == m_End;
    }

  private:
    using InternalIteratorType = typename std::map<LabelType, LabelObjectPointerType>::const_iterator;
    InternalIteratorType m_Iterator;
    InternalIteratorType m_Begin;
    InternalIteratorType m_End;
  };

  /**
   *\class Iterator
   * \brief A forward iterator over the LabelObjects of a LabelMap
   * \ingroup ITKLabelMap
   */
  class Iterator
  {
  public:
    Iterator() = default;

    Iterator(Self * lm)
    {
      m_Begin = lm->m_LabelObjectContainer.begin();
      m_End = lm->m_LabelObjectContainer.end();
      m_Iterator = m_Begin;
    }

    Iterator(const Iterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
    }

    Iterator &
    operator=(const Iterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
      return *this;
    }

    LabelObjectType *
    GetLabelObject()
    {
      return m_Iterator->second;
    }

    const LabelType &
    GetLabel() const
    {
      return m_Iterator->first;
    }

    Iterator
    operator++(int)
    {
      Iterator tmp = *this;
      ++(*this);
      return tmp;
    }

    Iterator &
    operator++()
    {
      ++m_Iterator;
      return *this;
    }

    bool
    operator==(const Iterator & iter) const
    {
      return m_Iterator == iter.m_Iterator && m_Begin == iter.m_Begin && m_End == iter.m_End;
    }

    bool
    operator!=(const Iterator & iter) const
    {
      return !(*this == iter);
    }

    void
    GoToBegin()
    {
      m_Iterator = m_Begin;
    }

    bool
    IsAtEnd() const
    {
      return m_Iterator == m_End;
    }

  private:
    using InternalIteratorType = typename std::map<LabelType, LabelObjectPointerType>::iterator;
    InternalIteratorType m_Iterator;
    InternalIteratorType m_Begin;
    InternalIteratorType m_End;

    friend class LabelMap;
  };

protected:
  LabelMap();
  ~LabelMap() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  Graft(const DataObject * data) override;
  using Superclass::Graft;

private:
  /** the LabelObject container type */
  using LabelObjectContainerType = std::map<LabelType, LabelObjectPointerType>;
  using LabelObjectContainerIterator = typename LabelObjectContainerType::iterator;
  using LabelObjectContainerConstIterator = typename LabelObjectContainerType::const_iterator;

  LabelObjectContainerType m_LabelObjectContainer;
  LabelType                m_BackgroundValue;

  void
  AddPixel(const LabelObjectContainerIterator & it, const IndexType & idx, const LabelType & label);

  void
  RemovePixel(const LabelObjectContainerIterator & it, const IndexType & idx, bool iEmitModifiedEvent);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMap.hxx"
#endif

#endif
