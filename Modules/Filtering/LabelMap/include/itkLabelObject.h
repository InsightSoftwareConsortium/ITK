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
#ifndef itkLabelObject_h
#define itkLabelObject_h

#include <deque>
#include "itkLightObject.h"
#include "itkLabelObjectLine.h"
#include "itkWeakPointer.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class LabelObject
 *  \brief The base class for the representation of an labeled binary object in an image.
 *
 * LabelObject is the base class to represent a labeled object in an image.
 * It should be used associated with the LabelMap.
 *
 * LabelObject store mainly 2 things: the label of the object, and a set of lines
 * which are part of the object.
 * No attribute is available in that class, so this class can be used as a base class
 * to implement a label object with attribute, or when no attribute is needed (see the
 * reconstruction filters for an example. If a simple attribute is needed,
 * AttributeLabelObject can be used directly.
 *
 * All the subclasses of LabelObject have to reinplement the CopyAttributesFrom() and CopyAllFrom() method.
 * No need to reimplement CopyLinesFrom() since all derived class share the same type line data members.
 *
 * The pixels locations belonging to the LabelObject can be obtained using:
 * \code
 * for(unsigned int pixelId = 0; pixelId < labelObject->Size(); pixelId++)
 *   {
 *   std::cout << labelObject->GetIndex(pixelId);
 *   }
 * \endcode
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapFilter, AttributeLabelObject
 * \ingroup DataRepresentation
 * \ingroup LabeledImageObject
 * \ingroup ITKLabelMap
 */
template< typename TLabel, unsigned int VImageDimension >
class ITK_TEMPLATE_EXPORT LabelObject:public LightObject
{
public:
  /** Standard class typedefs */
  typedef LabelObject                Self;
  typedef LightObject                Superclass;
  typedef Self                       LabelObjectType;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelObject, LightObject);

  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  typedef Index< VImageDimension >           IndexType;
  typedef Offset< VImageDimension >          OffsetType;
  typedef TLabel                             LabelType;
  typedef LabelObjectLine< VImageDimension > LineType;
  typedef typename LineType::LengthType      LengthType;
  typedef unsigned int                       AttributeType;
  typedef itk::SizeValueType                 SizeValueType;

  itkStaticConstMacro(LABEL, AttributeType, 0);

  static AttributeType GetAttributeFromName(const std::string & s);

  static std::string GetNameFromAttribute(const AttributeType & a);

  /**
   * Set/Get the label associated with the object.
   */
  const LabelType & GetLabel() const;

  void SetLabel(const LabelType & label);

  /**
   * Return true if the object contain the given index and false otherwise.
   * Worst case complexity is O(L) where L is the number of lines in the object.
   */
  bool HasIndex(const IndexType & idx) const;

  /**
   * Add an index to the object. If the index is already in the object, the index can
   * be found several time in the object.
   */
  void AddIndex(const IndexType & idx);

  /**
   * Remove an index to the object. Depending on the configuration, it can either
   * reduce the size of the corresponding line, add one more line, remove the line
   * from the line container.
   */
  bool RemoveIndex(const IndexType & idx );

  /**
   * Add a new line to the object, without any check.
   */
  void AddLine(const IndexType & idx, const LengthType & length);

  /**
   * Add a new line to the object, without any check.
   */
  void AddLine(const LineType & line);

  SizeValueType GetNumberOfLines() const;

  const LineType & GetLine(SizeValueType i) const;

  LineType & GetLine(SizeValueType i);

  /**
   * Returns the number of pixels contained in the object.
   *
   * \warning To get an accurate result, you need to make sure
   * there is no duplication in the line container. One way to
   * ensure this (at a cost) is to call the Optimize method.
   */
  SizeValueType Size() const;

  /**
   * Returns true if there no line in the container (and thus no pixel in
   * the object.
   */
  bool Empty() const;

  void Clear();

  /**
   * Get the index of the ith pixel associated with the object.
   * Valid indices are from 0 to LabelObject->GetSize() - 1.
   */
  IndexType GetIndex(SizeValueType i) const;

  /** Copy the lines of another node to this one */
  template< typename TSourceLabelObject >
  void CopyLinesFrom(const TSourceLabelObject *src);

  /** Copy the label and the attributes of another node to this one */
  template< typename TSourceLabelObject >
  void CopyAttributesFrom(const TSourceLabelObject *src);

  /** Copy the lines, the label and the attributes from another node. */
  template< typename TSourceLabelObject >
  void CopyAllFrom(const TSourceLabelObject *src);

  /** Reorder the lines, merge the touching lines and ensure that no
   * pixel is covered by two lines
   */
  void Optimize();

  /** Shift the object position */
  void Shift( OffsetType offset );

  /** \class ConstLineIterator
   * \brief A forward iterator over the lines of a LabelObject
   * \ingroup ITKLabelMap
   */
  class ConstLineIterator
  {
  public:

    ConstLineIterator() {}

    ConstLineIterator(const Self *lo)
    {
      m_Begin = lo->m_LineContainer.begin();
      m_End = lo->m_LineContainer.end();
      m_Iterator = m_Begin;
    }

    ConstLineIterator(const ConstLineIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
    }

    ConstLineIterator & operator=(const ConstLineIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
      return *this;
    }

    const LineType & GetLine() const
    {
      return *m_Iterator;
    }

    ConstLineIterator operator++(int)
    {
      ConstLineIterator tmp = *this;
      ++(*this);
      return tmp;
    }

    ConstLineIterator & operator++()
    {
      ++m_Iterator;
      return *this;
    }

  bool operator==(const ConstLineIterator & iter) const
    {
    return m_Iterator == iter.m_Iterator && m_Begin == iter.m_Begin && m_End == iter.m_End;
    }

  bool operator!=(const ConstLineIterator & iter) const
    {
    return !( *this == iter );
    }

  void GoToBegin()
    {
      m_Iterator = m_Begin;
    }

    bool IsAtEnd() const
    {
      return m_Iterator == m_End;
    }

  private:
    typedef typename std::deque< LineType >            LineContainerType;
    typedef typename LineContainerType::const_iterator InternalIteratorType;
    InternalIteratorType m_Iterator;
    InternalIteratorType m_Begin;
    InternalIteratorType m_End;
  };

  /** \class ConstLineIterator
   * \brief A forward iterator over the indexes of a LabelObject
   * \ingroup ITKLabelMap
   */
  class ConstIndexIterator
  {
  public:

    ConstIndexIterator():
      m_Iterator(),
      m_Begin(),
      m_End()
        {
        m_Index.Fill(0);
        }

    ConstIndexIterator(const Self *lo)
    {
      m_Begin = lo->m_LineContainer.begin();
      m_End = lo->m_LineContainer.end();
      GoToBegin();
    }

    ConstIndexIterator(const ConstIndexIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Index = iter.m_Index;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
    }

    ConstIndexIterator & operator=(const ConstIndexIterator & iter)
    {
      m_Iterator = iter.m_Iterator;
      m_Index = iter.m_Index;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
      return *this;
    }

    const IndexType & GetIndex() const
    {
      return m_Index;
    }

    ConstIndexIterator & operator++()
    {
      m_Index[0]++;
      if( m_Index[0] >= m_Iterator->GetIndex()[0] + (OffsetValueType)m_Iterator->GetLength() )
        {
        // we've reached the end of the line - go to the next one
        ++m_Iterator;
        NextValidLine();
        }
      return *this;
    }

    ConstIndexIterator operator++(int)
    {
      ConstIndexIterator tmp = *this;
      ++(*this);
      return tmp;
    }

    bool operator==(const ConstIndexIterator & iter) const
    {
      return m_Index == iter.m_Index && m_Iterator == iter.m_Iterator && m_Begin == iter.m_Begin && m_End == iter.m_End;
    }

    bool operator!=(const ConstIndexIterator & iter) const
    {
      return !( *this == iter );
    }

    void GoToBegin()
    {
      m_Iterator = m_Begin;
      m_Index.Fill(0);
      NextValidLine();
    }

    bool IsAtEnd() const
    {
      return m_Iterator == m_End;
    }

  private:

    typedef typename std::deque< LineType >            LineContainerType;
    typedef typename LineContainerType::const_iterator InternalIteratorType;
    void NextValidLine()
    {
      // search for the next valid position
      while( m_Iterator != m_End && m_Iterator->GetLength() == 0 )
        {
        ++m_Iterator;
        }
      if( m_Iterator != m_End )
        {
        m_Index = m_Iterator->GetIndex();
        }
    }

    InternalIteratorType m_Iterator;
    InternalIteratorType m_Begin;
    InternalIteratorType m_End;
    IndexType            m_Index;
  };

protected:
  LabelObject();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelObject);

  typedef typename std::deque< LineType >    LineContainerType;

  LineContainerType m_LineContainer;
  LabelType         m_Label;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelObject.hxx"
#endif

#endif
