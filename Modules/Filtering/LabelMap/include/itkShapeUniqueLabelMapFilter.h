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
#ifndef itkShapeUniqueLabelMapFilter_h
#define itkShapeUniqueLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkProgressReporter.h"
#include <queue>
#include "itkMath.h"

namespace itk
{
/** \class ShapeUniqueLabelMapFilter
 * \brief Remove some pixels in the label object according to the value of their shape attribute to ensure that a pixel is not in to objects
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ShapeUniqueLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ShapeUniqueLabelMapFilter       Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;
  typedef typename LabelObjectType::LineType  LineType;

  typedef typename LabelObjectType::AttributeType AttributeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShapeUniqueLabelMapFilter,
               InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
// End concept checking
#endif

  /**
   * Set/Get the ordering of the objects. By default, the objects with
   * a the largest attribute value are kept. If set to true, the filter
   * to keeps the object with the smallest attribute instead.
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to remove. The default
   * is "Size".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void SetAttribute(const std::string & s)
  {
    this->SetAttribute( LabelObjectType::GetAttributeFromName(s) );
  }

protected:
  ShapeUniqueLabelMapFilter();
  ~ShapeUniqueLabelMapFilter() ITK_OVERRIDE {}

  virtual void GenerateData() ITK_OVERRIDE;

  template< typename TAttributeAccessor >
  void TemplatedGenerateData(const TAttributeAccessor & accessor)
  {
    // Allocate the output
    this->AllocateOutputs();

    // the priority queue to store all the lines of all the objects sorted
    typedef typename std::priority_queue< LineOfLabelObject, std::vector< LineOfLabelObject >,
                                          LineOfLabelObjectComparator > PriorityQueueType;
    PriorityQueueType priorityQueue;

    ProgressReporter progress(this, 0, 1);
    // TODO: really report the progress

    for ( typename ImageType::Iterator it( this->GetLabelMap() );
          ! it.IsAtEnd();
          ++it )
      {
      LabelObjectType *labelObject = it.GetLabelObject();

      // may reduce the number of lines to proceed
      labelObject->Optimize();

      typename LabelObjectType::ConstLineIterator lit( labelObject );
      while( ! lit.IsAtEnd() )
        {
        priorityQueue.push( LineOfLabelObject(lit.GetLine(), labelObject) );
        ++lit;
        }

      // clear the lines to read them later
      labelObject->Clear();

      // go to the next label
      // progress.CompletedPixel();
      }

    if ( priorityQueue.empty() )
      {
      // nothing to do
      return;
      }

    typedef typename std::deque< LineOfLabelObject > LinesType;
    LinesType lines;

    lines.push_back( priorityQueue.top() );
    LineOfLabelObject prev = lines.back();
    IndexType         prevIdx = prev.line.GetIndex();
    priorityQueue.pop();

    while ( !priorityQueue.empty() )
      {
      LineOfLabelObject l = priorityQueue.top();
      IndexType         idx = l.line.GetIndex();
      priorityQueue.pop();

      bool newMainLine = false;
      // don't check dim 0!
      for ( unsigned int i = 1; i < ImageDimension; i++ )
        {
        if ( idx[i] != prevIdx[i] )
          {
          newMainLine = true;
          }
        }

      if ( newMainLine )
        {
        // just push the line
        lines.push_back(l);
        }
      else
        {
        OffsetValueType prevLength = prev.line.GetLength();
        OffsetValueType length = l.line.GetLength();

        if ( prevIdx[0] + prevLength >= idx[0] )
          {
          // the lines are overlapping. We need to choose which line to keep.
          // the label, the only "attribute" to be guaranteed to be unique, is
          // used to choose
          // which line to keep. This is necessary to avoid the case where a
          // part of a label is over
          // a second label, and below in another part of the image.
          bool keepCurrent;
          typename TAttributeAccessor::AttributeValueType prevAttr = accessor(prev.labelObject);
          typename TAttributeAccessor::AttributeValueType attr = accessor(l.labelObject);
          // this may be changed to a single boolean expression, but may become
          // quite difficult to read
          if ( Math::ExactlyEquals(attr, prevAttr)  )
            {
            if ( l.labelObject->GetLabel() > prev.labelObject->GetLabel() )
              {
              keepCurrent = !m_ReverseOrdering;
              }
            else
              {
              keepCurrent = m_ReverseOrdering;
              }
            }
          else
            {
            if ( attr > prevAttr )
              {
              keepCurrent = !m_ReverseOrdering;
              }
            else
              {
              keepCurrent = m_ReverseOrdering;
              }
            }

          if ( keepCurrent )
            {
            // keep the current one. We must truncate the previous one to remove
            // the
            // overlap, and take care of the end of the previous line if it
            // extends
            // after the current one.
            if ( prevIdx[0] + prevLength > idx[0] + length )
              {
              // the previous line is longer than the current one. Lets take its
              // tail and
              // add it to the priority queue
              IndexType newIdx = idx;
              newIdx[0] = idx[0] + length;
              OffsetValueType newLength = prevIdx[0] + prevLength - newIdx[0];
              priorityQueue.push( LineOfLabelObject(LineType(newIdx, newLength), prev.labelObject) );
              }
            // truncate the previous line to let some place for the current one
            prevLength = idx[0] - prevIdx[0];
            if ( prevLength != 0 )
              {
              lines.back(). line.SetLength(idx[0] - prevIdx[0]);
              }
            else
              {
              // length is 0 - no need to keep that line
              lines.pop_back();
              }
            // and push the current one
            lines.push_back(l);
            }
          else
            {
            // keep the previous one. If the previous line fully overlap the
            // current one,
            // the current one is fully discarded.
            if ( prevIdx[0] + prevLength > idx[0] + length )
              {
              // discarding the current line - just do nothing
              }
            else
              {
              IndexType newIdx = idx;
              newIdx[0] = prevIdx[0] + prevLength;
              OffsetValueType newLength = idx[0] + length - newIdx[0];
              l.line.SetIndex(newIdx);
              l.line.SetLength(newLength);
              lines.push_back(l);
              }
            }
          }
        else
          {
          // no overlap - things are just fine already
          lines.push_back(l);
          }
        }

      // store the current line as the previous one, and go to the next one.
      prev = lines.back();
      prevIdx = prev.line.GetIndex();
      }

    // put the lines in their object
    for ( size_t i = 0; i < lines.size(); ++i )
      {
      LineOfLabelObject & l = lines[i];
      l.labelObject->AddLine(l.line);
      }

    // remove objects without lines
    typename ImageType::Iterator it( this->GetLabelMap() );
    while ( ! it.IsAtEnd() )
      {
      typename LabelObjectType::LabelType label = it.GetLabel();
      LabelObjectType *labelObject = it.GetLabelObject();

      if ( labelObject->Empty() )
        {
        // must increment the iterator before removing the object to avoid
        // invalidating the iterator
        ++it;
        this->GetLabelMap()->RemoveLabel(label);
        }
      else
        {
        ++it;
        }
      }
  }

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  AttributeType m_Attribute;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapeUniqueLabelMapFilter);

  bool m_ReverseOrdering;
  struct LineOfLabelObject {
    typedef typename LabelObjectType::LineType LineType;
    LineOfLabelObject(const LineType _line, LabelObjectType *_lo)
    {
      this->line = _line;
      this->labelObject = _lo;
    }

    LineType line;
    LabelObjectType *labelObject;
  };

  class LineOfLabelObjectComparator
  {
public:
    bool operator()(const LineOfLabelObject & lla, const LineOfLabelObject & llb)
    {
      for ( int i = ImageDimension - 1; i >= 0; i-- )
        {
        if ( lla.line.GetIndex()[i] > llb.line.GetIndex()[i] )
          {
          return true;
          }
        else if ( lla.line.GetIndex()[i] < llb.line.GetIndex()[i] )
          {
          return false;
          }
        }
      return false;
    }
  };
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeUniqueLabelMapFilter.hxx"
#endif

#endif
