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
#ifndef itkLabelObject_hxx
#define itkLabelObject_hxx

#include "itkLabelObject.h"
#include "itkLabelObjectLineComparator.h"
#include "itkMath.h"
#include <algorithm>

namespace itk
{
template< typename TLabel, unsigned int VImageDimension >
LabelObject< TLabel, VImageDimension >::LabelObject()
{
  m_Label = NumericTraits< LabelType >::ZeroValue();
  m_LineContainer.clear();
}

template< typename TLabel, unsigned int VImageDimension >
typename LabelObject< TLabel, VImageDimension >::AttributeType
LabelObject< TLabel, VImageDimension >
::GetAttributeFromName(const std::string & s)
{
  if ( s == "Label" )
    {
    return LABEL;
    }
  // can't recognize the name
  itkGenericExceptionMacro(<< "Unknown attribute: " << s);
}

template< typename TLabel, unsigned int VImageDimension >
std::string
LabelObject< TLabel, VImageDimension >
::GetNameFromAttribute(const AttributeType & a)
{
  switch ( a )
    {
    case LABEL:
      return "Label";
    }
  // can't recognize the namespace
  itkGenericExceptionMacro(<< "Unknown attribute: " << a);
}

/**
 * Set/Get the label associated with that object.
 */
template< typename TLabel, unsigned int VImageDimension >
const typename LabelObject< TLabel, VImageDimension >::LabelType &
LabelObject< TLabel, VImageDimension >
::GetLabel() const
{
  return m_Label;
}

template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >::SetLabel(const LabelType & label)
{
  m_Label = label;
}

/**
 * Return true if the object contain the given index and false otherwise.
 * Worst case complexity is O(L) where L is the number of lines in the object.
 */
template< typename TLabel, unsigned int VImageDimension >
bool
LabelObject< TLabel, VImageDimension >
::HasIndex(const IndexType & idx) const
{
  typedef typename LineContainerType::const_iterator LineContainerConstIteratorType;
  LineContainerConstIteratorType end = m_LineContainer.end();

  for ( LineContainerConstIteratorType it = m_LineContainer.begin();
        it != end;
        ++it )
    {
    if ( it->HasIndex(idx) )
      {
      return true;
      }
    }
  return false;
}


template< typename TLabel, unsigned int VImageDimension >
bool
LabelObject< TLabel, VImageDimension >
::RemoveIndex(const IndexType & idx)
{
  typename LineContainerType::iterator it = m_LineContainer.begin();

  while( it != m_LineContainer.end() )
    {
    if( it->HasIndex( idx ) )
      {
      IndexType orgLineIndex    = it->GetIndex();
      LengthType orgLineLength  = it->GetLength();

      if( orgLineLength == 1 )
        {
        // remove the line and exit
        m_LineContainer.erase( it );
        return true;
        }

      if( orgLineIndex == idx )
        {
        // shift the index to the right and decrease the length by one
        ++orgLineIndex[0];
        it->SetIndex( orgLineIndex );
        it->SetLength( orgLineLength - 1 );
        return true;
        }
      else if( orgLineIndex[0] + static_cast< IndexValueType >( orgLineLength ) - 1 == idx[0] )
        {
        // decrease the length by one
        it->SetLength( orgLineLength - 1 );
        return true;
        }
      else
        {
        // we have to split the line in two parts
        it->SetLength( idx[0] - orgLineIndex[0] );
        IndexType newIdx = idx;
        ++newIdx[0];
        LengthType newLength = orgLineLength - it->GetLength() - 1;
        m_LineContainer.push_back( LineType( newIdx, newLength ) );
        return true;
        }
      }
    ++it;
    }
  return false;
}

/**
 * Add an index to the object. If the index is already in the object, the index can
 * be found several time in the object.
 */
template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::AddIndex(const IndexType & idx)
{
  if ( !m_LineContainer.empty() )
    {
    // can we use the last line to add that index ?
    LineType & lastLine = *m_LineContainer.rbegin();
    if ( lastLine.IsNextIndex(idx) )
      {
      lastLine.SetLength(lastLine.GetLength() + 1);
      return;
      }
    }
  // create a new line
  this->AddLine(idx, 1);
}

/**
 * Add a new line to the object, without any check.
 */
template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::AddLine(const IndexType & idx, const LengthType & length)
{
  LineType line(idx, length);

  this->AddLine(line);
}

/**
 * Add a new line to the object, without any check.
 */
template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::AddLine(const LineType & line)
{
  m_LineContainer.push_back(line);
}

template< typename TLabel, unsigned int VImageDimension >
typename LabelObject< TLabel, VImageDimension >::SizeValueType
LabelObject< TLabel, VImageDimension >
::GetNumberOfLines() const
{
  return static_cast<typename LabelObject< TLabel, VImageDimension >::SizeValueType>( m_LineContainer.size());
}

template< typename TLabel, unsigned int VImageDimension >
const
typename LabelObject< TLabel, VImageDimension >::LineType &
LabelObject< TLabel, VImageDimension >
::GetLine(SizeValueType i) const
{
  return m_LineContainer[i];
}

template< typename TLabel, unsigned int VImageDimension >
typename LabelObject< TLabel, VImageDimension >::LineType &
LabelObject< TLabel, VImageDimension >
::GetLine(SizeValueType i)
{
  return m_LineContainer[i];
}

template< typename TLabel, unsigned int VImageDimension >
typename LabelObject< TLabel, VImageDimension >::SizeValueType
LabelObject< TLabel, VImageDimension >
::Size() const
{
  int size = 0;

  for ( typename LineContainerType::const_iterator it = m_LineContainer.begin();
        it != m_LineContainer.end();
        it++ )
    {
    size += it->GetLength();
    }
  return size;
}

template< typename TLabel, unsigned int VImageDimension >
bool
LabelObject< TLabel, VImageDimension >
::Empty() const
{
  return this->m_LineContainer.empty();
}

template< typename TLabel, unsigned int VImageDimension >
typename LabelObject< TLabel, VImageDimension >::IndexType
LabelObject< TLabel, VImageDimension >
::GetIndex(SizeValueType offset) const
{
  SizeValueType o = offset;

  typename LineContainerType::const_iterator it = this->m_LineContainer.begin();

  while ( it != m_LineContainer.end() )
    {
    SizeValueType size = it->GetLength();

    if ( o >= size )
      {
      o -= size;
      }
    else
      {
      IndexType idx = it->GetIndex();
      idx[0] += o;
      return idx;
      }

    it++;
    }
  itkGenericExceptionMacro(<< "Invalid offset: " << offset);
}

/** Copy the lines from another node. */
template< typename TLabel, unsigned int VImageDimension >
template<typename TSourceLabelObject>
void
LabelObject< TLabel, VImageDimension >
::CopyLinesFrom(const TSourceLabelObject *src)
{
  itkAssertOrThrowMacro ( ( src != ITK_NULLPTR ), "Null Pointer" );
  // clear original lines and copy lines
  m_LineContainer.clear();
  for( size_t i = 0; i < src->GetNumberOfLines(); ++i )
    {
    this->AddLine( src->GetLine( static_cast< SizeValueType >( i ) ) );
    }
  this->Optimize();
}

/** Copy the attributes of another node to this one */
template< typename TLabel, unsigned int VImageDimension >
template<typename TSourceLabelObject>
void
LabelObject< TLabel, VImageDimension >
::CopyAttributesFrom(const TSourceLabelObject *src)
{
  itkAssertOrThrowMacro ( ( src != ITK_NULLPTR ), "Null Pointer" );
  m_Label = src->GetLabel();
}

/** Copy the lines, the label and the attributes from another node. */
template< typename TLabel, unsigned int VImageDimension >
template<typename TSourceLabelObject>
void
LabelObject< TLabel, VImageDimension >
::CopyAllFrom(const TSourceLabelObject *src)
{
  itkAssertOrThrowMacro ( ( src != ITK_NULLPTR ), "Null Pointer" );
  // Basically all derived class just need to copy the following two
  // lines to copy all data members
  this->template CopyLinesFrom<TSourceLabelObject>( src );
  this->template CopyAttributesFrom<TSourceLabelObject>( src );
}

/** Reorder the lines, merge the touching lines and ensure that no
 * pixel is covered by two lines
 */
template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::Optimize()
{
  if ( !m_LineContainer.empty() )
    {
    // first copy the lines in another container and clear the current one
    LineContainerType lineContainer = m_LineContainer;
    m_LineContainer.clear();

    // reorder the lines
    typename Functor::LabelObjectLineComparator< LineType > comparator;
    std::sort(lineContainer.begin(), lineContainer.end(), comparator);

    // then check the lines consistancy
    // we'll proceed line index by line index
    IndexType currentIdx = lineContainer.begin()->GetIndex();
    LengthType  currentLength = lineContainer.begin()->GetLength();

    typename LineContainerType::const_iterator it = lineContainer.begin();

    while ( it != lineContainer.end() )
      {
      const LineType & line = *it;
      IndexType        idx = line.GetIndex();
      LengthType    length = line.GetLength();

      // check the index to be sure that we are still in the same line idx
      bool sameIdx = true;
      for ( unsigned int i = 1; i < ImageDimension; i++ )
        {
        if ( currentIdx[i] != idx[i] )
          {
          sameIdx = false;
          }
        }

      // try to extend the current line idx, or create a new line
      if ( sameIdx && currentIdx[0] + (OffsetValueType)currentLength >= idx[0] )
        {
        // we may expand the line
        LengthType newLength = idx[0] + (OffsetValueType)length - currentIdx[0];
        currentLength = std::max(newLength, currentLength);
        }
      else
        {
        // add the previous line to the new line container and use the new line
        // index and size
        this->AddLine(currentIdx, currentLength);
        currentIdx = idx;
        currentLength = length;
        }

      it++;
      }

    // complete the last line
    this->AddLine(currentIdx, currentLength);
    }
}

template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::Shift( OffsetType offset )
{
  for( typename LineContainerType::iterator it = m_LineContainer.begin();
       it != m_LineContainer.end();
       it++ )
    {
    LineType & line = *it;
    line.SetIndex( line.GetIndex() + offset );
    }
}

template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >
::Clear()
{
  m_LineContainer.clear();
}

template< typename TLabel, unsigned int VImageDimension >
void
LabelObject< TLabel, VImageDimension >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "LineContainer: " << &m_LineContainer << std::endl;
  os << indent << "Label: " << static_cast< typename NumericTraits< LabelType >::PrintType >( m_Label ) << std::endl;
}
} // end namespace itk

#endif
