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
#ifndef __itkLabelObject_h
#define __itkLabelObject_h

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
 * All the subclasses of LabelObject have to reinplement the CopyAttributesFrom() method.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapFilter, AttributeLabelObject
 * \ingroup DataRepresentation
 * \ingroup LabeledImageObject
 * \ingroup ITK-Review
 */
template< class TLabel, unsigned int VImageDimension >
class ITK_EXPORT LabelObject:public LightObject
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
  typedef typename std::deque< LineType >    LineContainerType;
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
   * Add a new line to the object, without any check.
   */
  void AddLine(const IndexType & idx, const LengthType & length);

  /**
   * Add a new line to the object, without any check.
   */
  void AddLine(const LineType & line);

  /** Return the line container of this object */
  const LineContainerType & GetLineContainer() const;

  LineContainerType & GetLineContainer();

  void SetLineContainer(const LineContainerType & lineContainer);

  SizeValueType GetNumberOfLines() const;

  const LineType & GetLine(SizeValueType i) const;

  LineType & GetLine(SizeValueType i);

  SizeValueType Size() const;

  bool Empty() const;

  IndexType GetIndex(SizeValueType offset) const;

  /** Copy the attributes of another node to this one */
  virtual void CopyAttributesFrom(const Self *src);

  /** Copy the lines, the label and the attributes from another node. */
  void CopyAllFrom(const Self *src);

  /** Reorder the lines, merge the touching lines and ensure that no
   * pixel is covered by two lines
   */
  void Optimize();

  /** Shift the object position */
  void Shift( OffsetType offset );

protected:
  LabelObject();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  LabelObject(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  LineContainerType m_LineContainer;
  LabelType         m_Label;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelObject.txx"
#endif

#endif
