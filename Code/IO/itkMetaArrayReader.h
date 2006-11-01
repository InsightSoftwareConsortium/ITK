/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaArrayReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaArrayReader_h
#define __itkMetaArrayReader_h

#include "itkLightProcessObject.h"
#include "itkArray.h"
#include "itkFixedArray.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkVariableLengthVector.h"
#include "metaArray.h"

namespace itk
{

class MetaArrayReader : public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef MetaArrayReader         Self;
  typedef LightProcessObject       Superclass;

  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaArrayReader, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Get a pointer to the metaArray so that you can change 
   *    metaArray options (e.g., add comments and user-defined fields */
  MetaArray * GetMetaArrayPointer(void);

  /** Specify the buffer (already allocated) into which data should be read */
  void SetBuffer(void * _buffer);

  /** Read the Array in MetaIO format */
  void Update();

  /** Return the MetaIO type that corresponds to the element type */
  MET_ValueEnumType GetDataType(void) const
    {
    return m_MetaArray.ElementType();
    }

  /** Size is used by itkArray and VariableLengthVector */
  int Size(void) const
    {
    return m_MetaArray.Length();
    }

  /** GetNumberOfElements is used by itkArray and VariableLengthVector */
  int GetNumberOfElements(void) const
    {
    return m_MetaArray.Length();
    }

  /** GetVectorDimension is used by itkVector */
  int GetVectorDimension(void) const
    {
    return m_MetaArray.Length();
    }

  /** GetNumberOfComponents is used by itkVector and itkCovariantVector */
  int GetNumberOfComponents(void) const
    {
    return m_MetaArray.Length();
    }

  /** GetCovariantVectorDimension is used by itkVector */
  int GetCovariantVectorDimension(void) const
    {
    return m_MetaArray.Length();
    }

  /** GetElement is used by itkArray, itkFixedArray, and VariableLengthVector */
  template <typename TValueType>
  inline void GetElement( TValueType & value, unsigned int i, unsigned int channel=0 ) const
    {
    value = static_cast<TValueType>(m_MetaArray.ElementData( 
              i * m_MetaArray.ElementNumberOfChannels() + channel));
    }

  /** Get itk Array 
   *      Specify the MetaType of the elements of the Array and provide a
   *    pointer to the Array.  The buffer of the Array is replaced by the
   *    buffer into which the data was directly read (a copy does not occur).
   *      If _letVectorManageData is true, the buffer persists even after this
   *    MetaArrayReader is destroyed.  Otherwise, the buffer (and therefore
   *    the validity of the dat ain the Array) is destroyed when the
   *    MetaArrayReader is dstroyed. */
  template <typename TValueType>
  void GetOutput(MET_ValueEnumType _metaElementType,
                   Array<TValueType> * _array,
                   bool _letArrayManageData = true)
    {
    if(m_MetaArray.ElementType() != _metaElementType)
      {
      m_MetaArray.ConvertElementDataTo(_metaElementType);
      }
    _array->SetData((TValueType *)(m_MetaArray.ElementData()), 
                    m_MetaArray.Length(),
                    _letArrayManageData);
    if(_letArrayManageData)
      {
      m_MetaArray.AutoFreeElementData(false);
      }
    }

  /** Get itk FixedArray */
  template <typename TValueType, unsigned int vLength>
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 FixedArray<TValueType, vLength> * _array)
    {
    if(static_cast<int>(vLength) <= m_MetaArray.Length())
      {
      unsigned int i;
      for(i = 0; i<vLength; i++)
        {
        this->GetElement( (*_array)[i], i );
        }
      return true;
      }
    return false;
    }

  /** Get itk Vector 
   *    Specify the MetaType of the elements of the itkVector and provide
   *    a pointer to the itkVector into which the data should be copied */
  template <typename TValueType, unsigned int vLength>
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 Vector<TValueType, vLength> * _vector)
    {
    if(static_cast<int>(vLength) <= m_MetaArray.Length())
      {
      unsigned int i;
      for(i = 0; i<vLength; i++)
        {
        this->GetElement( (*_vector)[i], i );
        }
      return true;
      }
    return false;
    }


  /** Get itk CovariantVector 
   *    Specify the MetaType of the elements of the itkCovariantVector and
   *    provide a pointer to the itkCovariantVector into which the data
   *    should be copied */
  template <typename TValueType, unsigned int vLength>
  bool GetOutput(MET_ValueEnumType itkNotUsed(_metaElementType),
                 CovariantVector<TValueType, vLength> * _vector)
    {
    if(static_cast<int>(vLength) <= m_MetaArray.Length())
      {
      unsigned int i;
      for(i = 0; i<vLength; i++)
        {
        this->GetElement( (*_vector)[i], i );
        }
      return true;
      }
    return false;
    }

  /** Get itk VariableLengthVector 
   *      Specify the MetaType of the elements of the VariableLengthVector and
   *    provide a pointer to the VariableLengthVector.  The buffer of the
   *    VariableLengthVector is replaced by the buffer into which the data
   *    was directly read (a copy does not occur).
   *      If _letVectorManageData is true, the buffer persists even after this
   *    MetaArrayReader is destroyed.  Otherwise, the buffer (and therefore
   *    the validity of the dat ain the VariableLengthVector) is destroyed
   *    when the MetaArrayReader is dstroyed. */
  template <typename TValueType>
  void GetOutput(MET_ValueEnumType _metaElementType,
                 VariableLengthVector<TValueType> * _vector,
                 bool _letVectorManageData = true)
    {
    if(m_MetaArray.ElementType() != _metaElementType)
      {
      m_MetaArray.ConvertElementDataTo(_metaElementType);
      }
    _vector->SetData((TValueType *)(m_MetaArray.ElementData()), 
                     m_MetaArray.Length(),
                     _letVectorManageData);
    if(_letVectorManageData)
      {
      m_MetaArray.AutoFreeElementData(false);
      }
    }



protected:

  MetaArrayReader();
  virtual ~MetaArrayReader();

private:

  MetaArray    m_MetaArray;

  std::string  m_FileName;

  void *       m_Buffer;

};

} // namespace itk


#endif // __itkTransformFileReader_h
