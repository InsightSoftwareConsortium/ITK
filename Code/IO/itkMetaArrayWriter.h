/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaArrayWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaArrayWriter_h
#define __itkMetaArrayWriter_h

#include "itkLightProcessObject.h"
#include "itkArray.h"
#include "itkFixedArray.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkVariableLengthVector.h"
#include "metaArray.h"

namespace itk
{

class MetaArrayWriter : public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef MetaArrayWriter           Self;
  typedef LightProcessObject        Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaArrayWriter, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set the filename to which the data is written 
   *    Optional param - use if header and data should be
   *      in separate files.  */
  itkSetStringMacro(DataFileName);

  /** Get the filename to which the data will be written */
  itkGetStringMacro(DataFileName);

  /** Set the writer to store the array as binary data */
  itkSetMacro(Binary, bool);
  /** Returns true if the file to be produced will store the data in binary
   *    (instead of ascii) format */
  itkGetMacro(Binary, bool);

  /** Set the input itk Array to write */
  template <typename TValueType>
  void SetInput(MET_ValueEnumType _metaElementType,
                const Array<TValueType> * _array)
    {
    m_Buffer = (const void *)(_array->data_block());
    m_MetaArray.InitializeEssential(_array->Size(),
                                    _metaElementType);
    }

  /** Set the input itk FixedArray to write */
  template <typename TValueType, unsigned int vLength>
  void SetInput(MET_ValueEnumType _metaElementType,
                const FixedArray<TValueType, vLength> * _array)
    {
    m_Buffer = (const void *)(_array->GetDataPointer());
    m_MetaArray.InitializeEssential(vLength,
                                    _metaElementType);
    }

  /** Set the input itk Vector to write */
  template <typename TValueType, unsigned int vLength>
  void SetInput(MET_ValueEnumType _metaElementType,
                const Vector<TValueType, vLength> * _vector)
    {
    m_Buffer = (const void *)(_vector->GetDataPointer());
    m_MetaArray.InitializeEssential(vLength,
                                    _metaElementType);
    }

  /** Set the input itk CovariantVector to write */
  template <typename TValueType, unsigned int vLength>
  void SetInput(MET_ValueEnumType _metaElementType,
                const CovariantVector<TValueType, vLength> * _vector)
    {
    m_Buffer = (const void *)(_vector->GetDataPointer());
    m_MetaArray.InitializeEssential(vLength,
                                    _metaElementType);
    }

  /** Set the input itk VariableLengthVector to write */
  template <typename TValueType>
  void SetInput(MET_ValueEnumType _metaElementType,
                const VariableLengthVector<TValueType> * _vector)
    {
    m_Buffer = (const void *)(_vector->GetDataPointer());
    m_MetaArray.InitializeEssential(_vector->Size(),
                                    _metaElementType);
    }

  /** Copies the elements from an array of arrays into the output
   *    buffer.   Requires all sub-arrays to have the same length.
   *    length of the major array is the "length" of the array, while
   *    the length of teh sub-arrays is the "number of channels" at each
   *    array position.   Expected form itk::Array< itk::Array< * > >.
   *    May work for other sub-array-types that define the [] operator and the
   *    GetSize() function.  */
  template <typename TValueType>
  void SetMultiChannelInput(MET_ValueEnumType _metaElementType,
                            int _NumberOfChannels,
                            const Array<TValueType> * _array)
    {
    int rows = _array->GetSize();
    int cols = (*_array)[0].GetSize();
    m_MetaArray.InitializeEssential(rows, 
                                    _metaElementType,
                                    cols,
                                    NULL,
                                    true,
                                    true);
    m_Buffer = m_MetaArray.ElementData();
    for(int i=0; i<rows; i++)
      {
      for(int j=0; j<cols; j++)
        {
        m_MetaArray.ElementData(i*cols+j, (double)((*_array)[i][j]));
        }
      }
    }


  /** Set/Get the precision of the writing */
  itkSetMacro(Precision, unsigned int);
  itkGetMacro(Precision, unsigned int);

  /** Set the data type written to the file */
  void ConvertTo(MET_ValueEnumType _metaElementType);

  /** Write out the array */
  void Update();

protected:

  MetaArrayWriter();
  virtual ~MetaArrayWriter();

private:

  bool          m_Binary;

  unsigned int  m_Precision;

  std::string   m_FileName;
  std::string   m_DataFileName;

  MetaArray     m_MetaArray;

  const void *  m_Buffer;

};

} // namespace itk


#endif // __itkMetaArrayWriter_h
