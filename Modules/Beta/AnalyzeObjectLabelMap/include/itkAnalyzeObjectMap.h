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
#ifndef itkAnalyzeObjectMap_h
#define itkAnalyzeObjectMap_h

#include <cstdio>
#include <string>
#include <vector>
#include "itkAnalyzeObjectEntry.h"
#include "itkObject.h"
#include <itkMetaDataDictionary.h>
#include "itkMetaDataObject.h"
#include "itkThresholdImageFilter.h"

const char * const ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY = "ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY";
/**
 * Constants representing the current version number of the object map file for Analyze
 */
constexpr int    VERSION1 = 880102;
constexpr int    VERSION2 = 880801;
constexpr int    VERSION3 = 890102;
static const int VERSION4 = 900302;
static const int VERSION5 = 910402;
static const int VERSION6 = 910926;
static const int VERSION7 = 20050829;

namespace itk
{

using AnalyzeObjectEntryArrayType = std::vector<AnalyzeObjectEntry::Pointer>;
template <class TImage = itk::Image<unsigned char, 4>, class TRGBImage = itk::Image<itk::RGBPixel<unsigned char>, 4>>

/** \class AnalyzeObjectMap
 *  \ingroup AnalyzeObjectMapIO
 *  \brief A class that is an image with functions that let the user change aspects of the class.  This
 * is a templated class where most everything will depend on the Image type that is used.
 */
class AnalyzeObjectMap : public TImage
{
public:
  /** Standard type alias. */
  using Self = AnalyzeObjectMap;
  using Superclass = TImage;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ImageType = TImage;
  using ObjectMapType = itk::AnalyzeObjectMap<TImage>;

  using PixelType = typename TImage::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeObjectMap, TImage);

  /**
   * \brief an assignment operator
   * \param const AnalyzeObjectMap & rhs
   * \return AnalyzeObjectMap &, a new object created from the right hand side
   */
  AnalyzeObjectMap &
  operator=(const AnalyzeObjectMap & rhs);

  /**
   *\brief GetAnalyzeObjectEntryArrayPointer
   *
   *This will return a pointer to the vector of object entries that an object map has.
   */
  AnalyzeObjectEntryArrayType *
  GetAnalyzeObjectEntryArrayPointer();

  /**
   * \brief GetNumberOfObjects/SetNumberOfObjects
   *
   * This function is used to Get/Set the number of objects in the Object map
   */
  itkSetMacro(NumberOfObjects, int);
  itkGetConstMacro(NumberOfObjects, int);

  /**
   * \brief PickOneEntry
   *
   *The user will input the number of the Object entry that they would like to pick.
   *Then the function will create a new object map that will re returned.
   *Then the function will go through the original object map image and get rid of
   *all of the other object numbers and change the number of the object entry the
   *user specified to one.  Then the new image will be outputted to the new Object map.
   *After that the object entry from the original object map will be copied over to the
   *new object map's vector of object entries.
   */
  typename itk::AnalyzeObjectMap<TImage>::Pointer
  PickOneEntry(const int numberOfEntry = -1);

  /**
   * \brief ObjectMapToRGBImage
   *
   *This will convert the object map into an RGB Image based on the end red, end green and
   *end blue specified for each object entry.  That means that the function will have to go
   *through the object map image, get the value at each pixel, find the object entry that
   *corresponds to the value of the pixel and then pull out the end red, end green and end
   *blue and set that as the pixel color for the RGB Image.  Then that RGB Image will be returned.
   */
  typename TRGBImage::Pointer
  ObjectMapToRGBImage();

  /**
   * \brief AddObjectEntryBasedOnImagePixel
   *
   *This will go through an image that the user inputs, find the specific pixel value the user
   *inputs and then create an object map at the location that it finds the specific pixel value.
   The user will also have the option of inputing the red, green and blue they want the object map to be.
   */
  void
  AddObjectEntryBasedOnImagePixel(ImageType *       Image,
                                  const int         value = -1,
                                  const std::string ObjectName = "",
                                  const int         Red = 0,
                                  const int         Green = 0,
                                  const int         Blue = 0);

  /**
   * \brief AddObjectEntry
   *
   *This will just add an object entry to the end of the vector of object entries that an object map has.
   */
  void
  AddAnalyzeObjectEntry(const std::string ObjectName = "");

  /**
   * \brief DeleteObjectEntry
   *
   * This will delete an object entry that a user specifies.
   *The function will go through the image and delete the number that corresponds to the object entry.
   *Then the function will move all of the object entry numbers above the object entry that was deleted down one number.
   *Then the function will move all of the object entries above the object entry that was deleted in the vector one
   *number down.
   */
  void
  DeleteAnalyzeObjectEntry(const std::string ObjectName = "");

  /**
   * \brief FindObject
   *
   *This function will find an object entry based on the name that the user inputs.
   *If the function finds the object entry then it will return the number of the vector of the object entry.
   *If the function does not find the object entry then the function will return -1.
   */
  int
  FindObjectEntry(const std::string ObjectName = "");

  /**
   * \brief PlaceObjectMapEntriesIntoMetaData
   *
   *This function will place the object entries into the meta data so that the object map can be moved around
   *just like a normal image.  This function is normally called in the functions that are in this class.
   */
  void
  PlaceObjectMapEntriesIntoMetaData();

  /**
   * \brief GetObjectEntry
   *
   * This function will return the smart pointer of the object entry the user inputs.
   */
  AnalyzeObjectEntry::Pointer
  GetObjectEntry(const int index);

  /**
   * \brief GetObjectEntry const
   *
   * This function will return the smart pointer of the object entry the user inputs.
   */
  const AnalyzeObjectEntry::Pointer
  GetObjectEntry(const int index) const;

  /**
   *\brief ImageToObjectMap
   *
   *This function will take an image and make it into an object map.
   *If there is data for object entries in the meta data then extract that data.
   *Then take the pixel container of the image and place it into the object map's pixel container.
   */
  void
  ImageToObjectMap(ImageType * image);

protected:
  /**
   * \brief the default constructor
   */
  AnalyzeObjectMap();

  /**
   * \brief the destructor for AnalyzeObjectMap
   */
  ~AnalyzeObjectMap() override;

  /**
   * \brief the copy constructor,
   * THIS IS NOT ALLOWED
   */
  AnalyzeObjectMap(const AnalyzeObjectMap & /* rhs */); /*Explicitly not allowed*/

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Number of Objects in the object file */
  int m_NumberOfObjects{ 1 };
  /** Pointers to individual objects in the object map, maximum of 256 */
  AnalyzeObjectEntryArrayType m_AnaylzeObjectEntryArray;
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAnalyzeObjectMap.hxx"
#endif
#endif // __OBJECTMAP_H_
