/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomSeriesReadPrintTags.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a DICOM series into a volume and then
//  print most of the DICOM header information. The binary field are skipped.
//
//  \index{DICOM!Header}
//  \index{DICOM!Tags}
//  \index{DICOM!Printing Tags}
//
//  Software Guide : EndLatex 



// Software Guide : BeginLatex
// 
// The header files for the series reader and the GDCM classes for image IO and
// name generation should be included first.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageSeriesReader.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
// Software Guide : EndCodeSnippet




int main( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomDirectory " << std::endl;
    return EXIT_FAILURE;
    }



// Software Guide : BeginLatex
// 
//  We instantiate then the type to be used for storing the image once it is
//  read into memory.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef signed short       PixelType;
  const unsigned int         Dimension = 3;
  
  typedef itk::Image< PixelType, Dimension >      ImageType;
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
// 
// We use the image type for instantiating the series reader type and then we
// construct one object of this class.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// A GDCMImageIO object is created and assigned to the reader.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO       ImageIOType;
    
  ImageIOType::Pointer dicomIO = ImageIOType::New();

  reader->SetImageIO( dicomIO );
// Software Guide : EndCodeSnippet






// Software Guide : BeginLatex
// 
// A GDCMSeriesFileNames is declared in order to generate the names of DICOM
// slices. We specify the directory with the \code{SetInputDirectory()} method
// and, in this case, take the directory name from the command line arguments.
// You could have obtained the directory name from a file dialog in a GUI.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GDCMSeriesFileNames     NamesGeneratorType;
  
  NamesGeneratorType::Pointer nameGenerator = NamesGeneratorType::New();
  
  nameGenerator->SetInputDirectory( argv[1] );
// Software Guide : EndCodeSnippet
  


// Software Guide : BeginLatex
// 
// The list of files to read is obtained from the name generator by invoking
// the \code{GetInputFileNames()} method and receiving the results in a
// container of strings. The list of filenames is passed to the reader using
// the \code{SetFileNames()} method.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef std::vector<std::string>    FileNamesContainer;
  FileNamesContainer fileNames = nameGenerator->GetInputFileNames();

  reader->SetFileNames( fileNames );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
// 
// We trigger the reader by invoking the \code{Update()} method. This
// invokation should normally be done inside a \code{try/catch} block given
// that it may eventually throw exceptions.
//
// Software Guide : EndLatex

  try
    {
// Software Guide : BeginCodeSnippet
    reader->Update();
// Software Guide : EndCodeSnippet
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  
  


// Software Guide : BeginLatex
// 
// ITK internally queries GDCM and obtain all the DICOM tags from the file
// headers. The tag values are stored in the \doxygen{MetaDataDictionary} that
// is a general purpose container for \{key,value\} pairs. The Meta data
// dictionary can be recovered from any ImageIO class by invoking the
// \code{GetMetaDataDictionary()} method.
//
// \index{MetaDataDictionary}
// \index{ImageIO!GetMetaDataDictionary()}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::MetaDataDictionary   DictionaryType;

  const  DictionaryType & dictionary = dicomIO->GetMetaDataDictionary();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// In this example, we are only interested in the DICOM tags that can be
// represented as strings. We declare therefore a \doxygen{MetaDataObject} of
// string type in order to receive those particular values.
//
// \index{MetaDataDictionary!MetaDataObject}
// \index{MetaDataDictionary!String entries}
// \index{MetaDataObject!Strings}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::MetaDataObject< std::string > MetaDataStringType;
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// The Meta data dictionary is organized as a container with its corresponding
// iterators. We can therefore visit all its entries by first getting access to
// its \code{Begin()} and \code{End()} methods.
//
// \index{MetaDataDictionary!Begin()}
// \index{MetaDataDictionary!End()}
// \index{MetaDataDictionary!Iterator}
// \index{MetaDataDictionary!ConstIterator}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  DictionaryType::ConstIterator itr = dictionary.Begin();
  DictionaryType::ConstIterator end = dictionary.End();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// We are now ready for walking through the list of DICOM tags. For this
// purpose we use the iterators that we just declared. At every entry we
// attempt to convert it in to a string entry by using the \code{dynamic\_cast}
// based on RTTI information\footnote{Run Time Type Information}. The
// dictionary is organized like a \code{std::map} structure, we should use
// therfore the \code{first} and \code{second} members of every entry in order
// to get access to the \{key,value\} pairs.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry = itr->second;

    MetaDataStringType::Pointer entryvalue = 
      dynamic_cast<MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {
      std::string tagkey   = itr->first;
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << tagkey <<  " = " << tagvalue << std::endl;
      }

    ++itr;
    }
// Software Guide : EndCodeSnippet




  
  
//  Software Guide : BeginLatex
//
//  It is also possible to query for specific entries instead of reading all of
//  them as we did above. In this case, the user must provide the tag
//  identifier using the DICOM Standar encoding. The identifier is stored in a
//  string and used as key on the dictionary. 
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
    std::string entryId = "0010|0010";

    DictionaryType::ConstIterator tagItr = dictionary.Find( entryId );

    if( tagItr == end )
      {
      std::cerr << "Tag " << entryId;
      std::cerr << " not found in the DICOM header" << std::endl;
      }
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// Since the entry may or may not be of string type we must again use a
// \code{dynamic\_cast} in order to attempt to convert it to a string dictionary
// entry. If the convertion is successful, then we can print out its content.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
    MetaDataStringType::ConstPointer entryvalue = 
      dynamic_cast<const MetaDataStringType *>( tagItr->second.GetPointer() );

    if( entryvalue )
      {
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << "Patient's Name (" << entryId <<  ") ";
      std::cout << " is: " << tagvalue << std::endl;
      }
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// This type of functionality will probably be more useful when provided
// through a graphical user interface. For a full description of the DICOM
// dictionary please look at the file
//
// \code{Insight/Utilities/gdcm/Dicts/dicomV3.dic}
//
// Software Guide : EndLatex



  return EXIT_SUCCESS;

}

