/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmIPPSorter.h"
#include "gdcmScanner.h"
#include "gdcmElement.h"
#include "gdcmDirectionCosines.h"

#include <map>
#include <math.h>

namespace gdcm
{

IPPSorter::IPPSorter()
{
  ComputeZSpacing = true;
  ZSpacing = 0;
  ZTolerance = 1e-6;
}


IPPSorter::~IPPSorter()
{
}

inline double spacing_round(double n, int d) /* pow is defined as pow( double, double) or pow(double int) on M$ comp */
{
  return floor(n * pow(10., d) + .5) / pow(10., d);
}

bool IPPSorter::Sort(std::vector<std::string> const & filenames)
{
  // BUG: I cannot clear Filenames since input filenames could also be the output of ourself...
  // Filenames.clear();
  ZSpacing = 0;
  if( filenames.empty() )
    {
    Filenames.clear();
    return true;
    }

  Scanner scanner;
  const Tag ipp(0x0020,0x0032); // Image Position (Patient)
  const Tag iop(0x0020,0x0037); // Image Orientation (Patient)
  const Tag frame(0x0020,0x0052); // Frame of Reference UID
  // Temporal Position Identifier (0020,0100) 3 Temporal order of a dynamic or functional set of Images.
  //const Tag tpi(0x0020,0x0100);
  scanner.AddTag( ipp );
  scanner.AddTag( iop );
  bool b = scanner.Scan( filenames );
  if( !b )
    {
    gdcmDebugMacro( "Scanner failed" );
    return false;
    }
  Scanner::ValuesType iops = scanner.GetValues(iop);
  Scanner::ValuesType frames = scanner.GetValues(frame);
  if( iops.size() != 1 )
    {
    gdcmDebugMacro( "More than one IOP (or no IOP): " << iops.size() );
    //std::copy(iops.begin(), iops.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
    return false;
    }
  if( frames.size() > 1 ) // Should I really tolerate no Frame of Reference UID ?
    {
    gdcmDebugMacro( "More than one Frame Of Reference UID" );
    return false;
    }

  const char *reference = filenames[0].c_str();
  Scanner::TagToValue const &t2v = scanner.GetMapping(reference);
  Scanner::TagToValue::const_iterator it = t2v.find( iop );
  // Take the first file in the list of filenames, if not IOP is found, simply gives up:
  if( it == t2v.end() )
    {
    // DEAD CODE
    gdcmDebugMacro( "No iop in: " << reference );
    return false;
    }
  if( it->first != iop )
    {
    // first file does not contains Image Orientation (Patient), let's give up
    gdcmDebugMacro( "No iop in first file ");
    return false;
    }
  const char *dircos = it->second;
  std::stringstream ss;
  ss.str( dircos );
  Element<VR::DS,VM::VM6> cosines;
  cosines.Read( ss );

  // http://www.itk.org/pipermail/insight-users/2003-September/004762.html
  // Compute normal:
  // The steps I take when reconstructing a volume are these: First,
  // calculate the slice normal from IOP:
  double normal[3];
  normal[0] = cosines[1]*cosines[5] - cosines[2]*cosines[4];
  normal[1] = cosines[2]*cosines[3] - cosines[0]*cosines[5];
  normal[2] = cosines[0]*cosines[4] - cosines[1]*cosines[3];

  gdcm::DirectionCosines dc;
  dc.SetFromString( dircos );
  if( !dc.IsValid() ) return false;
  double normal2[3];
  dc.Cross( normal2 );
  assert( normal2[0] == normal[0] &&
          normal2[1] == normal[1] &&
          normal2[2] == normal[2] );
  // You only have to do this once for all slices in the volume. Next, for
  // each slice, calculate the distance along the slice normal using the IPP
  // tag ("dist" is initialized to zero before reading the first slice) :
  //typedef std::multimap<double, const char*> SortedFilenames;
  typedef std::map<double, const char*> SortedFilenames;
  SortedFilenames sorted;
{
  std::vector<std::string>::const_iterator it1 = filenames.begin();
  for(; it1 != filenames.end(); ++it1)
    {
    const char *filename = it1->c_str();
    bool iskey = scanner.IsKey(filename);
    if( iskey )
      {
      const char *value =  scanner.GetValue(filename, ipp);
      if( value )
        {
        //gdcmDebugMacro( filename << " has " << ipp << " = " << value );
        Element<VR::DS,VM::VM3> ipp;
        std::stringstream ss;
        ss.str( value );
        ipp.Read( ss );
        double dist = 0;
        for (int i = 0; i < 3; ++i) dist += normal[i]*ipp[i];
        // FIXME: This test is weak, since implicitely we are doing a != on floating point value
        if( sorted.find(dist) != sorted.end() )
          {
          gdcmDebugMacro( "dist: " << dist << " already found" );
          return false;
          }
        sorted.insert(
          SortedFilenames::value_type(dist,filename) );
        }
      else
        {
        gdcmDebugMacro( "File: " << filename << " has no Tag" << ipp << ". Skipping." );
        }
      }
    else
      {
      gdcmDebugMacro( "File: " << filename << " could not be read. Skipping." );
      }
    }
}
  assert( !sorted.empty() );
{
  SortedFilenames::const_iterator it2 = sorted.begin();
  double prev = it2->first;
  Filenames.push_back( it2->second );
  if( sorted.size() > 1 )
    {
    bool spacingisgood = true;
    ++it2;
    double current = it2->first;
    double zspacing = current - prev;
    for( ; it2 != sorted.end(); ++it2)
      {
      //std::cout << it2->first << " " << it2->second << std::endl;
      current = it2->first;
      Filenames.push_back( it2->second );
      if( fabs((current - prev) - zspacing) > ZTolerance )
        {
        gdcmDebugMacro( "ZTolerance test failed. You need to decrease ZTolerance." );
        spacingisgood = false;
        }
      // update prev for the next for-loop
      prev = current;
      }
    // is spacing good ?
    if( spacingisgood && ComputeZSpacing )
      {
      // If user ask for a ZTolerance of 1e-4, there is no need for us to
      // store the extra digits... this will make sure to return 2.2 from a 2.1999938551239993 value
      const int l = (int)( -log10(ZTolerance) );
      ZSpacing = spacing_round(zspacing, l);
      }
    assert( spacingisgood == false ||  (ZSpacing > ZTolerance && ZTolerance > 0) );
    }
}

  // return true: means sorting succeed, it does not mean spacing computation succeded !
  return true;
}

bool IPPSorter::ComputeSpacing(std::vector<std::string> const & filenames)
{
  (void)filenames;
  return false;
}

} // end namespace gdcm
