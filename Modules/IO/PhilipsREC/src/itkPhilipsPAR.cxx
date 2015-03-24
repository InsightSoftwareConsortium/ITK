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


#include "itkPhilipsPAR.h"
#include <fstream>
#include <iostream>
#include <algorithm>

/**
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */

namespace itk
{
/*#=== IMAGE INFORMATION DEFINITION ============================================
#
#The rest of this file contains ONE line per image, this line contains the
#following information:
#
#slice number                             (integer)
#echo number                              (integer)
#dynamic scan number                      (integer)
#cardiac phase number                     (integer)
#image_type_mr                            (integer)
#scanning sequence                        (integer)
#index in REC file (in images)            (integer)
#rescale intercept                        (float)
#rescale slope                            (float)
#scale slope                              (float)
#window center                            (integer)
#window width                             (integer)
#image angulation (ap,fh,rl in degrees )  (3*float)
#image offcentre (ap,fh,rl in mm )        (3*float)
#image_display_orientation                (integer)
#slice orientation ( TRA/SAG/COR )        (integer)
#fmri_status_indication                   (integer)
#image_type_ed_es  (end diast/end syst)   (integer)
#pixel spacing (x,y) (in mm)              (2*float)
#echo_time                                (float)
#dyn_scan_begin_time                      (float)
#trigger_time                             (float)
#diffusion_b_factor                       (float)
#image_flip_angle (in degrees)            (float)
#
#=== IMAGE INFORMATION =======================================================*/
/**
 * \struct image_info_defV3
 */
struct image_info_defV3 {
  int problemreading;
  int slice;
  int echo;
  int dynamic;
  int cardiac;
  int image_type_mr;
  int scan_sequence;
  int index;
  float rescale_int;
  float rescale_slope;
  float scale_slope;
  int window_center;
  int window_width;
  float angAP;
  float angFH;
  float angRL;
  float offAP;
  float offFH;
  float offRL;
  int display_orientation;
  int slice_orientation;
  int fmri_status_indication;
  int image_type_ed_es;
  float spacingx;
  float spacingy;
  float echo_time;
  float dyn_scan_begin_time;
  float trigger_time;
  float diffusion_b_factor;
  float image_flip_angle;
};

/*#=== IMAGE INFORMATION DEFINITION ============================================
#The rest of this file contains ONE line per image, this line contains the
#following information:
#
#slice number                             (integer)
#echo number                              (integer)
#dynamic scan number                      (integer)
#cardiac phase number                     (integer)
#image_type_mr                            (integer)
#scanning sequence                        (integer)
#index in REC file (in images)            (integer)
#image pixel size (in bits)               (integer)
#scan percentage                          (integer)
#recon resolution (x y)                   (2*integer)
#rescale intercept                        (float)
#rescale slope                            (float)
#scale slope                              (float)
#window center                            (integer)
#window width                             (integer)
#image angulation (ap,fh,rl in degrees )  (3*float)
#image offcentre (ap,fh,rl in mm )        (3*float)
#slice thickness (in mm )                 (float)
#slice gap (in mm )                       (float)
#image_display_orientation                (integer)
#slice orientation ( TRA/SAG/COR )        (integer)
#fmri_status_indication                   (integer)
#image_type_ed_es  (end diast/end syst)   (integer)
#pixel spacing (x,y) (in mm)              (2*float)
#echo_time                                (float)
#dyn_scan_begin_time                      (float)
#trigger_time                             (float)
#diffusion_b_factor                       (float)
#number of averages                       (integer)
#image_flip_angle (in degrees)            (float)
#cardiac frequency   (bpm)                (integer)
#minimum RR-interval (in ms)              (integer)
#maximum RR-interval (in ms)              (integer)
#TURBO factor  <0=no turbo>               (integer)
#Inversion delay (in ms)                  (float)
Version 4.1
#diffusion b value number    (imagekey!)  (integer)
#gradient orientation number (imagekey!)  (integer)
#contrast type                            (string)
#diffusion anisotropy type                (string)
#diffusion (ap, fh, rl)                   (3*float)
Version 4.2
#label type (ASL)            (imagekey!)  (integer)
#
#=== IMAGE INFORMATION =======================================================*/
/**
 * \struct image_info_defV4
 */
struct image_info_defV4 {
  int problemreading;
  int slice;
  int echo;
  int dynamic;
  int cardiac;
  int image_type_mr;
  int scan_sequence;
  int index;
  int image_bits;
  int scan_percent;
  int recon_dimx;
  int recon_dimy;
  float rescale_int;
  float rescale_slope;
  float scale_slope;
  int window_center;
  int window_width;
  float angAP;
  float angFH;
  float angRL;
  float offAP;
  float offFH;
  float offRL;
  float slice_thick;
  float slice_gap;
  int display_orientation;
  int slice_orientation;
  int fmri_status_indication;
  int image_type_ed_es;
  float spacingx;
  float spacingy;
  float echo_time;
  float dyn_scan_begin_time;
  float trigger_time;
  float diffusion_b_factor;
  int num_averages;
  float image_flip_angle;
  int cardiac_freq;
  int min_rr_int;
  int max_rr_int;
  int turbo_factor;
  float inversion_delay;
  // Version 4.1 added diffusion gradient information
  int diffusion_b_value_number;
  int gradient_orientation_number;
  int contrast_type;
  float diffusion_ap;
  float diffusion_fh;
  float diffusion_rl;
  // Version 4.2 added ASL labels
  int labelTypeASL;
};

struct image_info_defV3 GetImageInformationDefinitionV3(std::string file,
                                                        int lineNum, PhilipsPAR *philipsPARClass)
{
  struct image_info_defV3 tempInfo;
  std::string             currentLine = "";

  memset( (void *)&tempInfo, 0, sizeof( struct image_info_defV3 ) );
  if ( lineNum < 89 )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  currentLine = philipsPARClass->GetLineNumber(file, lineNum);
  if ( ( currentLine == "" )
       || ( currentLine == "\n" )
       || ( currentLine == "\r\n" )
       || ( currentLine == "\r" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================\r" ) )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  std::istringstream inString(currentLine);
  if ( !inString )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  inString >> tempInfo.slice >> tempInfo.echo >> tempInfo.dynamic;
  inString >> tempInfo.cardiac >> tempInfo.image_type_mr
  >> tempInfo.scan_sequence;
  inString >> tempInfo.index >> tempInfo.rescale_int >> tempInfo.rescale_slope;
  inString >> tempInfo.scale_slope >> tempInfo.window_center
  >> tempInfo.window_width;
  inString >> tempInfo.angAP >> tempInfo.angFH >> tempInfo.angRL;
  inString >> tempInfo.offAP >> tempInfo.offFH >> tempInfo.offRL;
  inString >> tempInfo.display_orientation >> tempInfo.slice_orientation
  >> tempInfo.fmri_status_indication;
  inString >> tempInfo.image_type_ed_es >> tempInfo.spacingx
  >> tempInfo.spacingy;
  inString >> tempInfo.echo_time >> tempInfo.dyn_scan_begin_time
  >> tempInfo.trigger_time;
  inString >> tempInfo.diffusion_b_factor >> tempInfo.image_flip_angle;
  return tempInfo;
}

struct image_info_defV4 GetImageInformationDefinitionV4(std::string file,
                                                        int lineNum, PhilipsPAR *philipsPARClass)
{
  struct image_info_defV4 tempInfo;
  std::string             currentLine = "";

  memset( (void *)&tempInfo, 0, sizeof( struct image_info_defV4 ) );
  if ( lineNum < 92 )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  currentLine = philipsPARClass->GetLineNumber(file, lineNum);
  if ( ( currentLine == "" )
       || ( currentLine == "\n" )
       || ( currentLine == "\r\n" )
       || ( currentLine == "\r" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================\r" ) )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  std::istringstream inString(currentLine);
  if ( !inString )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  inString >> tempInfo.slice >> tempInfo.echo >> tempInfo.dynamic;
  inString >> tempInfo.cardiac >> tempInfo.image_type_mr
  >> tempInfo.scan_sequence;
  inString >> tempInfo.index >> tempInfo.image_bits >> tempInfo.scan_percent;
  inString >> tempInfo.recon_dimx >> tempInfo.recon_dimy;
  inString >> tempInfo.rescale_int >> tempInfo.rescale_slope;
  inString >> tempInfo.scale_slope >> tempInfo.window_center
  >> tempInfo.window_width;
  inString >> tempInfo.angAP >> tempInfo.angFH >> tempInfo.angRL;
  inString >> tempInfo.offAP >> tempInfo.offFH >> tempInfo.offRL;
  inString >> tempInfo.slice_thick >> tempInfo.slice_gap;
  inString >> tempInfo.display_orientation >> tempInfo.slice_orientation
  >> tempInfo.fmri_status_indication;
  inString >> tempInfo.image_type_ed_es >> tempInfo.spacingx
  >> tempInfo.spacingy;
  inString >> tempInfo.echo_time >> tempInfo.dyn_scan_begin_time
  >> tempInfo.trigger_time;
  inString >> tempInfo.diffusion_b_factor >> tempInfo.num_averages
  >> tempInfo.image_flip_angle;
  inString >> tempInfo.cardiac_freq >> tempInfo.min_rr_int
  >> tempInfo.max_rr_int;
  inString >> tempInfo.turbo_factor >> tempInfo.inversion_delay;
  return tempInfo;
}

struct image_info_defV4 GetImageInformationDefinitionV41(std::string file,
                                                         int lineNum, PhilipsPAR *philipsPARClass)
{
  struct image_info_defV4 tempInfo;
  std::string             currentLine = "";

  memset( (void *)&tempInfo, 0, sizeof( struct image_info_defV4 ) );
  if ( lineNum < 99 )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  currentLine = philipsPARClass->GetLineNumber(file, lineNum);
  if ( ( currentLine == "" )
       || ( currentLine == "\n" )
       || ( currentLine == "\r\n" )
       || ( currentLine == "\r" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================\r" ) )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  std::istringstream inString(currentLine);
  if ( !inString )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  inString >> tempInfo.slice >> tempInfo.echo >> tempInfo.dynamic;
  inString >> tempInfo.cardiac >> tempInfo.image_type_mr
  >> tempInfo.scan_sequence;
  inString >> tempInfo.index >> tempInfo.image_bits >> tempInfo.scan_percent;
  inString >> tempInfo.recon_dimx >> tempInfo.recon_dimy;
  inString >> tempInfo.rescale_int >> tempInfo.rescale_slope;
  inString >> tempInfo.scale_slope >> tempInfo.window_center
  >> tempInfo.window_width;
  inString >> tempInfo.angAP >> tempInfo.angFH >> tempInfo.angRL;
  inString >> tempInfo.offAP >> tempInfo.offFH >> tempInfo.offRL;
  inString >> tempInfo.slice_thick >> tempInfo.slice_gap;
  inString >> tempInfo.display_orientation >> tempInfo.slice_orientation
  >> tempInfo.fmri_status_indication;
  inString >> tempInfo.image_type_ed_es >> tempInfo.spacingx
  >> tempInfo.spacingy;
  inString >> tempInfo.echo_time >> tempInfo.dyn_scan_begin_time
  >> tempInfo.trigger_time;
  inString >> tempInfo.diffusion_b_factor >> tempInfo.num_averages
  >> tempInfo.image_flip_angle;
  inString >> tempInfo.cardiac_freq >> tempInfo.min_rr_int
  >> tempInfo.max_rr_int;
  inString >> tempInfo.turbo_factor >> tempInfo.inversion_delay;
  inString >> tempInfo.diffusion_b_value_number
  >> tempInfo.gradient_orientation_number;
  inString >> tempInfo.contrast_type >> tempInfo.contrast_type;
  inString >> tempInfo.diffusion_ap >> tempInfo.diffusion_fh
  >> tempInfo.diffusion_rl;
  return tempInfo;
}

struct image_info_defV4 GetImageInformationDefinitionV42(std::string file,
                                                         int lineNum, PhilipsPAR *philipsPARClass)
{
  struct image_info_defV4 tempInfo;
  std::string             currentLine = "";

  memset( (void *)&tempInfo, 0, sizeof( struct image_info_defV4 ) );
  if ( lineNum < 101 )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  currentLine = philipsPARClass->GetLineNumber(file, lineNum);
  if ( ( currentLine == "" )
       || ( currentLine == "\n" )
       || ( currentLine == "\r\n" )
       || ( currentLine == "\r" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================" )
       || ( currentLine == "#=== END OF DATA DESCRIPTION FILE ======================"
                           "=========================\r" ) )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  std::istringstream inString(currentLine);
  if ( !inString )
    {
    tempInfo.problemreading = 1;
    return tempInfo;
    }
  inString >> tempInfo.slice >> tempInfo.echo >> tempInfo.dynamic;
  inString >> tempInfo.cardiac >> tempInfo.image_type_mr
  >> tempInfo.scan_sequence;
  inString >> tempInfo.index >> tempInfo.image_bits >> tempInfo.scan_percent;
  inString >> tempInfo.recon_dimx >> tempInfo.recon_dimy;
  inString >> tempInfo.rescale_int >> tempInfo.rescale_slope;
  inString >> tempInfo.scale_slope >> tempInfo.window_center
  >> tempInfo.window_width;
  inString >> tempInfo.angAP >> tempInfo.angFH >> tempInfo.angRL;
  inString >> tempInfo.offAP >> tempInfo.offFH >> tempInfo.offRL;
  inString >> tempInfo.slice_thick >> tempInfo.slice_gap;
  inString >> tempInfo.display_orientation >> tempInfo.slice_orientation
  >> tempInfo.fmri_status_indication;
  inString >> tempInfo.image_type_ed_es >> tempInfo.spacingx
  >> tempInfo.spacingy;
  inString >> tempInfo.echo_time >> tempInfo.dyn_scan_begin_time
  >> tempInfo.trigger_time;
  inString >> tempInfo.diffusion_b_factor >> tempInfo.num_averages
  >> tempInfo.image_flip_angle;
  inString >> tempInfo.cardiac_freq >> tempInfo.min_rr_int
  >> tempInfo.max_rr_int;
  inString >> tempInfo.turbo_factor >> tempInfo.inversion_delay;
  inString >> tempInfo.diffusion_b_value_number
  >> tempInfo.gradient_orientation_number;
  inString >> tempInfo.contrast_type >> tempInfo.contrast_type;
  inString >> tempInfo.diffusion_ap >> tempInfo.diffusion_fh
  >> tempInfo.diffusion_rl;
  inString >> tempInfo.labelTypeASL;
  return tempInfo;
}

PhilipsPAR::PhilipsPAR()
{
  this->m_FileName = "";
  this->m_PARFileLines.resize(0);
}

PhilipsPAR::~PhilipsPAR()
{}

std::string PhilipsPAR::GetLineNumber(std::string file, int lineNum)
{
  std::string line = "";
  char        readFileBuffer[1024] = "";

  if ( lineNum <= 0 )
    {
    return line;
    }

  // If this is the first time, read the whole file into memory.
  if ( file != this->m_FileName )
    {
    this->m_FileName = file;
    this->m_PARFileLines.resize(0);

    // Try to read the text file.
    std::ifstream local_InputStream;
    local_InputStream.open(file.c_str(), std::ios::in);
    if ( local_InputStream.fail() )
      {
      return line;
      }
    while ( !local_InputStream.eof() )
      {
      local_InputStream.getline( readFileBuffer, sizeof( readFileBuffer ) );
      line = readFileBuffer;
      this->m_PARFileLines.push_back(line);
      }
    local_InputStream.close();
    }

  // Return line if not EOF.
  line = "";
  if ( ( std::vector< std::string >::size_type )lineNum
       <= this->m_PARFileLines.size() )
    {
    line = this->m_PARFileLines[lineNum - 1];
    }

  return line;
}

int PhilipsPAR::GetPARVersion(std::string parFile)
{
  //read version number of Philips research tools
  //Research tools are used to extract data from database; data formats differ
  //considerably between versions. Handles V3, V4, V4.1, and V4.2
  int         ResToolsVersion = RESEARCH_IMAGE_EXPORT_TOOL_UNKNOWN;

  // Character index 61 on line 8 should be 'V'.
  std::string currentLine = this->GetLineNumber(parFile, 8);
  if ( ( currentLine.length() >= 63 )
       && ( currentLine[61] == 'V' ) )
    {
    // Next characters contain the version number.
    switch ( currentLine[62] )
      {
      case '3':
        ResToolsVersion = RESEARCH_IMAGE_EXPORT_TOOL_V3;
        break;
      case '4':
        {
        if ( ( currentLine.length() == 63 )
             || ( currentLine[63] ) != '.' )
          {
          ResToolsVersion = RESEARCH_IMAGE_EXPORT_TOOL_V4;
          }
        else if ( currentLine.length() >= 65 )
          {
          switch ( currentLine[64] )
            {
            case '1':
              ResToolsVersion = RESEARCH_IMAGE_EXPORT_TOOL_V4_1;
              break;
            case '2':
              ResToolsVersion = RESEARCH_IMAGE_EXPORT_TOOL_V4_2;
              break;
            }
          }
        }
        break;
      }
    }
  return ResToolsVersion;
}

std::string PhilipsPAR::GetGeneralInfoString(std::string file, int lineNum)
{
  std::string            currentLine = "";
  std::string::size_type index;
  std::string            outString = "";

  if ( ( lineNum < 12 ) || ( lineNum > 51 ) )
    {
    return outString;
    }
  currentLine = this->GetLineNumber(file, lineNum);
  index = currentLine.find(":");
  if ( index != std::string::npos )
    {
    std::string tempString = ":";
    outString = currentLine.substr( index + tempString.length() );
    }
  return outString;
}

#define UNDEFINED "Undefined"

// Originally adapted from r2agui.m
void PhilipsPAR::ReadPAR(std::string parFile, struct par_parameter *pPar)
{
  std::istringstream inString;

  if ( pPar == ITK_NULLPTR )
    {
    std::ostringstream message;
    message << "ReadPAR: pPar == ITK_NULLPTR";
    ExceptionObject exception(__FILE__, __LINE__,
                              message.str(),
                              ITK_LOCATION);
    throw exception;
    }

  // Zero out struct.
  memset( (void *)pPar, 0, sizeof( struct par_parameter ) );
  // Need to set strings to UNDEFINED to avoid segmentation faults.
  strcpy(pPar->patient_name, UNDEFINED);
  strcpy(pPar->exam_name, UNDEFINED);
  strcpy(pPar->protocol_name, UNDEFINED);
  strcpy(pPar->exam_date, UNDEFINED);
  strcpy(pPar->exam_time, UNDEFINED);
  strcpy(pPar->series_type, UNDEFINED);
  strcpy(pPar->patient_position, UNDEFINED);
  strcpy(pPar->prep_direction, UNDEFINED);
  strcpy(pPar->technique, UNDEFINED);
  strcpy(pPar->scan_mode, UNDEFINED);
  // Set image types index to -1.
  memset( (void *)pPar->image_types, -1, sizeof( pPar->image_types ) );
  // Set num_slice_repetitions to 1 to avoid divide by zero.
  pPar->num_slice_repetitions = 1;

  // Get PAR version.
  pPar->ResToolsVersion = this->GetPARVersion(parFile);

  // Parse PAR file according to version.
  switch ( pPar->ResToolsVersion )
    {
    case RESEARCH_IMAGE_EXPORT_TOOL_V3:
      {
      struct image_info_defV3 tempInfo;
      struct image_info_defV3 tempInfo1;
      float                   fovAP, fovFH, fovRL;
      // Start at line 12 and work through PAR file.
      // Line numbers are hard-coded on purpose.
      strncpy( pPar->patient_name, this->GetGeneralInfoString(parFile, 12).c_str(),
               sizeof( pPar->patient_name ) );
      strncpy( pPar->exam_name, this->GetGeneralInfoString(parFile, 13).c_str(),
               sizeof( pPar->exam_name ) );
      strncpy( pPar->protocol_name, this->GetGeneralInfoString(parFile, 14).c_str(),
               sizeof( pPar->protocol_name ) );
      strncpy( pPar->exam_date, this->GetGeneralInfoString(parFile, 15).c_str(),
               this->GetGeneralInfoString(parFile, 15).find("/") );
      strncpy( pPar->exam_time,
               this->GetGeneralInfoString(parFile, 15).substr(
                 this->GetGeneralInfoString(parFile, 15).find("/") + 1).c_str(),
               sizeof( pPar->exam_time ) );
      inString.str( this->GetGeneralInfoString(parFile, 16) );
      inString >> pPar->scno;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 17) );
      inString >> pPar->recno;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 18) );
      inString >> pPar->scan_duration;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 19) );
      inString >> pPar->cardiac_phases;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 20) );
      inString >> pPar->echoes;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 21) );
      inString >> pPar->slice;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 22) );
      inString >> pPar->dyn;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 23) );
      inString >> pPar->mixes;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 24) );
      inString >> pPar->bit;
      inString.clear();
      strncpy( pPar->technique, this->GetGeneralInfoString(parFile, 25).c_str(),
               sizeof( pPar->technique ) );
      strncpy( pPar->scan_mode, this->GetGeneralInfoString(parFile, 26).c_str(),
               sizeof( pPar->scan_mode ) );
      inString.str( this->GetGeneralInfoString(parFile, 27) );
      inString >> pPar->scan_resolution[0];
      inString >> pPar->scan_resolution[1];
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 28) );
      inString >> pPar->scan_percent;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 29) );
      inString >> pPar->dim[0] >> pPar->dim[1];
      pPar->dim[2] = pPar->slice;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 30) );
      inString >> pPar->num_averages;
      inString.clear();
      // It appears that the max number of mixes
      // parameter indicates the number of experiment
      // repititions.  This assumption is based on
      // the T1 mapping images that use the look-locker
      // sequence.
      inString.str( this->GetGeneralInfoString(parFile, 31) );
      for ( int repTime = 0; repTime < pPar->mixes; repTime++ )
        {
        inString >> pPar->repetition_time[repTime];
        }
      inString.clear();
      tempInfo = GetImageInformationDefinitionV3(parFile, 89, this);
      if ( tempInfo.problemreading )
        {
        std::ostringstream message;
        message << "ReadPAR: Problem with GetImageInformationDefinitionV3()";
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      pPar->sliceorient = tempInfo.slice_orientation;
      int echoNumber = tempInfo.echo;
      pPar->echo_times[0] = tempInfo.echo_time;
      int cardiacPhase = tempInfo.cardiac;
      pPar->trigger_times[0] = tempInfo.trigger_time;
      pPar->vox[0] = tempInfo.spacingx;
      pPar->vox[1] = tempInfo.spacingy;
      inString.str( this->GetGeneralInfoString(parFile, 32) );
      inString >> fovAP >> fovFH >> fovRL;
      inString.clear();
      // slice orientation: transversal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_TRANSVERSAL )
        {
        pPar->fov[0] = fovAP;
        pPar->fov[1] = fovRL;
        }
      // slice orientation: sagittal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_SAGITTAL )
        {
        pPar->fov[0] = fovFH;
        pPar->fov[1] = fovAP;
        }
      // slice orientation: coronal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_CORONAL )
        {
        pPar->fov[0] = fovRL;
        pPar->fov[1] = fovFH;
        }
      inString.str( this->GetGeneralInfoString(parFile, 33) );
      inString >> pPar->slth;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 34) );
      inString >> pPar->gap;
      inString.clear();
      pPar->fov[2] = ( pPar->gap + pPar->slth ) * pPar->slice;
      pPar->vox[2] = pPar->slth + pPar->gap;
      inString.str( this->GetGeneralInfoString(parFile, 35) );
      inString >> pPar->water_fat_shift;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 36) );
      inString >> pPar->angAP;
      inString >> pPar->angFH;
      inString >> pPar->angRL;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 37) );
      inString >> pPar->offAP;
      inString >> pPar->offFH;
      inString >> pPar->offRL;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 38) );
      inString >> pPar->flow_comp;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 39) );
      inString >> pPar->presaturation;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 40) );
      inString >> pPar->cardiac_freq;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 41) );
      inString >> pPar->min_rr_int;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 42) );
      inString >> pPar->max_rr_int;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 43) );
      inString >> pPar->phase_encode_vel[0];
      inString >> pPar->phase_encode_vel[1];
      inString >> pPar->phase_encode_vel[2];
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 44) );
      inString >> pPar->mtc;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 45) );
      inString >> pPar->spir;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 46) );
      inString >> pPar->epi;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 47) );
      inString >> pPar->turbo;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 48) );
      inString >> pPar->dynamic_scan;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 49) );
      inString >> pPar->diffusion;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 50) );
      inString >> pPar->diff_echo;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 51) );
      inString >> pPar->inversion_delay;
      inString.clear();
      // OK, need to figure out how many images are stored in the REC file
      // and whether or not the images are sorted by slice or by image blocks.
      // Also get echo times and trigger_times.
      if ( pPar->slice > 1 )
        {
        int lineIncrement = 89;
        int echoIndex = 0;
        int cardiacIndex = 0;
        tempInfo1 = GetImageInformationDefinitionV3(parFile, 90, this);
        if ( tempInfo1.problemreading )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: Problem with GetImageInformationDefinitionV3()";
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        if ( ( tempInfo1.slice - tempInfo.slice ) > 0 )
          {
          pPar->slicessorted = 1;
          }
        // If slices are sorted I only need to calculate the number of
        // image blocks (if more than 1) and store the echo times.
        if ( pPar->slicessorted )
          {
          ++pPar->image_blocks;
          ++pPar->num_image_types;
          pPar->image_types[0] = tempInfo.image_type_mr;
          ++pPar->num_scanning_sequences;
          pPar->scanning_sequences[0] = tempInfo.scan_sequence;
          lineIncrement += pPar->slice;
          tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
          while ( !tempInfo1.problemreading && tempInfo1.slice )
            {
            int isUnique = 1;
            // Find unique image types in REC.
            for ( int i = 0; i < pPar->num_image_types; i++ )
              {
              if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_image_types;
              pPar->image_types[pPar->num_image_types - 1] =
                tempInfo1.image_type_mr;
              }
            isUnique = 1;
            // Find all of the scanning sequences.
            for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
              {
              if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_scanning_sequences;
              pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                tempInfo1.scan_sequence;
              }
            ++pPar->image_blocks;
            lineIncrement += pPar->slice;
            // Get the echo times.
            if ( echoNumber != tempInfo1.echo )
              {
              ++echoIndex;
              pPar->echo_times[echoIndex] = tempInfo1.echo_time;
              echoNumber = tempInfo1.echo;
              }
            // Get the trigger times
            if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
              (cardiacPhase != tempInfo1.cardiac) )
              {
              ++cardiacIndex;
              pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
              cardiacPhase = tempInfo1.cardiac;
              }
            tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
            }
          }
        // Slices are not sorted.
        else
          {
          int slice = tempInfo.slice;
          ++pPar->image_blocks;
          ++pPar->num_image_types;
          pPar->image_types[0] = tempInfo.image_type_mr;
          ++pPar->num_scanning_sequences;
          pPar->scanning_sequences[0] = tempInfo.scan_sequence;
          ++lineIncrement;
          tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
          while ( !tempInfo1.problemreading && tempInfo1.slice )
            {
            if ( slice == tempInfo1.slice )
              {
              int isUnique = 1;
              // Find unique image types in REC.
              for ( int i = 0; i < pPar->num_image_types; i++ )
                {
                if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                  {
                  isUnique = 0;
                  break;
                  }
                }
              if ( isUnique )
                {
                ++pPar->num_image_types;
                pPar->image_types[pPar->num_image_types - 1] =
                  tempInfo1.image_type_mr;
                }
              isUnique = 1;
              // Find all of the scanning sequences.
              for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
                {
                if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                  {
                  isUnique = 0;
                  break;
                  }
                }
              if ( isUnique )
                {
                ++pPar->num_scanning_sequences;
                pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                  tempInfo1.scan_sequence;
                }
              ++pPar->image_blocks;
              // Get the echo times.
              if ( echoNumber != tempInfo1.echo )
                {
                ++echoIndex;
                pPar->echo_times[echoIndex] = tempInfo1.echo_time;
                echoNumber = tempInfo1.echo;
                }
              // Get the trigger times
              if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
                (cardiacPhase != tempInfo1.cardiac) )
                {
                ++cardiacIndex;
                pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
                cardiacPhase = tempInfo1.cardiac;
                }
              }
            else
              {
              lineIncrement = 89;
              // OK, I need to determine if there are more image blocks, only
              // if pPar->num_image_types or pPar->num_scanning_sequences > 1
              if ( ( pPar->num_image_types > 1 )
                   || ( pPar->num_scanning_sequences > 1 ) )
                {
                pPar->num_slice_repetitions = pPar->image_blocks;
                lineIncrement += ( pPar->slice * pPar->num_slice_repetitions );
                tempInfo1 = GetImageInformationDefinitionV3(parFile,
                                                            lineIncrement, this);
                while ( !tempInfo1.problemreading && tempInfo1.slice )
                  {
                  // Get the echo times.
                  if ( echoNumber != tempInfo1.echo )
                    {
                    ++echoIndex;
                    pPar->echo_times[echoIndex] = tempInfo1.echo_time;
                    echoNumber = tempInfo1.echo;
                    }
                  // Get the trigger times
                  if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
                    (cardiacPhase != tempInfo1.cardiac) )
                    {
                    ++cardiacIndex;
                    pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
                    cardiacPhase = tempInfo1.cardiac;
                    }
                  pPar->image_blocks += pPar->num_slice_repetitions;
                  lineIncrement += ( pPar->slice * pPar->num_slice_repetitions );
                  tempInfo1 = GetImageInformationDefinitionV3(parFile,
                                                              lineIncrement, this);
                  }
                }
              break;
              }
            ++lineIncrement;
            tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
            }
          }
        // This is a sanity check.  The echoIndex should match
        // (pPar->echoes-1).
        if ( ( pPar->echoes - 1 ) != echoIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->echoes-1) != echoIndex, "
                  << "pPar->echoes-1 = " << pPar->echoes - 1
                  << " and echoIndex = " << echoIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        // Another sanity check.  The cardiacIndex should match
        // (pPar->cardiac_phases-1).
        if ( ( pPar->cardiac_phases - 1 ) != cardiacIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->cardiac_phases-1) != cardiacIndex, "
                  << "pPar->cardiac_phases-1 = " << pPar->cardiac_phases - 1
                  << " and cardiacIndex = " << cardiacIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      // Only 1 slice, but how many repetitions of that slice?
      else
        {
        int lineIncrement = 89;
        int echoIndex = 0;
        int cardiacIndex = 0;
        int slice = tempInfo.slice;
        int firstEchoNumber = echoNumber;
        int firstCardiacPhase = cardiacPhase;
        int firstDynamic = tempInfo.dynamic;
        ++pPar->image_blocks;
        ++pPar->num_image_types;
        pPar->image_types[0] = tempInfo.image_type_mr;
        ++pPar->num_scanning_sequences;
        pPar->scanning_sequences[0] = tempInfo.scan_sequence;
        ++lineIncrement;
        tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
        while ( !tempInfo1.problemreading && tempInfo1.slice )
          {
          if ( slice == tempInfo1.slice )
            {
            int isUnique = 1;
            // Find unique image types in REC.
            for ( int i = 0; i < pPar->num_image_types; i++ )
              {
              if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_image_types;
              pPar->image_types[pPar->num_image_types - 1] =
                tempInfo1.image_type_mr;
              }
            isUnique = 1;
            // Find all of the scanning sequences.
            for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
              {
              if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_scanning_sequences;
              pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                tempInfo1.scan_sequence;
              }
            ++pPar->image_blocks;
            // Get the echo times.
            if ( echoNumber != tempInfo1.echo )
              {
              ++echoIndex;
              pPar->echo_times[echoIndex] = tempInfo1.echo_time;
              echoNumber = tempInfo1.echo;
              }
            // Get the trigger times
            if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
              (cardiacPhase != tempInfo1.cardiac) )
              {
              ++cardiacIndex;
              pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
              cardiacPhase = tempInfo1.cardiac;
              }
            // Need to keep track of the number of consecutive slice
            // repetitions.
            if ( ( pPar->echoes > 1 ) && ( firstEchoNumber == tempInfo1.echo ) )
              {
              ++pPar->num_slice_repetitions;
              }
            if ( ( pPar->cardiac_phases > 1 )
                 && ( firstCardiacPhase == tempInfo1.cardiac ) )
              {
              ++pPar->num_slice_repetitions;
              }
            if ( ( pPar->dyn > 1 ) && ( firstDynamic == tempInfo1.dynamic ) )
              {
              ++pPar->num_slice_repetitions;
              }
            }
          else
            {
            break;
            }
          ++lineIncrement;
          tempInfo1 = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
          }
        // This is a sanity check.  The echoIndex should match
        // (pPar->echoes-1).
        if ( ( pPar->echoes - 1 ) != echoIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->echoes-1) != echoIndex, "
                  << "pPar->echoes-1 = " << pPar->echoes - 1
                  << " and echoIndex = " << echoIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        // Another sanity check.  The cardiacIndex should match
        // (pPar->cardiac_phases-1).
        if ( ( pPar->cardiac_phases - 1 ) != cardiacIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->cardiac_phases-1) != cardiacIndex, "
                  << "pPar->cardiac_phases-1 = " << pPar->cardiac_phases - 1
                  << " and cardiacIndex = " << cardiacIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      }
      break;

    case RESEARCH_IMAGE_EXPORT_TOOL_V4:
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
      {
      struct image_info_defV4 tempInfo;
      struct image_info_defV4 tempInfo1;
      float                   fovAP, fovFH, fovRL;
      // Start at line 12 and work through PAR file.
      // Line numbers are hard-coded on purpose.
      strncpy( pPar->patient_name, this->GetGeneralInfoString(parFile, 12).c_str(),
               sizeof( pPar->patient_name ) );
      strncpy( pPar->exam_name, this->GetGeneralInfoString(parFile, 13).c_str(),
               sizeof( pPar->exam_name ) );
      strncpy( pPar->protocol_name, this->GetGeneralInfoString(parFile, 14).c_str(),
               sizeof( pPar->protocol_name ) );
      strncpy( pPar->exam_date,
               this->GetGeneralInfoString(parFile, 15).c_str(),
               this->GetGeneralInfoString(parFile, 15).find("/") );
      strncpy( pPar->exam_time,
               this->GetGeneralInfoString(parFile, 15).substr(
                 this->GetGeneralInfoString(parFile, 15).find("/") + 1).c_str(),
               sizeof( pPar->exam_time ) );
      strncpy( pPar->series_type, this->GetGeneralInfoString(parFile, 16).c_str(),
               sizeof( pPar->series_type ) );
      inString.str( this->GetGeneralInfoString(parFile, 17) );
      inString >> pPar->scno;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 18) );
      inString >> pPar->recno;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 19) );
      inString >> pPar->scan_duration;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 20) );
      inString >> pPar->cardiac_phases;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 21) );
      inString >> pPar->echoes;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 22) );
      inString >> pPar->slice;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 23) );
      inString >> pPar->dyn;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 24) );
      inString >> pPar->mixes;
      inString.clear();
      strncpy( pPar->patient_position, this->GetGeneralInfoString(parFile, 25).c_str(),
               sizeof( pPar->patient_position ) );
      strncpy( pPar->prep_direction, this->GetGeneralInfoString(parFile, 26).c_str(),
               sizeof( pPar->prep_direction ) );
      strncpy( pPar->technique, this->GetGeneralInfoString(parFile, 27).c_str(),
               sizeof( pPar->technique ) );
      inString.str( this->GetGeneralInfoString(parFile, 28) );
      inString >> pPar->scan_resolution[0];
      inString >> pPar->scan_resolution[1];
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 29) );
      inString >> pPar->scan_mode;
      inString.clear();
      // It appears that the max number of mixes
      // parameter indicates the number of experiment
      // repititions.  This assumption is based on
      // the T1 mapping images that use the look-locker
      // sequence.
      inString.str( this->GetGeneralInfoString(parFile, 30) );
      for ( int repTime = 0; repTime < pPar->mixes; repTime++ )
        {
        inString >> pPar->repetition_time[repTime];
        }
      inString.clear();
      switch ( pPar->ResToolsVersion )
        {
        case RESEARCH_IMAGE_EXPORT_TOOL_V4:
          tempInfo = GetImageInformationDefinitionV4(parFile, 92, this);
          break;
        case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
          tempInfo = GetImageInformationDefinitionV41(parFile, 99, this);
          break;
        case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
          tempInfo = GetImageInformationDefinitionV42(parFile, 101, this);
          break;
        }
      if ( tempInfo.problemreading )
        {
        pPar->problemreading = 1;
        std::ostringstream message;
        message << "ReadPAR: Problem with GetImageInformationDefinitionV4()";
        ExceptionObject exception(__FILE__, __LINE__,
                                  message.str(),
                                  ITK_LOCATION);
        throw exception;
        }
      pPar->sliceorient = tempInfo.slice_orientation;
      int echoNumber = tempInfo.echo;
      pPar->echo_times[0] = tempInfo.echo_time;
      int cardiacPhase = tempInfo.cardiac;
      pPar->trigger_times[0] = tempInfo.trigger_time;
      pPar->dim[0] = tempInfo.recon_dimx;
      pPar->dim[1] = tempInfo.recon_dimy;
      pPar->dim[2] = pPar->slice;
      pPar->bit = tempInfo.image_bits;
      pPar->slth = tempInfo.slice_thick;
      pPar->gap = tempInfo.slice_gap;
      pPar->vox[0] = tempInfo.spacingx;
      pPar->vox[1] = tempInfo.spacingy;
      pPar->vox[2] = tempInfo.slice_thick + tempInfo.slice_gap;
      inString.str( this->GetGeneralInfoString(parFile, 31) );
      inString >> fovAP >> fovFH >> fovRL;
      inString.clear();
      // slice orientation: transversal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_TRANSVERSAL )
        {
        pPar->fov[0] = fovAP;
        pPar->fov[1] = fovRL;
        }
      // slice orientation: sagittal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_SAGITTAL )
        {
        pPar->fov[0] = fovFH;
        pPar->fov[1] = fovAP;
        }
      // slice orientation: coronal
      if ( pPar->sliceorient == PAR_SLICE_ORIENTATION_CORONAL )
        {
        pPar->fov[0] = fovRL;
        pPar->fov[1] = fovFH;
        }
      pPar->fov[2] = ( pPar->gap + pPar->slth ) * pPar->slice;
      inString.str( this->GetGeneralInfoString(parFile, 32) );
      inString >> pPar->water_fat_shift;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 33) );
      inString >> pPar->angAP;
      inString >> pPar->angFH;
      inString >> pPar->angRL;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 34) );
      inString >> pPar->offAP;
      inString >> pPar->offFH;
      inString >> pPar->offRL;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 35) );
      inString >> pPar->flow_comp;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 36) );
      inString >> pPar->presaturation;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 37) );
      inString >> pPar->phase_encode_vel[0];
      inString >> pPar->phase_encode_vel[1];
      inString >> pPar->phase_encode_vel[2];
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 38) );
      inString >> pPar->mtc;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 39) );
      inString >> pPar->spir;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 40) );
      inString >> pPar->epi;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 41) );
      inString >> pPar->dynamic_scan;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 42) );
      inString >> pPar->diffusion;
      inString.clear();
      inString.str( this->GetGeneralInfoString(parFile, 43) );
      inString >> pPar->diff_echo;
      inString.clear();
      // Versions >= 4.1
      if ( pPar->ResToolsVersion >= RESEARCH_IMAGE_EXPORT_TOOL_V4_1 )
        {
        inString.str( this->GetGeneralInfoString(parFile, 44) );
        inString >> pPar->max_num_diff_vals;
        inString.clear();
        inString.str( this->GetGeneralInfoString(parFile, 45) );
        inString >> pPar->max_num_grad_orient;
        inString.clear();
        }
      // Version 4.2 only
      if ( pPar->ResToolsVersion == RESEARCH_IMAGE_EXPORT_TOOL_V4_2 )
        {
        inString.str( this->GetGeneralInfoString(parFile, 46) );
        inString >> pPar->num_label_types;
        inString.clear();
        }
      // OK, need to figure out how many images are stored in the REC file
      // and whether or not the images are sorted by slice or by image blocks.
      // Also get echo times.
      if ( pPar->slice > 1 )
        {
        int lineIncrement = 92;
        int echoIndex = 0;
        int cardiacIndex = 0;
        switch ( pPar->ResToolsVersion )
          {
          case RESEARCH_IMAGE_EXPORT_TOOL_V4:
            tempInfo1 = GetImageInformationDefinitionV4(parFile, 93, this);
            break;
          case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
            lineIncrement = 99;
            tempInfo1 = GetImageInformationDefinitionV41(parFile, 100, this);
            break;
          case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
            lineIncrement = 101;
            tempInfo1 = GetImageInformationDefinitionV42(parFile, 102, this);
            break;
          }
        if ( tempInfo1.problemreading )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: Problem with GetImageInformationV4()";
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        if ( ( tempInfo1.slice - tempInfo.slice ) > 0 )
          {
          pPar->slicessorted = 1;
          }
        // If slices are sorted I only need to calculate the number of
        // image blocks (if more than 1) and store the echo times.
        if ( pPar->slicessorted )
          {
          ++pPar->image_blocks;
          ++pPar->num_image_types;
          pPar->image_types[0] = tempInfo.image_type_mr;
          ++pPar->num_scanning_sequences;
          pPar->scanning_sequences[0] = tempInfo.scan_sequence;
          lineIncrement += pPar->slice;
          // lineIncrement is set according to version 4.x PAR file.
          tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
          while ( !tempInfo1.problemreading && tempInfo1.slice )
            {
            int isUnique = 1;
            // Find unique image types in REC.
            for ( int i = 0; i < pPar->num_image_types; i++ )
              {
              if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_image_types;
              pPar->image_types[pPar->num_image_types - 1] =
                tempInfo1.image_type_mr;
              }
            isUnique = 1;
            // Find all of the scanning sequences.
            for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
              {
              if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_scanning_sequences;
              pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                tempInfo1.scan_sequence;
              }
            ++pPar->image_blocks;
            // Get the echo times.
            if ( echoNumber != tempInfo1.echo )
              {
              ++echoIndex;
              pPar->echo_times[echoIndex] = tempInfo1.echo_time;
              echoNumber = tempInfo1.echo;
              }
            // Get the trigger times
            if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
              (cardiacPhase != tempInfo1.cardiac) )
              {
              ++cardiacIndex;
              pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
              cardiacPhase = tempInfo1.cardiac;
              }
            lineIncrement += pPar->slice;
            // lineIncrement is set according to version 4.x PAR file.
            tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
            }
          }
        // Slices are not sorted.
        else
          {
          int slice = tempInfo.slice;
          ++pPar->image_blocks;
          ++pPar->num_image_types;
          pPar->image_types[0] = tempInfo.image_type_mr;
          ++pPar->num_scanning_sequences;
          pPar->scanning_sequences[0] = tempInfo.scan_sequence;
          ++lineIncrement;
          // lineIncrement is set according to version 4.x PAR file.
          tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
          while ( !tempInfo1.problemreading && tempInfo1.slice )
            {
            // This if statement applies to just the first slice.
            if ( slice == tempInfo1.slice )
              {
              // Find unique image types in REC.
              int isUnique = 1;
              for ( int i = 0; i < pPar->num_image_types; i++ )
                {
                if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                  {
                  isUnique = 0;
                  break;
                  }
                }
              if ( isUnique )
                {
                ++pPar->num_image_types;
                pPar->image_types[pPar->num_image_types - 1] =
                  tempInfo1.image_type_mr;
                }
              isUnique = 1;
              // Find all of the scanning sequences.
              for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
                {
                if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                  {
                  isUnique = 0;
                  break;
                  }
                }
              if ( isUnique )
                {
                ++pPar->num_scanning_sequences;
                pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                  tempInfo1.scan_sequence;
                }
              ++pPar->image_blocks;
              // Get the echo times.
              if ( echoNumber != tempInfo1.echo )
                {
                ++echoIndex;
                pPar->echo_times[echoIndex] = tempInfo1.echo_time;
                echoNumber = tempInfo1.echo;
                }
              // Get the trigger times
              if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
                (cardiacPhase != tempInfo1.cardiac) )
                {
                ++cardiacIndex;
                pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
                cardiacPhase = tempInfo1.cardiac;
                }
              }
            // Now we have sufficient information to parse the rest of the PAR
            // file.
            else
              {
              switch ( pPar->ResToolsVersion )
                {
                case RESEARCH_IMAGE_EXPORT_TOOL_V4:
                  lineIncrement = 92;
                  break;
                case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
                  lineIncrement = 99;
                  break;
                case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
                  lineIncrement = 101;
                  break;
                }
              // OK, I need to determine if there are more image blocks, only
              // if pPar->num_image_types or pPar->num_scanning_sequences > 1
              if ( ( pPar->num_image_types > 1 )
                   || ( pPar->num_scanning_sequences > 1 ) )
                {
                pPar->num_slice_repetitions = pPar->image_blocks;
                lineIncrement += ( pPar->slice * pPar->num_slice_repetitions );
                // lineIncrement is set according to version 4.x PAR file.
                tempInfo1 = GetImageInformationDefinitionV4(parFile,
                                                            lineIncrement, this);
                while ( !tempInfo1.problemreading && tempInfo1.slice )
                  {
                  // Get the echo times.
                  if ( echoNumber != tempInfo1.echo )
                    {
                    ++echoIndex;
                    pPar->echo_times[echoIndex] = tempInfo1.echo_time;
                    echoNumber = tempInfo1.echo;
                    }
                  // Get the trigger times
                  if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
                    (cardiacPhase != tempInfo1.cardiac) )
                    {
                    ++cardiacIndex;
                    pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
                    cardiacPhase = tempInfo1.cardiac;
                    }
                  pPar->image_blocks += pPar->num_slice_repetitions;
                  lineIncrement += ( pPar->slice * pPar->num_slice_repetitions );
                  tempInfo1 = GetImageInformationDefinitionV4(parFile,
                                                              lineIncrement, this);
                  }
                }
              break;
              }
            ++lineIncrement;
            tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
            }
          }
        // This is a sanity check.  The echoIndex should match
        // (pPar->echoes-1).
        if ( ( pPar->echoes - 1 ) != echoIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->echoes-1) != echoIndex, "
                  << "pPar->echoes-1 = " << pPar->echoes - 1
                  << " and echoIndex = " << echoIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        // Another sanity check.  The cardiacIndex should match
        // (pPar->cardiac_phases-1).
        if ( ( pPar->cardiac_phases - 1 ) != cardiacIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->cardiac_phases-1) != cardiacIndex, "
                  << "pPar->cardiac_phases-1 = " << pPar->cardiac_phases - 1
                  << " and cardiacIndex = " << cardiacIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      // Only 1 slice, but how many repetitions of that slice?
      else
        {
        int lineIncrement = 92;
        int echoIndex = 0;
        int cardiacIndex = 0;
        int slice = tempInfo.slice;
        int firstEchoNumber = echoNumber;
        int firstCardiacPhase = cardiacPhase;
        int firstDynamic = tempInfo.dynamic;
        ++pPar->image_blocks;
        ++pPar->num_image_types;
        pPar->image_types[0] = tempInfo.image_type_mr;
        ++pPar->num_scanning_sequences;
        pPar->scanning_sequences[0] = tempInfo.scan_sequence;
        switch ( pPar->ResToolsVersion )
          {
          case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
            lineIncrement = 99;
            break;
          case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
            lineIncrement = 101;
            break;
          }
        ++lineIncrement;
        tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
        while ( !tempInfo1.problemreading && tempInfo1.slice )
          {
          if ( slice == tempInfo1.slice )
            {
            int isUnique = 1;
            // Find unique image types in REC.
            for ( int i = 0; i < pPar->num_image_types; i++ )
              {
              if ( pPar->image_types[i] == tempInfo1.image_type_mr )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_image_types;
              pPar->image_types[pPar->num_image_types - 1] =
                tempInfo1.image_type_mr;
              }
            isUnique = 1;
            // Find all of the scanning sequences.
            for ( int i = 0; i < pPar->num_scanning_sequences; i++ )
              {
              if ( pPar->scanning_sequences[i] == tempInfo1.scan_sequence )
                {
                isUnique = 0;
                break;
                }
              }
            if ( isUnique )
              {
              ++pPar->num_scanning_sequences;
              pPar->scanning_sequences[pPar->num_scanning_sequences - 1] =
                tempInfo1.scan_sequence;
              }
            ++pPar->image_blocks;
            // Should be equal after the first iteration, but will only
            // add additional echoes in latter iterations if they differ
            // from the first.
            if ( echoNumber != tempInfo1.echo )
              {
              ++echoIndex;
              pPar->echo_times[echoIndex] = tempInfo1.echo_time;
              echoNumber = tempInfo1.echo;
              }
            // Get the trigger times
            if ( (cardiacIndex < (pPar->cardiac_phases-1)) &&
              (cardiacPhase != tempInfo1.cardiac) )
              {
              ++cardiacIndex;
              pPar->trigger_times[cardiacIndex] = tempInfo1.trigger_time;
              cardiacPhase = tempInfo1.cardiac;
              }
            // Need to keep track of the number of consecutive slice
            // repetitions.
            if ( ( pPar->echoes > 1 ) && ( firstEchoNumber == tempInfo1.echo ) )
              {
              ++pPar->num_slice_repetitions;
              }
            if ( ( pPar->cardiac_phases > 1 )
                 && ( firstCardiacPhase == tempInfo1.cardiac ) )
              {
              ++pPar->num_slice_repetitions;
              }
            if ( ( pPar->dyn > 1 ) && ( firstDynamic == tempInfo1.dynamic ) )
              {
              ++pPar->num_slice_repetitions;
              }
            }
          else
            {
            break;
            }
          ++lineIncrement;
          tempInfo1 = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
          }
        // This is a sanity check.  The echoIndex should match
        // (pPar->echoes-1).
        if ( ( pPar->echoes - 1 ) != echoIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->echoes-1) != echoIndex, "
                  << "pPar->echoes-1 = " << pPar->echoes - 1
                  << " and echoIndex = " << echoIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        // Another sanity check.  The cardiacIndex should match
        // (pPar->cardiac_phases-1).
        if ( ( pPar->cardiac_phases - 1 ) != cardiacIndex )
          {
          pPar->problemreading = 1;
          std::ostringstream message;
          message << "ReadPAR: (pPar->cardiac_phases-1) != cardiacIndex, "
                  << "pPar->cardiac_phases-1 = " << pPar->cardiac_phases - 1
                  << " and cardiacIndex = " << cardiacIndex;
          ExceptionObject exception(__FILE__, __LINE__,
                                    message.str(),
                                    ITK_LOCATION);
          throw exception;
          }
        }
      }
      break;

    default:
      {
      pPar->problemreading = 1;
      std::ostringstream message;
      message << "ReadPAR: Unknown PAR version";
      ExceptionObject exception(__FILE__, __LINE__,
                                message.str(),
                                ITK_LOCATION);
      throw exception;
      }
    }

  // Reorder the image type matrix so that it is least to greatest.
  // Only if the slices need sorting.
  if ( !pPar->slicessorted )
    {
    std::vector< int > sortedImageTypes;
    for ( int j = 0; j < pPar->num_image_types; j++ )
      {
      sortedImageTypes.push_back(pPar->image_types[j]);
      }

    std::sort( sortedImageTypes.begin(), sortedImageTypes.end() );
    for ( int k = 0; k < pPar->num_image_types; k++ )
      {
      pPar->image_types[k] = sortedImageTypes[k];
      }

    // Reorder the scanning sequences matrix so that it is least to greatest.
    std::vector< int > sortedScanningSequences;
    for ( int l = 0; l < pPar->num_scanning_sequences; l++ )
      {
      sortedScanningSequences.push_back(pPar->scanning_sequences[l]);
      }

    std::sort( sortedScanningSequences.begin(), sortedScanningSequences.end() );
    for ( int m = 0; m < pPar->num_scanning_sequences; m++ )
      {
      pPar->scanning_sequences[m] = sortedScanningSequences[m];
      }
    }

  // This is a final fixup that will report the total z dimension
  // as the product of the #of slices and the number of image blocks.
  pPar->dim[2] *= pPar->image_blocks;
}

PhilipsPAR::PARSliceIndexImageTypeVector
PhilipsPAR::GetRECSliceIndexImageTypes(std::string parFile)
{
  PhilipsPAR::PARSliceIndexImageTypeVector recSliceIndexImageTypes;
  int                                      ResToolsVersion;

  // Check version of PAR file.
  ResToolsVersion = this->GetPARVersion(parFile);
  if ( ResToolsVersion == RESEARCH_IMAGE_EXPORT_TOOL_UNKNOWN )
    {
    return recSliceIndexImageTypes;
    }

  switch ( ResToolsVersion )
    {
    case RESEARCH_IMAGE_EXPORT_TOOL_V3:
      {
      struct image_info_defV3            tempInfo;
      PhilipsPAR::PARSliceIndexImageType sliceAndType;
      int                                lineIncrement = 89;
      tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndType.first = tempInfo.slice;
        sliceAndType.second = tempInfo.image_type_mr;
        recSliceIndexImageTypes.push_back(sliceAndType);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4:
      {
      struct image_info_defV4            tempInfo;
      PhilipsPAR::PARSliceIndexImageType sliceAndType;
      int                                lineIncrement = 92;
      tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndType.first = tempInfo.slice;
        sliceAndType.second = tempInfo.image_type_mr;
        recSliceIndexImageTypes.push_back(sliceAndType);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
      {
      struct image_info_defV4            tempInfo;
      PhilipsPAR::PARSliceIndexImageType sliceAndType;
      int                                lineIncrement = 99;
      tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndType.first = tempInfo.slice;
        sliceAndType.second = tempInfo.image_type_mr;
        recSliceIndexImageTypes.push_back(sliceAndType);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
      {
      struct image_info_defV4            tempInfo;
      PhilipsPAR::PARSliceIndexImageType sliceAndType;
      int                                lineIncrement = 101;
      tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndType.first = tempInfo.slice;
        sliceAndType.second = tempInfo.image_type_mr;
        recSliceIndexImageTypes.push_back(sliceAndType);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
        }
      }
      break;
    }
  return recSliceIndexImageTypes;
}

PhilipsPAR::PARSliceIndexScanSequenceVector
PhilipsPAR::GetRECSliceIndexScanningSequence(std::string parFile)
{
  PhilipsPAR::PARSliceIndexScanSequenceVector recSliceIndexScanSequence;
  int                                         ResToolsVersion;

  // Check version of PAR file.
  ResToolsVersion = this->GetPARVersion(parFile);
  if ( ResToolsVersion == RESEARCH_IMAGE_EXPORT_TOOL_UNKNOWN )
    {
    return recSliceIndexScanSequence;
    }

  switch ( ResToolsVersion )
    {
    case RESEARCH_IMAGE_EXPORT_TOOL_V3:
      {
      struct image_info_defV3               tempInfo;
      PhilipsPAR::PARSliceIndexScanSequence sliceAndSequence;
      int                                   lineIncrement = 89;
      tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndSequence.first = tempInfo.slice;
        sliceAndSequence.second = tempInfo.scan_sequence;
        recSliceIndexScanSequence.push_back(sliceAndSequence);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4:
      {
      struct image_info_defV4               tempInfo;
      PhilipsPAR::PARSliceIndexScanSequence sliceAndSequence;
      int                                   lineIncrement = 92;
      tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndSequence.first = tempInfo.slice;
        sliceAndSequence.second = tempInfo.scan_sequence;
        recSliceIndexScanSequence.push_back(sliceAndSequence);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
      {
      struct image_info_defV4               tempInfo;
      PhilipsPAR::PARSliceIndexScanSequence sliceAndSequence;
      int                                   lineIncrement = 99;
      tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndSequence.first = tempInfo.slice;
        sliceAndSequence.second = tempInfo.scan_sequence;
        recSliceIndexScanSequence.push_back(sliceAndSequence);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
      {
      struct image_info_defV4               tempInfo;
      PhilipsPAR::PARSliceIndexScanSequence sliceAndSequence;
      int                                   lineIncrement = 101;
      tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        sliceAndSequence.first = tempInfo.slice;
        sliceAndSequence.second = tempInfo.scan_sequence;
        recSliceIndexScanSequence.push_back(sliceAndSequence);
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
        }
      }
      break;
    }
  return recSliceIndexScanSequence;
}

PhilipsPAR::PARImageTypeScanSequenceVector
PhilipsPAR::GetImageTypesScanningSequence(std::string parFile)
{
  PhilipsPAR::PARImageTypeScanSequenceVector recImageTypesScanSequence;
  struct par_parameter                       parParam;

  // Read the PAR file.
  try
    {
    this->ReadPAR(parFile, &parParam);
    }
  catch ( ExceptionObject & )
    {
    return recImageTypesScanSequence;
    }

  switch ( parParam.ResToolsVersion )
    {
    case RESEARCH_IMAGE_EXPORT_TOOL_V3:
      {
      struct image_info_defV3 tempInfo;
      for ( int scanIndex = 0; scanIndex < parParam.num_scanning_sequences;
            scanIndex++ )
        {
        PhilipsPAR::PARImageTypeScanSequence imageTypeAndSequence;
        int                                  lineIncrement = 89;
        int                                  imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
        tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
        while ( !tempInfo.problemreading && tempInfo.slice )
          {
          if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
               && ( tempInfo.scan_sequence == *( parParam.scanning_sequences + scanIndex ) ) )
            {
            *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
            }
          ++lineIncrement;
          tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
          }
        for ( int imageTypeIndex = 0; imageTypeIndex < PAR_DEFAULT_IMAGE_TYPES_SIZE;
              imageTypeIndex++ )
          {
          if ( *( imageType + imageTypeIndex ) >= 0 )
            {
            imageTypeAndSequence.first = imageTypeIndex;
            imageTypeAndSequence.second =
              *( parParam.scanning_sequences + scanIndex );
            recImageTypesScanSequence.push_back(imageTypeAndSequence);
            }
          }
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4:
      {
      struct image_info_defV4 tempInfo;
      for ( int scanIndex = 0; scanIndex < parParam.num_scanning_sequences;
            scanIndex++ )
        {
        PhilipsPAR::PARImageTypeScanSequence imageTypeAndSequence;
        int                                  lineIncrement = 92;
        int                                  imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
        tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
        while ( !tempInfo.problemreading && tempInfo.slice )
          {
          if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
               && ( tempInfo.scan_sequence == *( parParam.scanning_sequences + scanIndex ) ) )
            {
            *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
            }
          ++lineIncrement;
          tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
          }
        for ( int imageTypeIndex = 0; imageTypeIndex < PAR_DEFAULT_IMAGE_TYPES_SIZE;
              imageTypeIndex++ )
          {
          if ( *( imageType + imageTypeIndex ) >= 0 )
            {
            imageTypeAndSequence.first = imageTypeIndex;
            imageTypeAndSequence.second =
              *( parParam.scanning_sequences + scanIndex );
            recImageTypesScanSequence.push_back(imageTypeAndSequence);
            }
          }
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
      {
      struct image_info_defV4 tempInfo;
      for ( int scanIndex = 0; scanIndex < parParam.num_scanning_sequences;
            scanIndex++ )
        {
        PhilipsPAR::PARImageTypeScanSequence imageTypeAndSequence;
        int                                  lineIncrement = 99;
        int                                  imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
        tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
        while ( !tempInfo.problemreading && tempInfo.slice )
          {
          if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
               && ( tempInfo.scan_sequence == *( parParam.scanning_sequences + scanIndex ) ) )
            {
            *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
            }
          ++lineIncrement;
          tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
          }
        for ( int imageTypeIndex = 0; imageTypeIndex < PAR_DEFAULT_IMAGE_TYPES_SIZE;
              imageTypeIndex++ )
          {
          if ( *( imageType + imageTypeIndex ) >= 0 )
            {
            imageTypeAndSequence.first = imageTypeIndex;
            imageTypeAndSequence.second =
              *( parParam.scanning_sequences + scanIndex );
            recImageTypesScanSequence.push_back(imageTypeAndSequence);
            }
          }
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
      {
      struct image_info_defV4 tempInfo;
      for ( int scanIndex = 0; scanIndex < parParam.num_scanning_sequences;
            scanIndex++ )
        {
        PhilipsPAR::PARImageTypeScanSequence imageTypeAndSequence;
        int                                  lineIncrement = 101;
        int                                  imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
        tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
        while ( !tempInfo.problemreading && tempInfo.slice )
          {
          if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
               && ( tempInfo.scan_sequence == *( parParam.scanning_sequences + scanIndex ) ) )
            {
            *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
            }
          ++lineIncrement;
          tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
          }
        for ( int imageTypeIndex = 0; imageTypeIndex < PAR_DEFAULT_IMAGE_TYPES_SIZE;
              imageTypeIndex++ )
          {
          if ( *( imageType + imageTypeIndex ) >= 0 )
            {
            imageTypeAndSequence.first = imageTypeIndex;
            imageTypeAndSequence.second =
              *( parParam.scanning_sequences + scanIndex );
            recImageTypesScanSequence.push_back(imageTypeAndSequence);
            }
          }
        }
      }
      break;
    }
  return recImageTypesScanSequence;
}

bool PhilipsPAR::GetRECRescaleValues(std::string parFile,
                                     PhilipsPAR::PARRescaleValuesContainer *rescaleValues, int scan_sequence)
{
  int ResToolsVersion;

  rescaleValues->clear();
  // Must match size of image_types
  rescaleValues->resize(PAR_DEFAULT_IMAGE_TYPES_SIZE);
  PhilipsPAR::PARRescaleValues zero(0.0);
  for ( unsigned int zeroIndex = 0; zeroIndex < rescaleValues->size(); zeroIndex++ )
    {
    ( *rescaleValues )[zeroIndex] = zero; // Zero out everything
    }

  // Check version of PAR file.
  ResToolsVersion = this->GetPARVersion(parFile);
  if ( ResToolsVersion == RESEARCH_IMAGE_EXPORT_TOOL_UNKNOWN )
    {
    return false;
    }

  switch ( ResToolsVersion )
    {
    case RESEARCH_IMAGE_EXPORT_TOOL_V3:
      {
      struct image_info_defV3      tempInfo;
      PhilipsPAR::PARRescaleValues rescale;
      int                          imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
      int                          lineIncrement = 89;
      tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
             && ( tempInfo.scan_sequence == scan_sequence ) )
          {
          *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
          rescale[0] = tempInfo.rescale_int;
          rescale[1] = tempInfo.rescale_slope;
          rescale[2] = tempInfo.scale_slope;
          ( *rescaleValues )[tempInfo.image_type_mr] = rescale;
          }
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV3(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4:
      {
      struct image_info_defV4      tempInfo;
      PhilipsPAR::PARRescaleValues rescale;
      int                          imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
      int                          lineIncrement = 92;
      tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
             && ( tempInfo.scan_sequence == scan_sequence ) )
          {
          *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
          rescale[0] = tempInfo.rescale_int;
          rescale[1] = tempInfo.rescale_slope;
          rescale[2] = tempInfo.scale_slope;
          ( *rescaleValues )[tempInfo.image_type_mr] = rescale;
          }
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV4(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_1:
      {
      struct image_info_defV4      tempInfo;
      PhilipsPAR::PARRescaleValues rescale;
      int                          imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
      int                          lineIncrement = 99;
      tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
             && ( tempInfo.scan_sequence == scan_sequence ) )
          {
          *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
          rescale[0] = tempInfo.rescale_int;
          rescale[1] = tempInfo.rescale_slope;
          rescale[2] = tempInfo.scale_slope;
          ( *rescaleValues )[tempInfo.image_type_mr] = rescale;
          }
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
        }
      }
      break;
    case RESEARCH_IMAGE_EXPORT_TOOL_V4_2:
      {
      struct image_info_defV4      tempInfo;
      PhilipsPAR::PARRescaleValues rescale;
      int                          imageType[PAR_DEFAULT_IMAGE_TYPES_SIZE] = { -1, -1, -1, -1, -1, -1, -1, -1 };
      int                          lineIncrement = 101;
      tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
      while ( !tempInfo.problemreading && tempInfo.slice )
        {
        if ( ( *( imageType + tempInfo.image_type_mr ) < 0 )
             && ( tempInfo.scan_sequence == scan_sequence ) )
          {
          *( imageType + tempInfo.image_type_mr ) = tempInfo.image_type_mr;
          rescale[0] = tempInfo.rescale_int;
          rescale[1] = tempInfo.rescale_slope;
          rescale[2] = tempInfo.scale_slope;
          ( *rescaleValues )[tempInfo.image_type_mr] = rescale;
          }
        ++lineIncrement;
        tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
        }
      }
      break;
    }
  return true;
}

bool PhilipsPAR::GetDiffusionGradientOrientationAndBValues(std::string parFile,
                                                           PhilipsPAR::PARDiffusionValuesContainer *gradientValues,
                                                           PhilipsPAR::PARBValuesContainer *bValues)
{
  gradientValues->resize(0); // Reset to zero size.
  bValues->resize(0);
  struct par_parameter tempPar;
  int                  ResToolsVersion;

  // Check version of PAR file.
  // Diffusion gradients are only stored in PAR version >= 4.1
  ResToolsVersion = this->GetPARVersion(parFile);
  if ( ResToolsVersion >= RESEARCH_IMAGE_EXPORT_TOOL_V4_1 )
    {
    struct image_info_defV4 tempInfo;
    int                     gradientOrientationNumber = -1;
    int                     lineIncrement = 99;

    if ( ResToolsVersion == RESEARCH_IMAGE_EXPORT_TOOL_V4_2 )
      {
      lineIncrement = 101;
      }

    try
      {
      this->ReadPAR(parFile, &tempPar);
      }
    catch ( ExceptionObject & )
      {
      return false;
      }

    gradientValues->resize(tempPar.max_num_grad_orient);
    bValues->resize(tempPar.max_num_grad_orient);

    if ( tempPar.max_num_grad_orient <= 0 )
      {
      return true;
      }

    // Can use either version 4.1 or 4.2 GetImageInformationDefinition
    // function.
    int gradientDirectionCount = 0;
    tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
    while ( !tempInfo.problemreading && tempInfo.slice
            && ( gradientDirectionCount < tempPar.max_num_grad_orient ) )
      {
      int tempGradientOrientationNumber = tempInfo.gradient_orientation_number;
      if ( gradientOrientationNumber != tempGradientOrientationNumber )
        {
        PhilipsPAR::PARDiffusionValues direction;
        direction[0] = tempInfo.diffusion_ap;
        direction[1] = tempInfo.diffusion_fh;
        direction[2] = tempInfo.diffusion_rl;
        ( *gradientValues )[gradientDirectionCount] = direction;
        ( *bValues )[gradientDirectionCount] = tempInfo.diffusion_b_factor;
        ++gradientDirectionCount;
        gradientOrientationNumber = tempGradientOrientationNumber;
        }
      ++lineIncrement;
      tempInfo = GetImageInformationDefinitionV41(parFile, lineIncrement, this);
      }
    }
  return true;
}

bool PhilipsPAR::GetLabelTypesASL(std::string parFile,
                                  PhilipsPAR::PARLabelTypesASLContainer *labelTypes)
{
  labelTypes->resize(0); // Reset to zero size.
  struct par_parameter tempPar;
  int                  ResToolsVersion;

  // Check version of PAR file.
  // ASL labels are only stored in PAR version >= 4.2
  ResToolsVersion = this->GetPARVersion(parFile);
  if ( ResToolsVersion >= RESEARCH_IMAGE_EXPORT_TOOL_V4_2 )
    {
    struct image_info_defV4 tempInfo;
    int                     aslLabelNumber = -1;
    int                     lineIncrement = 101;

    try
      {
      this->ReadPAR(parFile, &tempPar);
      }
    catch ( ExceptionObject & )
      {
      return false;
      }

    labelTypes->resize(tempPar.num_label_types);

    if ( tempPar.num_label_types <= 0 )
      {
      return true;
      }

    int aslLabelCount = 0;
    tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
    while ( !tempInfo.problemreading && tempInfo.slice
            && ( aslLabelCount < tempPar.num_label_types ) )
      {
      int tempASLLabelNumber = tempInfo.labelTypeASL;
      if ( aslLabelNumber != tempASLLabelNumber )
        {
        ( *labelTypes )[aslLabelCount] = tempASLLabelNumber;
        ++aslLabelCount;
        aslLabelNumber = tempASLLabelNumber;
        }
      ++lineIncrement;
      tempInfo = GetImageInformationDefinitionV42(parFile, lineIncrement, this);
      }
    }
  return true;
}

void PhilipsPAR::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk
