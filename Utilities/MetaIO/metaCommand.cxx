/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    metaCommand.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "metaCommand.h"
#include <stdio.h>
#include <string>
#include <fstream>

MetaCommand::MetaCommand()
{
  m_HelpCallBack = NULL;
  m_OptionVector.clear();
  m_Version = "Not defined";
  m_Date = "Not defined";
  m_Name = "";
  m_Author = "Not defined";
  m_Description = "";
  m_ParsedOptionVector.clear();
}


/** Extract the date from the $Date: 2006-05-19 18:15:22 $ cvs command */
std::string MetaCommand::ExtractDateFromCVS(std::string date)
{
  std::string newdate;
  for(int i=7;i<(int)date.size()-1;i++)
    {
    newdate += date[i];
    }
  return newdate.c_str();
}


/** */
bool MetaCommand::SetOption(Option option)
{
  // need to add some tests here to check if the option is not defined yet
  m_OptionVector.push_back(option);
  return true;
}

bool MetaCommand::SetOption(std::string name,
                            std::string tag,
                            bool required,
                            std::string description,
                            std::vector<Field> fields)
{
  // need to add some tests here to check if the option is not defined yet
  if(tag == "")
    {
    std::cout << "Tag cannot be empty : use AddField() instead." << std::endl;
    return false;
    }

  Option option;
  option.name = name;
  option.tag = tag;
  option.fields = fields;
  option.required = required;
  option.description = description;
  option.userDefined = false;
  option.complete = false;

  m_OptionVector.push_back(option);
  return true;
}


bool MetaCommand::SetOption(std::string name,
                            std::string tag,
                            bool required,
                            std::string description,
                            TypeEnumType type,
                            std::string defVal)
{
  // need to add some tests here to check if the option is not defined yet
  if(tag == "")
    {
    std::cout << "Tag cannot be empty : use AddField() instead." << std::endl;
    return false;
    }

  Option option;
  option.tag = tag;
  option.name = name;
  option.required = required;
  option.description = description;
  option.userDefined = false;
  option.complete = false;

  // Create a field without description as a flag
  Field field;
  if(type == LIST)
    {
    field.name = "NumberOfValues";
    }
  else
    {
    field.name = name;
    }
  field.externaldata = DATA_NONE;
  field.type = type;
  field.value = defVal;
  field.userDefined = false;
  field.required = true;
  field.rangeMin = "";
  field.rangeMax = "";
  option.fields.push_back(field);

  m_OptionVector.push_back(option);
  return true;
}


/** Add a field */
bool MetaCommand::AddField(std::string name,
                           std::string description,
                           TypeEnumType type,
                           DataEnumType externalData,
                           std::string rangeMin,
                           std::string rangeMax)
{
  // need to add some tests here to check if the option is not defined yet
  Option option;
  option.tag = "";

  // Create a field without description with the specified type
  Field field;
  field.name = name;
  field.type = type;
  field.required = true;
  field.userDefined = false;
  field.externaldata = externalData;
  field.rangeMin = rangeMin;
  field.rangeMax = rangeMax;
  option.fields.push_back(field);

  option.required = true;
  option.name = name;
  option.description = description;
  option.userDefined = false;
  option.complete = false;

  m_OptionVector.push_back(option);
  return true;
}


/** Collect all the information until the next tag 
  * \warning this function works only if the field is of type String */ 
void MetaCommand::SetOptionComplete(std::string optionName,
                                    bool complete)
{
  OptionVector::iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      (*it).complete = complete;
      return;
      }
    it++;
    }
 }

/** Add a field to a given an option */
bool MetaCommand::AddOptionField(std::string optionName,
                                 std::string name,
                                 TypeEnumType type,
                                 bool required,
                                 std::string defVal,
                                 std::string description,
                                 DataEnumType externalData
                                 )
{ 
  OptionVector::iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      // Create a field without description with the specified type
      Field field;
      field.name = name;
      field.type = type;
      field.required = required;
      field.value = defVal;
      field.description = description;
      field.userDefined = false;
      field.externaldata = externalData;
      field.rangeMin = "";
      field.rangeMax = "";
    
      // If this is the first field in the list we replace the current field
      if((*it).fields[0].type == FLAG)
        {
        (*it).fields[0] = field;
        }
      else
        {
        (*it).fields.push_back(field);
        }
      return true;
      }
    it++;
    }
  return false;
}

/** Set the range of an option */
bool MetaCommand::SetOptionRange(std::string optionName,
                                 std::string name,
                                 std::string rangeMin,
                                 std::string rangeMax)
{
  OptionVector::iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      std::vector<Field> & fields = (*it).fields;
      std::vector<Field>::iterator itField = fields.begin();
      while(itField != fields.end())
        {
        if((*itField).name == name)
          {
          (*itField).rangeMin = rangeMin;
          (*itField).rangeMax = rangeMax;
          return true;
          }
        itField++;
        }
      }
    it++;
    }
  return false;
}


/** Return the value of the option as a boolean */
bool MetaCommand::GetValueAsBool(std::string optionName,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = optionName;
    }
 
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      std::vector<Field>::const_iterator itField = (*it).fields.begin();
      while(itField != (*it).fields.end())
        {
        if((*itField).name == fieldname)
          {
          if((*itField).value == "true"
            || (*itField).value == "1"
            || (*itField).value == "True"
            || (*itField).value == "TRUE"
            )
            {
            return true;
            }
          return false;
          }
        itField++;
        }
      }
    it++;
    }
  return false;
}


/** Return the value of the option as a bool */
bool MetaCommand::GetValueAsBool(Option option,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = option.name;
    }

  std::vector<Field>::const_iterator itField = option.fields.begin();
  while(itField != option.fields.end())
    {
    if((*itField).name == fieldname)
      {
      if((*itField).value == "true"
         || (*itField).value == "1"
         || (*itField).value == "True"
         || (*itField).value == "TRUE"
        )
        {
        return true;
        }
      return false;
      }
    itField++;
    }
  return 0;
}

/** Return the value of the option as a float */
float MetaCommand::GetValueAsFloat(std::string optionName,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = optionName;
    }
 
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      std::vector<Field>::const_iterator itField = (*it).fields.begin();
      while(itField != (*it).fields.end())
        {
        if((*itField).name == fieldname)
          {
          return (float)atof((*itField).value.c_str());
          }
        itField++;
        }
      }
    it++;
    }
  return 0;
}

/** Return the value of the option as a float */
float MetaCommand::GetValueAsFloat(Option option,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = option.name;
    }

  std::vector<Field>::const_iterator itField = option.fields.begin();
  while(itField != option.fields.end())
    {
    if((*itField).name == fieldname)
      {
      return (float)atof((*itField).value.c_str());
      }
    itField++;
    }
  return 0;
}

/** Return the value of the option as a int */
int MetaCommand::GetValueAsInt(std::string optionName,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = optionName;
    }
 
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      std::vector<Field>::const_iterator itField = (*it).fields.begin();
      while(itField != (*it).fields.end())
        {
        if((*itField).name == fieldname)
          {
          return atoi((*itField).value.c_str());
          }
        itField++;
        }
      }
    it++;
    }
  return 0;
}

/** Return the value of the option as a int */
int MetaCommand::GetValueAsInt(Option option,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = option.name;
    }

  std::vector<Field>::const_iterator itField = option.fields.begin();
  while(itField != option.fields.end())
    {
    if((*itField).name == fieldname)
      {
      return atoi((*itField).value.c_str());
      }
    itField++;
    }
  return 0;
}

/** Return the value of the option as a string */
std::string MetaCommand::GetValueAsString(std::string optionName,
                                          std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = optionName;
    }
 
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      std::vector<Field>::const_iterator itField = (*it).fields.begin();
      while(itField != (*it).fields.end())
        {
        if((*itField).name == fieldname)
          {
          return (*itField).value;
          }
        itField++;
        }
      }
    it++;
    }
  return "";
}

/** Return the value of the option as a string */
std::string MetaCommand::GetValueAsString(Option option,std::string fieldName)
{
  std::string fieldname = fieldName;
  if(fieldName == "")
    {
    fieldname = option.name;
    }

  std::vector<Field>::const_iterator itField = option.fields.begin();
  while(itField != option.fields.end())
    {
    if((*itField).name == fieldname)
      {
      return (*itField).value;
      }
    itField++;
    }
  return "";
}

/** Return the value of the option as a list of strings */
std::list<std::string> MetaCommand::
GetValueAsList( Option option )
{
  std::list<std::string> results;
  results.clear();
  std::vector<Field>::const_iterator itField = option.fields.begin();
  itField++;
  while(itField != option.fields.end())
    {
    results.push_back((*itField).value);
    itField++;
    }
  return results;
}

std::list< std::string > MetaCommand::
GetValueAsList( std::string optionName )
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).name == optionName)
      {
      return this->GetValueAsList( *it );
      }
    it++;
    }
  std::list< std::string > empty;
  empty.clear();
  return empty;
}

bool MetaCommand::
GetOptionWasSet(Option option)
{
  if(option.userDefined)
    {
    return true;
    }
  return false;
}

bool MetaCommand::
GetOptionWasSet( std::string optionName)
{
  OptionVector::const_iterator it = m_ParsedOptionVector.begin();
  while(it != m_ParsedOptionVector.end())
    {
    if((*it).name == optionName)
      {
      return true;
      }
    it++;
    }
  return false;
}

/** List the current options */
void MetaCommand::ListOptions()
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  int i=0;
  while(it != m_OptionVector.end())
    {
    std::cout << "Option #" << i << std::endl;
    std::cout << "   Name: " <<  (*it).name.c_str() << std::endl;
    if((*it).tag.size() > 0)
      {
      std::cout << "   Tag: " << (*it).tag.c_str() << std::endl;
      }
    std::cout << "   Description: " << (*it).description.c_str() << std::endl;
    if((*it).required)
      {
      std::cout << "   Required: true" << std::endl;
      }
    else
      {
      std::cout << "   Required: false" << std::endl;
      }
    std::cout << "   Number of expeted values: " << (*it).fields.size() 
                                              << std::endl;
    
    std::vector<Field>::const_iterator itField = (*it).fields.begin();
    while(itField != (*it).fields.end())
      {
      std::cout << "      Field Name: " <<  (*itField).name.c_str() 
                                        << std::endl;
      std::cout << "      Description: " << (*itField).description.c_str() 
                                         << std::endl;
      std::cout << "      Type: " << this->TypeToString((*itField).type).c_str()
                                  << std::endl;
      std::cout << "      Value: " << (*itField).value.c_str() << std::endl;
      
      if((*itField).externaldata)
        {
        std::cout << "      External Data: true" << std::endl;
        }
      else
        {
        std::cout << "      External Data: false" << std::endl;
        }

      if((*itField).required)
        {
        std::cout << "      Required: true" << std::endl;
        }
      else
        {
        std::cout << "      Required: false" << std::endl;
        }
      itField++;
      }
    std::cout << std::endl;
    i++;
    it++;
    }
  if(m_HelpCallBack != NULL)
    {
    m_HelpCallBack();
    }
}

/** List the current options in xml format */
void MetaCommand::ListOptionsXML()
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  int i=0;
  while(it != m_OptionVector.end())
    {
    std::cout << "<option>" << std::endl;
    std::cout << "<number>" << i << "</number>" << std::endl;
    std::cout << "<name>" << (*it).name.c_str() << "</name>" << std::endl;
    std::cout << "<tag>" << (*it).tag.c_str() << "</tag>" << std::endl;
    std::cout << "<description>" << (*it).description.c_str() 
                                 << "</description>" << std::endl;
    std::cout << "<required>";
    if((*it).required)
      {
      std::cout << "1</required>" << std::endl;
      }
    else
      {
      std::cout << "0</required>" << std::endl;
      }

    std::cout << "<nvalues>" << (*it).fields.size() << "</nvalues>" << std::endl;
    
    std::vector<Field>::const_iterator itField = (*it).fields.begin();
    while(itField != (*it).fields.end())
      {
      std::cout << "<field>" << std::endl;
      std::cout << "<name>" << (*itField).name.c_str() << "</name>" << std::endl;
      std::cout << "<description>" << (*itField).description.c_str() 
                                   << "</description>" << std::endl;
      std::cout << "<type>" << this->TypeToString((*itField).type).c_str() 
                            << "</type>" << std::endl;
      std::cout << "<value>" << (*itField).value.c_str() << "</value>" 
                             << std::endl; 
      std::cout << "<external>";
      if((*itField).externaldata)
        {
        std::cout << "1</external>" << std::endl;
        }
      else
        {
        std::cout << "0</external>" << std::endl;
        }
      std::cout << "<required>";
      if((*itField).required)
        {
        std::cout << "1</required>" << std::endl;
        }
      else
        {
        std::cout << "0</required>" << std::endl;
        }


      std::cout << "</field>" << std::endl;
      itField++;
      }
    std::cout << "</option>" << std::endl;
    i++;
    it++;
    }
}

/** Internal small XML parser */
std::string MetaCommand::GetXML(const char* buffer,
                                const char* desc,
                                unsigned long pos)
{
  std::string begin = "<";
  begin += desc;
  begin += ">";
  std::string end = "</";
  end += desc;
  end += ">";

  std::string buf = buffer;

  long int posb = buf.find(begin,pos);
  if(posb == -1)
    {
    return "";
    }
  long int pose = buf.find(end,posb);
  if(pose == -1)
    {
    return "";
    }

  return buf.substr(posb+begin.size(),pose-posb-begin.size());
}

/** Given an XML buffer fill in the command line arguments */
bool MetaCommand::ParseXML(const char* buffer)
{
  m_OptionVector.clear();
  std::string buf = this->GetXML(buffer,"option",0);
  long pos = 0;
  while(buf.size() > 0)
    {
    Option option;
    option.name = this->GetXML(buf.c_str(),"name",0);
    option.tag = this->GetXML(buf.c_str(),"tag",0);
    option.description = this->GetXML(buf.c_str(),"description",0);
    if(atoi(this->GetXML(buf.c_str(),"required",0).c_str()) == 0)
      {
      option.required = false;
      }
    else
      {
      option.required = true;
      }
    unsigned int n = atoi(this->GetXML(buf.c_str(),"nvalues",0).c_str());

    // Now check the fields
    long posF = buf.find("<field>");
    for(unsigned int i=0;i<n;i++)
      {
      std::string f = this->GetXML(buf.c_str(),"field",posF);
      Field field;
      field.userDefined = false;
      field.name = this->GetXML(f.c_str(),"name",0);
      field.description = this->GetXML(f.c_str(),"description",0);
      field.value = this->GetXML(f.c_str(),"value",0);
      field.type = this->StringToType(this->GetXML(f.c_str(),"type",0).c_str());
      if(atoi(this->GetXML(f.c_str(),"external",0).c_str()) == 0)
        {
        field.externaldata = DATA_NONE;
        }
      else
        {
        if(atoi(this->GetXML(f.c_str(),"external",0).c_str()) == 1)
          {
          field.externaldata = DATA_IN;
          }
        else
          {
          field.externaldata = DATA_OUT;
          }
        }
      if(atoi(this->GetXML(f.c_str(),"required",0).c_str()) == 0)
        {
        field.required = false;
        }
      else
        {
        field.required = true;
        }

      option.fields.push_back(field);
      posF += f.size()+8;
      }

    m_OptionVector.push_back(option);

    pos += buf.size()+17;
    buf = this->GetXML(buffer,"option",pos);
    }

  return true;
}


/** List the current options */
void MetaCommand::ListOptionsSimplified()
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if(!(*it).required)
      {
      std::cout << "   [ ";
      }
    else
      {
      std::cout << "   ";
      }
    if((*it).tag.size() > 0)
      {
        std::cout << "-" << (*it).tag.c_str() << " ";
      }
    std::vector<Field>::const_iterator itField = (*it).fields.begin();
    while(itField != (*it).fields.end())
      {
      if((*itField).type != FLAG) // only display the type if it's not a FLAG
        {
        if((*itField).required)
          {
          std::cout << "<";
          }
        else
          {
          std::cout << "[";
          }

        std::cout << (*itField).name.c_str();
     
        if((*itField).required)
          {
          std::cout << "> ";
          }
        else
          {
          std::cout << "] ";
          }

        }
      itField++;
      }

    if(!(*it).required)
      {
      std::cout << "]";
      }
    std::cout << std::endl;

    if((*it).description.size()>0)
      {
      std::cout << "      = " << (*it).description.c_str();
      std::cout << std::endl;
      itField = (*it).fields.begin();
      while(itField != (*it).fields.end())
        {
        if((*itField).description.size() > 0
           || (*itField).value.size() > 0)
          {
          std::cout << "        With: " << (*itField).name.c_str();
          if((*itField).description.size() > 0)
            {
            std::cout << " = " << (*itField).description.c_str();
            }
          if((*itField).value.size() > 0)
            {
            std::cout << " (Default = " << (*itField).value << ")";
            }
          std::cout << std::endl;
          }
        itField++;
        }
      }

    std::cout << std::endl;
    it++;
    }

  if(m_HelpCallBack != NULL)
    {
    m_HelpCallBack();
    }
}

/** Get the option by "-"+tag */
bool 
MetaCommand::OptionExistsByMinusTag(std::string minusTag)
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    { 
    std::string tagToSearch = "-";
    tagToSearch += (*it).tag;
    if(tagToSearch == minusTag)
      {
      return true;
      }
    it++;
    }

  return false;
  
}


/** Get the option by "-"+tag */
MetaCommand::Option *
MetaCommand::GetOptionByMinusTag(std::string minusTag)
{
  OptionVector::iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    { 
    std::string tagToSearch = "-";
    tagToSearch += (*it).tag;
    if(tagToSearch == minusTag)
      {
      return &(*it);
      }
    it++;
    }
  return NULL;
}

/** Get the option by tag */
MetaCommand::Option *
MetaCommand::GetOptionByTag(std::string minusTag)
{
  OptionVector::iterator it = m_OptionVector.begin();
  while(it != m_OptionVector.end())
    {
    if((*it).tag == minusTag)
      {
      return &(*it);
      }
    it++;
    }
  return NULL;
}

/** Return the option id. i.e the position in the vector */
long
MetaCommand::GetOptionId(Option* option)
{
  OptionVector::iterator it = m_OptionVector.begin();
  unsigned long i = 0;
  while(it != m_OptionVector.end())
    {
    if(&(*it) == option)
      {
      return i;
      }
    i++;
    it++;
    }
  return -1;
}

/** Export the current command line arguments to a Grid Application
 *  Description file */
bool MetaCommand::ExportGAD(bool dynamic)
{
  std::cout << "Exporting GAD file...";

  OptionVector options = m_OptionVector;
  if(dynamic)
    {
    options = m_ParsedOptionVector;
    }

  if(m_Name=="")
    {
    std::cout << "Set the name of the application using SetName()" << std::endl;
    return false;
    }

  std::string filename = m_Name;
  filename += ".gad.xml";

  std::ofstream file;
  file.open(filename.c_str(), std::ios::binary | std::ios::out);
  if(!file.is_open())
    {
    std::cout << "Cannot open file for writing: " << filename.c_str() <<  std::endl;
    return false;
    }
  
  file << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" << std::endl;
  file << "<GridApplication" << std::endl;
  file << "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" << std::endl;
  file << "xsi:noNamespaceSchemaLocation=\"grid-application-description.xsd\"" << std::endl;
  file << "Name=\"" << m_Name.c_str() << "\"" << std::endl;
  file << "Description=\"" << m_Description.c_str() << "\">" << std::endl;
  file << "<ApplicationComponent Name=\"Client\" RemoteExecution=\"true\">" << std::endl;
  file << "<ComponentActionList>" << std::endl;
  file << std::endl;

  unsigned int order = 1;
  // Write out the input data to be transfered
  OptionVector::const_iterator it = options.begin();
  while(it != options.end())
    {
    std::vector<Field>::const_iterator itFields = (*it).fields.begin();
    while(itFields != (*it).fields.end())
      {
      if((*itFields).externaldata == DATA_IN)
        {
        file << " <ComponentAction Type=\"DataRelocation\" Order=\"" << order << "\">" << std::endl;
        file << "  <parameter Name=\"Name\" Value=\"" << (*itFields).name <<"\"/>" << std::endl;
        file << "  <parameter Name=\"Host\" Value=\"hostname\"/>" << std::endl;
        file << "  <parameter Name=\"Description\" Value=\"" << (*itFields).description << "\"/>" << std::endl;
        file << "  <parameter Name=\"Direction\" Value=\"In\"/>" << std::endl;
        file << "  <parameter Name=\"Protocol\" Value=\"gsiftp\"/>" << std::endl;
        file << "  <parameter Name=\"SourceDataPath\" Value=\"" << (*itFields).value << "\"/>" << std::endl;

        std::string datapath = (*itFields).value;
        long int slash = datapath.find_last_of("/");
        if(slash>0)
          {
          datapath = datapath.substr(slash+1,datapath.size()-slash-1);
          }
        slash = datapath.find_last_of("\\");
        if(slash>0)
          {
          datapath = datapath.substr(slash+1,datapath.size()-slash-1);
          }
        file << "  <parameter Name=\"DestDataPath\" Value=\"" << datapath.c_str() << "\"/>" << std::endl;
        file << " </ComponentAction>" << std::endl;
        file << std::endl;
        order++;
        }
      itFields++;
      }
    it++;
    }

  file << " <ComponentAction Type=\"JobSubmission\" Order=\"" << order << "\">" << std::endl;
  file << "  <parameter Name=\"Executable\" Value=\"" << m_ExecutableName.c_str() << "\"/>" << std::endl;
  file << "  <parameter Name=\"Arguments\"  Value=\"";
  // Write out the command line arguments
  it = options.begin();
  while(it != options.end())
    {
    if(it != options.begin())
      {
      file << " ";
      }
    file << "{" << (*it).name.c_str() << "}";
    it++;
    }
  file << "\"/>" << std::endl;

  file << "   <arguments>" << std::endl;
  // Write out the arguments that are not data
  it = options.begin();
  while(it != options.end())
    {
    // Find if this is a non data field
    bool isData = false;
    std::vector<Field>::const_iterator itFields = (*it).fields.begin();
    while(itFields != (*it).fields.end())
      {
      if((*itFields).externaldata != DATA_NONE)
        {
        isData = true;
        break;
        }
      itFields++;
      }

    if(isData)
      {
      it++;
      continue;
      }

    file << "   <group Name=\"" << (*it).name.c_str();
    file << "\" Syntax=\"";
    
    if((*it).tag.size()>0)
      {
      file << "-" << (*it).tag.c_str() << " ";
      }

    itFields = (*it).fields.begin();
    while(itFields != (*it).fields.end())
      {
      if(itFields != (*it).fields.begin())
        {
        file << " ";
        }
      file << "{" << (*it).name << (*itFields).name << "}";
      itFields++;
      }  
    file << "\"";
    
    if(!(*it).required)
      {
      file << " Optional=\"true\"";
      
      // Add if the option was selected
      if((*it).userDefined)
        {
        file << " Selected=\"true\"";
        }
      else
        {
        file << " Selected=\"false\"";
        }
      }
    
    file << ">" << std::endl; 

    // Now writes the value of the arguments 
    itFields = (*it).fields.begin();
    while(itFields != (*it).fields.end())
      {
      file << "    <argument Name=\"" << (*it).name << (*itFields).name;
      file << "\" Value=\"" << (*itFields).value;
      file << "\" Type=\"" << this->TypeToString((*itFields).type).c_str();
      file << "\"";
      
      if((*itFields).rangeMin != "")
        {
        file << " RangeMin=\"" << (*itFields).rangeMin << "\"";
        }

      if((*itFields).rangeMax != "")
        {
        file << " RangeMax=\"" << (*itFields).rangeMax << "\"";
        } 
      file << "/>" << std::endl;
      itFields++;
      }
    file << "  </group>" << std::endl;
    it++;
    }
  file << "  </arguments>" << std::endl;
  file << " </ComponentAction>" << std::endl;
  order++;
  file << std::endl;
  // Write out the input data to be transfered
  it = options.begin();
  while(it != options.end())
    {
    std::vector<Field>::const_iterator itFields = (*it).fields.begin();
    while(itFields != (*it).fields.end())
      {
      if((*itFields).externaldata == DATA_OUT)
        {
        file << " <ComponentAction Type=\"DataRelocation\" Order=\"" << order << "\">" << std::endl;
        file << "  <parameter Name=\"Name\" Value=\"" << (*itFields).name <<"\"/>" << std::endl;
        file << "  <parameter Name=\"Host\" Value=\"hostname\"/>" << std::endl;
        file << "  <parameter Name=\"Description\" Value=\"" << (*itFields).description << "\"/>" << std::endl;
        file << "  <parameter Name=\"Direction\" Value=\"Out\"/>" << std::endl;
        file << "  <parameter Name=\"Protocol\" Value=\"gsiftp\"/>" << std::endl;
        std::string datapath = (*itFields).value;
        long int slash = datapath.find_last_of("/");
        if(slash>0)
          {
          datapath = datapath.substr(slash+1,datapath.size()-slash-1);
          }
        slash = datapath.find_last_of("\\");
        if(slash>0)
          {
          datapath = datapath.substr(slash+1,datapath.size()-slash-1);
          }
        file << "  <parameter Name=\"SourceDataPath\" Value=\"" << datapath.c_str() << "\"/>" << std::endl;
        file << "  <parameter Name=\"DestDataPath\" Value=\"" << (*itFields).value << "\"/>" << std::endl;
        file << " </ComponentAction>" << std::endl;
        file << std::endl;
        order++;
        }
      itFields++;
      }
    it++;
    }
  file << std::endl;
  file << "    </ComponentActionList>" << std::endl;
  file << "  </ApplicationComponent>" << std::endl;
  file << "</GridApplication>" << std::endl;

  file.close();

  std::cout << "done" << std::endl;
  return true;
}


/** Parse the command line */
bool MetaCommand::Parse(int argc, char* argv[])
{  
  m_ExecutableName = argv[0];

  long int slash = m_ExecutableName.find_last_of("/");
  if(slash>0)
    {
    m_ExecutableName = m_ExecutableName.substr(slash+1,m_ExecutableName.size()-slash-1);
    }
  slash = m_ExecutableName.find_last_of("\\");
  if(slash>0)
    {
    m_ExecutableName = m_ExecutableName.substr(slash+1,m_ExecutableName.size()-slash-1);
    }

  // List the options if using -V
  if((argc == 2 && !strcmp(argv[1],"-V"))
     || (argc == 2 && !strcmp(argv[1],"-H")))
    {
    std::cout << "Usage : " << argv[0] << std::endl; 
    this->ListOptions();
    return false;
    }
  // List the options if using -v
  else if((argc == 2 && !strcmp(argv[1],"-v"))
          || (argc == 2 && !strcmp(argv[1],"-h")))
    {
    std::cout << "Usage : " << argv[0] << std::endl; 
    this->ListOptionsSimplified();
    return false;
    }
  else if(argc == 2 && !strcmp(argv[1],"-vxml"))
    {
    this->ListOptionsXML();
    return false;
    }
  else if(argc == 2 && !strcmp(argv[1],"-version"))
    {
    std::cout << "Version: " << m_Version.c_str() << std::endl;
    return false;
    }
  else if(argc == 2 && !strcmp(argv[1],"-date"))
    {
    std::cout << "Date: " << m_Date.c_str() << std::endl;
    return false;
    }
  else if(argc == 2 && !strcmp(argv[1],"-exportGAD"))
    {
    this->ExportGAD();
    return false;
    }

  // Fill in the results
  m_ParsedOptionVector.clear();
  bool inArgument = false;
  std::string tag = "";
  std::string args;
  
  unsigned int currentField = 0; // current field position
  int currentOption = 0; // id of the option to fill
  unsigned int valuesRemaining=0;
  bool isComplete = false; // check if the option should be parse until the next tag is found
  std::string completeString = "";

  bool exportGAD = false;

  for(unsigned int i=1;i<(unsigned int)argc;i++)
    {
    // If we have the tag -export-gad
    if(!strcmp(argv[i],"-exportGAD"))
      {
      exportGAD = true;
      continue;
      }

    // If this is a tag
    if(argv[i][0] == '-' && (atof(argv[i])==0))
      {    
      // if we have a tag before the expected values we throw an exception
      if(valuesRemaining!=0)
        {
        if(!isComplete)
          {
          std::cout << "Found tag before end of value list!" << std::endl;
          return false;
          }
        else
          {
          m_OptionVector[currentOption].fields[0].value = completeString;
          m_OptionVector[currentOption].fields[0].userDefined = true;
          m_OptionVector[currentOption].userDefined = true;
          m_ParsedOptionVector.push_back(m_OptionVector[currentOption]);
          }
        }
      inArgument = false;
      // New tag so we add the previous values to the tag
      tag = argv[i];

      // Check if the tag is in the list
      if(this->OptionExistsByMinusTag(tag))
        {
        inArgument = true;
        valuesRemaining = this->GetOptionByMinusTag(tag)->fields.size();
        currentOption = this->GetOptionId(this->GetOptionByMinusTag(tag));

        if(currentOption < 0)
          {
          std::cout << "Error processing tag " << tag.c_str()
                    << ".  Tag exists but cannot find its Id."
                    << std::endl;
          }
        else
          {
          isComplete = m_OptionVector[currentOption].complete;

          if(m_OptionVector[currentOption].fields[0].type == FLAG)
            {
            // the tag exists by default
            m_OptionVector[currentOption].fields[0].value = "true"; 
            valuesRemaining = 0;
            inArgument = false;
            }
          else if(m_OptionVector[currentOption].fields[0].type == LIST)
            {
            inArgument = true;        
            unsigned int valuesInList = (int)atoi(argv[++i]);
            m_OptionVector[currentOption].fields[0].value = argv[i];
            valuesRemaining += valuesInList-1;
            char optName[255];
            for(unsigned int j=0; j<valuesInList; j++)
              {
              sprintf(optName, "%03d", j);
              this->AddOptionField( m_OptionVector[currentOption].name,
                                    optName, STRING );
              }
            }
          args = "";
          }
        }
      else
        {
        std::cout << "The tag " << tag.c_str() 
                  << " is not a valid argument : skipping this tag" 
                  << std::endl;
        }
      if(inArgument)
        {
        i++;
        }
      }
    else if(!inArgument) // If this is a field
      {
      // Look for the field to add
      OptionVector::iterator it = m_OptionVector.begin();
      unsigned long pos = 0;
      bool found = false;
      while(it != m_OptionVector.end())
        {
        if((pos >= currentField) && ((*it).tag==""))
          {
          currentOption = pos;
          valuesRemaining = (*it).fields.size();
          found = true;
          break;
          }
        pos++;
        it++;
        }

      if(!found)
        {
        std::cout << "Too many arguments specified in your command line! "
                  << "Skipping extra argument: " << argv[i] << std::endl;
        }
      
      inArgument=true;
      currentField=currentOption+1;
      }

    // We collect the values
    if(isComplete)
      {
      if(completeString.size()==0)
        {
        completeString = argv[i];
        }
      else
        {
        completeString += " ";
        completeString += argv[i];
        }
      }
    else if(inArgument && i<(unsigned int)argc && valuesRemaining>0)
      {
      if(currentOption >=0 && currentOption < (int)(m_OptionVector.size()))
        {
        unsigned long s = m_OptionVector[currentOption].fields.size();
        m_OptionVector[currentOption].fields[s-valuesRemaining].value = argv[i];
        m_OptionVector[currentOption].fields[s-valuesRemaining].userDefined =
                                                                           true;
        }
      valuesRemaining--;
      }

    if(valuesRemaining == 0)
      {         
      inArgument = false;
      m_OptionVector[currentOption].userDefined = true;
      m_ParsedOptionVector.push_back(m_OptionVector[currentOption]);
      }    
    }

  if(valuesRemaining>0)
    {
    std::cout << "Not enough parameters for " 
              << m_OptionVector[currentOption].name << std::endl;
    std::cout << "Command: " << argv[0] << std::endl;
    std::cout << "Options: " << std::endl
              << "  -v or -h for help listed in short format" << std::endl
              << "  -V or -H for help listed in long format" << std::endl
              << "  -vxml for help listed in xml format" << std::endl
              << "  -export-gad to export Grid Application"
              << "Description file format" << std::endl;

    return false;
    }

  // Check if the options with required arguments are defined
  OptionVector::iterator it = m_OptionVector.begin();
  bool requiredAndNotDefined = false;
  while(it != m_OptionVector.end())
    {
    if((*it).required)
      {
      // First check if the option is actually defined
      if(!(*it).userDefined)
        {
        std::cout << "Option " << (*it).name 
                  << " is required but not defined" << std::endl;
        requiredAndNotDefined = true;
        it++;
        continue;
        }

      // Check if the values are defined
      std::vector<Field>::const_iterator itFields = (*it).fields.begin();
      bool defined = true;
      while(itFields != (*it).fields.end())
        {
        if((*itFields).value == "")
          {
          defined = false;
          }
        itFields++;
        }

      if(!defined)
        {
        if((*it).tag.size()>0)
          {
          std::cout << "Field " << (*it).tag.c_str() 
                    << " is required but not defined" << std::endl;
          }
        else
          {
          std::cout << "Field " << (*it).name.c_str() 
                    << " is required but not defined" << std::endl;
          }
        requiredAndNotDefined = true;
        }
      }
    it++;
    }

  if(requiredAndNotDefined)
    {
    std::cout << "Command: " << argv[0] << std::endl
              << "Options: " << std::endl
              << "  -v or -h for help listed in short format" << std::endl
              << "  -V or -H for help listed in long format" << std::endl
              << "  -vxml for help listed in xml format" << std::endl
              << "  -export-gad to export Grid Application"
              << "Description file format" << std::endl;
    return false;
    }

  // Check if the values are in range (if the range is defined)
  OptionVector::iterator itParsed = m_ParsedOptionVector.begin();
  bool valueInRange = true;
  while(itParsed != m_ParsedOptionVector.end())
    {
    std::vector<Field>::const_iterator itFields = (*itParsed).fields.begin();
    while(itFields != (*itParsed).fields.end())
      {
      // Check only if this is a number
      if(((*itFields).type == INT ||
        (*itFields).type == FLOAT ||
        (*itFields).type == CHAR)
        && ((*itFields).value != "")
        )
        {
        // Check the range min
        if(
          (((*itFields).rangeMin != "")
          && (atof((*itFields).rangeMin.c_str())>atof((*itFields).value.c_str())))
          ||
          (((*itFields).rangeMax != "")
          && (atof((*itFields).rangeMax.c_str())<atof((*itFields).value.c_str())))
          )
          {
          std::cout << (*itParsed).name << "." << (*itFields).name
                    << " : Value (" << (*itFields).value << ") "
                    << "is not in the range [" << (*itFields).rangeMin
                    << "," << (*itFields).rangeMax << "]" << std::endl;
          valueInRange = false;
          }
        } 
      itFields++;
      }
    itParsed++;
    }

  if(!valueInRange)
    {
    return false;
    }

  // If everything is ok
  if(exportGAD)
    {
    this->ExportGAD(true);
    return false; // prevent from running the application
    }

  return true;  
}

/** Return the string representation of a type */
std::string MetaCommand::TypeToString(TypeEnumType type)
{
  switch(type)
    {
    case INT:
      return "int";
    case FLOAT:
      return "float";
    case STRING:
      return "string";
    case LIST:
      return "list";
    case FLAG:
      return "flag";
    case BOOL:
      return "boolean";
    default:
      return "not defined";
    }
  return "not defined";
}



/** Return a type given a string */
MetaCommand::TypeEnumType MetaCommand::StringToType(const char* type)
{
  if(!strcmp(type,"int"))
    {
    return INT;
    }
  else if(!strcmp(type,"float"))
    {
    return FLOAT;
    }
  else if(!strcmp(type,"string"))
    {
    return STRING;
    }
  else if(!strcmp(type,"list"))
    {
    return LIST;
    }
  else if(!strcmp(type,"flag"))
    {
    return FLAG;
    }

  return INT; // by default

}
