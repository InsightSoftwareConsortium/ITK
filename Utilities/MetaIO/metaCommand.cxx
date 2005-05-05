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


MetaCommand::MetaCommand()
{
  m_OptionVector.clear();
  m_Version = "Not defined";
  m_Date = "Not defined";
}


/** Extract the date from the $Date: 2005-05-05 20:15:59 $ cvs command */
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

bool MetaCommand::SetOption(std::string name,std::string tag,bool required,std::string description,std::vector<Field> fields)
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

  m_OptionVector.push_back(option);
  return true;
}


bool MetaCommand::SetOption(std::string name,std::string tag,bool required,std::string description,TypeEnumType type,std::string defVal)
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

  // Create a field without description as a flag
  Field field;
  field.name = name;
  field.externaldata = false;
  field.type = type;
  field.value = defVal;
  field.required = true;
  option.fields.push_back(field);

  m_OptionVector.push_back(option);
  return true;
}


/** Add a field */
bool MetaCommand::AddField(std::string name,std::string description,TypeEnumType type,bool externalData)
{
  // need to add some tests here to check if the option is not defined yet
  Option option;
  option.tag = "";

  // Create a field without description with the specified type
  Field field;
  field.name = name;
  field.type = type;
  field.required = true;
  field.externaldata = externalData;
  option.fields.push_back(field);

  option.required = true;
  option.name = name;
  option.description = description;

  m_OptionVector.push_back(option);
  return true;
}

/** Add a field to a given an option */
bool MetaCommand::AddOptionField(std::string optionName,std::string name,TypeEnumType type,bool required,std::string defVal)
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
      field.externaldata = false;
            
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
std::string MetaCommand::GetValueAsString(std::string optionName,std::string fieldName)
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
  return 0;
}

/** List the current options */
void MetaCommand::ListOptions()
{
  OptionVector::const_iterator it = m_OptionVector.begin();
  int i=0;
  while(it != m_OptionVector.end())
    {
    std::cout << "Option #" << i << std::endl;
    std::cout << "Name: " <<  (*it).name.c_str() << std::endl;
    if((*it).tag.size() > 0)
      {
      std::cout << "Tag: " << (*it).tag.c_str() << std::endl;
      }
    std::cout << "Description: " << (*it).description.c_str() << std::endl;
    if((*it).required)
      {
      std::cout << "Required: true" << std::endl;
      }
    else
      {
      std::cout << "Required: false" << std::endl;
      }
    std::cout << "Number of expeted values: " << (*it).fields.size() << std::endl;
    
    std::vector<Field>::const_iterator itField = (*it).fields.begin();
    while(itField != (*it).fields.end())
      {
      std::cout << "Name: " <<  (*itField).name.c_str() << std::endl;
      std::cout << "Description: " << (*itField).description.c_str() << std::endl;
      std::cout << "Type: " << this->TypeToString((*itField).type).c_str() << std::endl;
      std::cout << "Value: " << (*itField).value.c_str() << std::endl;
      if((*itField).required)
        {
        std::cout << "Required: true" << std::endl;
        }
      else
        {
        std::cout << "Required: false" << std::endl;
        }
      itField++;
      }
    std::cout << std::endl;
    i++;
    it++;
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
    std::cout << "<description>" << (*it).description.c_str() << "</description>" << std::endl;
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
      std::cout << "<description>" << (*itField).description.c_str() << "</description>" << std::endl;
      std::cout << "<type>" << this->TypeToString((*itField).type).c_str() << "</type>" << std::endl;
      std::cout << "<value>" << (*itField).value.c_str() << "</value>" << std::endl;
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
std::string MetaCommand::GetXML(const char* buffer,const char* desc,unsigned long pos)
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
      field.name = this->GetXML(f.c_str(),"name",0);
      std::cout << "FIELD  = " << field.name.c_str() << std::endl;
      field.description = this->GetXML(f.c_str(),"description",0);
      field.value = this->GetXML(f.c_str(),"value",0);
      field.type = this->StringToType(this->GetXML(f.c_str(),"type",0).c_str());

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
      std::cout << "[";
      }
    if((*it).tag.size() > 0)
      {
        std::cout << "-" << (*it).tag.c_str() << " ";
      }
    std::vector<Field>::const_iterator itField = (*it).fields.begin();
    while(itField != (*it).fields.end())
      {
      if((*itField).type != FLAG) // only display the type if it's not a type
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
        std::cout << (*itField).description.c_str();
     
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

    if((*it).description.size()>0)
      {
      std::cout << " : " << (*it).description.c_str();
      }

    if(!(*it).required)
      {
      std::cout << "]";
      }
    std::cout << std::endl;
    it++;
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



/** Parse the command line */
bool MetaCommand::Parse(int argc, char* argv[])
{  
  // List the options if using -V
  if(argc == 2 && !strcmp(argv[1],"-V"))
    {
    this->ListOptions();
    return false;
    }
  // List the options if using -v
  else if(argc == 2 && !strcmp(argv[1],"-v"))
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

  // Fill in the results
  bool inArgument = false;
  std::string tag = "";
  std::string args;
  
  unsigned int currentField = 0; // current field position
  int currentOption = 0; // id of the option to fill
  unsigned int valuesRemaining=0;
  
  for(unsigned int i=1;i<(unsigned int)argc;i++)
    {
    // If this is a tag
    if(argv[i][0] == '-' && (atof(argv[i])==0))
      {
      // if we have a tag before the expected values we throw an exception
      if(valuesRemaining!=0)
        {
        std::cout << "Found tag before end of value list!" << std::endl;
        return false;
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
          if(m_OptionVector[currentOption].fields[0].type == FLAG)
            {
            // the tag exists by default
            m_OptionVector[currentOption].fields[0].value = "true"; 
            valuesRemaining = 0;
            inArgument = false;
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
    if(inArgument && i<(unsigned int)argc && valuesRemaining>0)
      {
      if(currentOption >=0 && currentOption < (int)(m_OptionVector.size()))
        {
        unsigned long s = m_OptionVector[currentOption].fields.size();
        m_OptionVector[currentOption].fields[s-valuesRemaining].value = argv[i];
        }
      valuesRemaining--;
      }

    if(valuesRemaining == 0)
      {
      inArgument = false;
      }
    
    }

  if(valuesRemaining>0)
    {
    std::cout << "Not enough parameters for " 
              << m_OptionVector[currentOption].name << std::endl;
    std::cout << "Type " << argv[0] 
              << " -v (or -V or -vxml) for more information" << std::endl;

    return false;
    }

  // Check if the options with required arguments are defined
  OptionVector::iterator it = m_OptionVector.begin();
  bool requiredAndNotDefined = false;
  while(it != m_OptionVector.end())
    {
    if((*it).required)
      {
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
          std::cout << "Option " << (*it).tag.c_str() << " is required but not defined" << std::endl;
          }
        else
          {
          std::cout << "Main field " << (*it).name.c_str() << " is required but not defined" << std::endl;
          }
        requiredAndNotDefined = true;
        }
      }
    it++;
    }

  if(requiredAndNotDefined)
    {
    std::cout << "Type " << argv[0] << " -v (or -V or -vxml) for more information" << std::endl;
    return false;
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
    case FLAG:
      return "flag";
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
  else if(!strcmp(type,"flag"))
    {
    return FLAG;
    }

  return INT; // by default

}
