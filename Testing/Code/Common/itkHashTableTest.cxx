/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHashTableTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itk_hashtable.h"
#include "itk_hash_set.h"
#include "itk_hash_map.h"
#include <iostream>

/**
 * Helper function to prevent compiler's unused variable warning.
 */
template <typename T>
void IgnoreUnusedVariable(const T&)
{
}

extern "C" {
#include "string.h"
}

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

void lookup(const itk::hash_set<const char*, itk::hash<const char*>, eqstr>& Set,
            const char* word)
{
  itk::hash_set<const char*, itk::hash<const char*>, eqstr>::const_iterator it
    = Set.find(word);
  std::cout << word << ": "
       << (it != Set.end() ? "present" : "not present")
       << std::endl;
}

inline void println(const char *s)
{ std::cout << std::endl << s << std::endl; }

int itkHashTableTest(int, char* [] )
{
  println("Testing itk::hash");
  itk::hash<const char*> H;
  std::cout << "foo -> " << H("foo") << std::endl;
  std::cout << "bar -> " << H("bar") << std::endl;
  itk::hash<int> H1;
  std::cout << "1 -> " << H1(1) << std::endl;
  std::cout << "234 -> " << H1(234) << std::endl;
  itk::hash<char> H2;
  std::cout << "a -> " << H2('a') << std::endl;
  std::cout << "Z -> " << H2('Z') << std::endl;
  
  println("Testing itk::hash_set");
  typedef itk::hash_set<const char*, itk::hash<const char*>, eqstr> HashSetType;
  HashSetType Set;
  Set.insert("kiwi");
  Set.insert("plum");
  Set.insert("apple");
  Set.insert("mango");
  Set.insert("apricot");
  Set.insert("banana");

  lookup(Set, "mango");
  lookup(Set, "apple");
  lookup(Set, "durian");

  Set.begin();
  Set.end();
  Set.size();
  Set.max_size();
  Set.empty();
  Set.bucket_count();
  Set.resize(50);
  Set.insert("the horror");
  Set.count("apple");
  Set.find("kiwi");
  HashSetType::iterator hsh_it = Set.begin();
  HashSetType::const_iterator hst_const_it;
  hst_const_it = Set.end();  
  HashSetType SetCopy;
  SetCopy = Set;
  //  SetCopy == Set; -- Removed until I can track down why the IRIX compiler
  //                     does not find this during link phase. cates 3/20/01
  
  
  println("Testing itk::hash_map");
  typedef itk::hash_map<const char*, int, itk::hash<const char*>, eqstr>
    HashMapType;
  
  HashMapType months;
  months["january"] = 31;
  months["february"] = 28;
  months["march"] = 31;
  months["april"] = 30;
  months["may"] = 31;
  months["june"] = 30;
  months["july"] = 31;
  months["august"] = 31;
  months["september"] = 30;
  months["october"] = 31;
  months["november"] = 30;
  months["december"] = 31;
  
  std::cout << "september -> " << months["september"] << std::endl;
  std::cout << "april     -> " << months["april"] << std::endl;
  std::cout << "june      -> " << months["june"] << std::endl;
  std::cout << "november  -> " << months["november"] << std::endl;

  months.begin();
  months.end();
  months.size();
  months.max_size();
  months.empty();
  months.bucket_count();
  months.resize(50);
  months.insert(std::pair<const char*, int>("psychotic break", 2));
  months.count("january");
  months.find("june");
  HashMapType::iterator map_it = months.begin();
  HashMapType::const_iterator map_const_it;
  map_const_it = months.end();  
  HashMapType MapCopy;
  MapCopy = months;
  //  MapCopy == months;  -- Removed until I can track down why IRIX compiler
  //                         does not find this during link phase. cates 3/20/01

  IgnoreUnusedVariable(hsh_it);
  IgnoreUnusedVariable(map_it);
  
  return 0;
}
