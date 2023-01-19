/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <unordered_set>
#include <unordered_map>
#include <iostream>
#include <cstring>

/**
 * Helper function to prevent compiler's unused variable warning.
 */
template <typename T>
void
IgnoreUnusedVariable(const T &)
{}

struct eqstr
{
  bool
  operator()(const char * s1, const char * s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

void
lookup(const std::unordered_set<const char *, std::hash<const char *>, eqstr> & Set, const char * word)
{
  auto it = Set.find(word);
  std::cout << word << ": " << (it != Set.end() ? "present" : "not present") << std::endl;
}

inline void
println(const char * s)
{
  std::cout << std::endl << s << std::endl;
}

int
itkHashTableTest(int, char *[])
{
  println("Testing std::hash");
  std::hash<const char *> H;
  std::cout << "foo -> " << H("foo") << std::endl;
  std::cout << "bar -> " << H("bar") << std::endl;
  std::hash<int> H1;
  std::cout << "1 -> " << H1(1) << std::endl;
  std::cout << "234 -> " << H1(234) << std::endl;
  std::hash<char> H2;
  std::cout << "a -> " << H2('a') << std::endl;
  std::cout << "Z -> " << H2('Z') << std::endl;

  println("Testing std::unordered_set");
  using HashSetType = std::unordered_set<const char *, std::hash<const char *>, eqstr>;
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

  // CppCheck gives us a warning if the return value isn't used.
  // This is to prevent the user from calling empty() when they mean clear().
  if (Set.empty())
  {
    std::cout << "Set is empty." << std::endl;
  }
  Set.rehash(50);
  Set.insert("the horror");
  auto                        hsh_it = Set.begin();
  HashSetType::const_iterator hst_const_it;
  hst_const_it = Set.end();
  HashSetType SetCopy;
  SetCopy = Set;

  println("Testing std::unordered_map");
  using HashMapType = std::unordered_map<const char *, int, std::hash<const char *>, eqstr>;

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

  // CppCheck gives us a warning if the return value isn't used.
  // This is to prevent the user from calling empty() when they mean clear().
  if (months.empty())
  {
    std::cout << "Set is empty." << std::endl;
  }
  months.rehash(50);
  HashMapType::value_type p("psychotic break", 2);
  months.insert(p);
  auto                        map_it = months.begin();
  HashMapType::const_iterator map_const_it;
  map_const_it = months.end();
  HashMapType MapCopy;
  MapCopy = months;

  IgnoreUnusedVariable(hsh_it);
  IgnoreUnusedVariable(map_it);

  return EXIT_SUCCESS;
}
