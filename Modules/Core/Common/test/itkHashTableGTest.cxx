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

#include "itkGTest.h"

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

TEST(HashTable, StdHash)
{
  println("Testing std::hash");
  constexpr std::hash<const char *> H;
  // Note: std::hash<const char*> is pointer-based; values are platform-dependent
  std::cout << "foo -> " << H("foo") << std::endl;
  std::cout << "bar -> " << H("bar") << std::endl;

  constexpr std::hash<int> H1;
  std::cout << "1 -> " << H1(1) << std::endl;
  std::cout << "234 -> " << H1(234) << std::endl;
  // std::hash must return the same value for equal keys (standard guarantee)
  EXPECT_EQ(H1(1), H1(1));
  EXPECT_EQ(H1(234), H1(234));
  EXPECT_NE(H1(1), H1(234));

  constexpr std::hash<char> H2;
  std::cout << "a -> " << H2('a') << std::endl;
  std::cout << "Z -> " << H2('Z') << std::endl;
  // std::hash must return the same value for equal keys (standard guarantee)
  EXPECT_EQ(H2('a'), H2('a'));
  EXPECT_EQ(H2('Z'), H2('Z'));
  EXPECT_NE(H2('a'), H2('Z'));
}

TEST(HashTable, UnorderedSet)
{
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
  EXPECT_TRUE(Set.count("mango") > 0);
  lookup(Set, "apple");
  EXPECT_TRUE(Set.count("apple") > 0);
  lookup(Set, "durian");
  EXPECT_FALSE(Set.count("durian") > 0);

  // CppCheck gives us a warning if the return value isn't used.
  // This is to prevent the user from calling empty() when they mean clear().
  if (Set.empty())
  {
    std::cout << "Set is empty." << std::endl;
  }
  Set.rehash(50);
  Set.insert("the horror");
  auto              hsh_it = Set.begin();
  const HashSetType SetCopy = Set;
  IgnoreUnusedVariable(hsh_it);
}

TEST(HashTable, UnorderedMap)
{
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

  EXPECT_EQ(months["september"], 30);
  EXPECT_EQ(months["april"], 30);
  EXPECT_EQ(months["june"], 30);
  EXPECT_EQ(months["november"], 30);

  // CppCheck gives us a warning if the return value isn't used.
  // This is to prevent the user from calling empty() when they mean clear().
  if (months.empty())
  {
    std::cout << "Set is empty." << std::endl;
  }
  months.rehash(50);
  constexpr HashMapType::value_type p{ "psychotic break", 2 };
  months.insert(p);
  auto              map_it = months.begin();
  const HashMapType MapCopy = months;
  IgnoreUnusedVariable(map_it);
}
