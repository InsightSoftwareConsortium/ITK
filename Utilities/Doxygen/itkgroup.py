#!/usr/bin/env python
#
#  Copyright NumFOCUS
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#         https://www.apache.org/licenses/LICENSE-2.0.txt
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

"""
Usage: itkgroup.py <filename>"

Processes the input filename and prints to stdout.

The file has additional Doxygen grouping markers to doxygen comment blocks, where the block does not contain an empty
 line. This groups multiple functions or variables not separated by a newline, into a single doxygen group allowing
 the same doxyen comment to apply to all of them.

"""


import re
import sys

ingroup = False
semicount = 0
endbracecount = 0
endparencount = 0
leading_space = "  "
savebuffer = ""


def process_line(line: str):
    global ingroup, semicount, endbracecount, endparencount, leading_space, savebuffer

    line = line.rstrip()
    if re.search(r"\S+", line):
        match = re.search(r"/\*\*(.*)", line)
        if match:
            if ingroup:
                print(f"{leading_space}/**{savebuffer}")
            if re.search(r"(\\class|\\brief)", line):
                print(line)
            else:
                savebuffer = f"{match.group(1)}\n"
                ingroup = True
                semicount = 0
                endbracecount = 0
                endparencount = 0
                leading_space = re.match(r"(^\s*)", line).group(1)
        else:
            if ingroup:
                savebuffer += f"{line}\n"
            else:
                print(line)
        if re.search(r";", line):
            semicount += 1
        if re.search(r"\}", line):
            endbracecount += 1
        if re.search(r"\)", line):
            endparencount += 1
    else:
        if ingroup:
            if endparencount > 1 and (semicount > 1 or endbracecount > 1):
                print(f"{leading_space}/**@{{{savebuffer}{leading_space}/**@}}*/\n")
            else:
                print(f"{leading_space}/**{savebuffer}")
            savebuffer = ""
            ingroup = False
        else:
            print(line)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python itkgroup.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]
    with open(filename) as file:
        for line in file:
            process_line(line)
