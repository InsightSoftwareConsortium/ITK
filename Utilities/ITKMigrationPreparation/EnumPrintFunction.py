#!/usr/bin/env python
# \author Mathew J. Seng
# This python script helps transition existing enumerations to ITKv5's strongly typed enumerations.

# This program will get specific information from the user to create an ITKv5's strongly typed enumerations.
# The prototype will be placed in a .csv file titled: enumoutput.csv in /ITK/Utilities/ITKv5Preparation/

# Usage: python3 EnumPrintFunction.py
# Questions:
#   1) Enter the name of the class the enumeration ORIGINALLY belonged to.
#   2) Enter name of the new enum class. This should NOT contain "enum" at the end.
#   3) List the enum constants of the enum class. Should be names separated by a comma. (i.e. ONE,TWO,THREE)
# Notes:
#   The below is assumed constants, thus refactoring any of these may be needed for a clean build.
#   1) Assumes in namespace itk.
#   2) Assumes ITK_Common_EXPORT for ostream declaration.
#   3) Assumes scope of enumeration is uint8_t.
#   4) Assigning number constants to enumeration class may be required.
#   5) Doxygen for enum class and new class must be defined.

import sys
import csv


def create_enum_class(housing_class, enum_class, enum_values):
    # Create new class containing enum class
    doxygen = "/**\\class \n * \\brief\n * \\ingroup\n*/\n"
    output = doxygen + "class " + housing_class + "\n{\n\tpublic:\n\t" + doxygen + "\t"
    output += "enum class " + enum_class + ": uint8_t\n{\n"
    for value in enum_values:
        output += "\t" + value + ",\n"
    output = output[:-2] + "\n};\n};\n\n"

    # Create ostream print function
    name = housing_class + "::" + enum_class
    output += (
        "//Define how to print enumeration\nextern ITKCommon_EXPORT std::ostream &operator<<(std::ostream & out, const "
        + name
        + " value);\n\n"
    )
    output += (
        "/** Print enum values */\nstd::ostream &\noperator<<(std::ostream & out, const "
        + name
        + " value)\n{\n\treturn out << [value] {\n\t\tswitch (value)\n\t\t{"
    )
    for value in enum_values:
        output += (
            "\n\t\t\tcase "
            + name
            + "::"
            + value
            + ':\n\t\t\t\treturn "itk::'
            + name
            + "::"
            + value
            + '";'
        )
    output += (
        '\n\t\t\tdefault: \n\t\t\t\treturn "INVALID VALUE FOR itk::'
        + name
        + '";\n\t\t}\n\t}();\n}\n\n'
    )

    # Create test function
    output += (
        "//Test streaming enumeration for "
        + name
        + " elements\n const std::set<itk::"
        + name
        + "> all"
        + name
        + "{\n"
    )
    for value in enum_values:
        output += "\titk::" + name + "::" + value + ",\n"
    output = (
        output[:-2]
        + "};\n for(const auto & ee : all"
        + name
        + ')\n{\n\tstd::cout << "STREAMED ENUM VALUE '
        + name
        + ': " << ee << std::endl;\n}\n'
    )

    # Dump data to enumoutput.csv
    with open("enumoutput.csv", "w") as file:
        file.write(output)
        file.close()


if __name__ == "__main__":
    print(
        "This will create a basic strongly typed enum class following the new ITKv5 migration guide.\nNOTE: Some refactoring will likely be needed."
    )
    class_containing_enum_class = (
        input("Enter class enum originally belongs to: ") + "Enums"
    )
    enum_class = input(
        "Enter the name of the enum class (should not contain enum at the end): "
    )
    enum_values = input("List all the enum constants (i.e. ONE,TWO,THREE): ").split(",")
    create_enum_class(class_containing_enum_class, enum_class, enum_values)
