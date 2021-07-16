from pydoc import render_doc
from html.parser import HTMLParser
from urllib.request import urlopen
from textwrap import fill
from difflib import unified_diff
from itk.support.template_class import itkTemplate as _itkTemplate
import itk


YELLOW = "\033[93m"
CYAN = "\033[96m"
BOLD = "\033[1m"
END = "\033[0m"


# Inefficient, needs updating
def generate_typehints(itk_class_template: _itkTemplate, verbose: bool = False, file_name: str = None):
    """Generate typehints for a given itkTemplate class.
        These typehints include Unions for parameters that vary by class type.
        This method only works if the class can be instantiated.
        :param: verbpse: Prints the output to the console when True.
        :param: file_name: Saves the output to a file when given."""
    is_typed = hasattr(itk_class_template, "keys")
    has_new_method = True
    try:
        itk_class_template.New()
    except:
        has_new_method = False

    if not is_typed:
        class_object = None
        if has_new_method:
            class_object = itk_class_template.New()
        else:
            class_object = itk_class_template()

        definitions = get_method_definitions(class_object)
        typehints = parse_defs(definitions, verbose=False, inherited_methods=False)
        return typehints

    keys = list(itk_class_template.keys())
    root = None
    if has_new_method:
        root = itk_class_template[keys[0]].New()
    else:
        root = itk_class_template[keys[0]]()

    root_defs = get_method_definitions(root)
    root_hints = parse_defs(root_defs, verbose=False, inherited_methods=False)
    result = root_hints
    differences = {}
    for key in keys[1:]:
        typed_class_object = None
        if has_new_method:
            typed_class_object = itk_class_template[key].New()
        else:
            typed_class_object = itk_class_template[key]()

        key_defs = get_method_definitions(typed_class_object)
        key_hints = parse_defs(key_defs, verbose=False, inherited_methods=False)

        diffs = list(
            unified_diff(
                root_hints.split("\n"), key_hints.split("\n"), lineterm="", n=0
            )
        )[2:]
        for diff in range(int(len(diffs) / 3)):
            diff = diff * 3
            root_diff = diffs[diff + 1]
            root_diff = root_diff[root_diff.find("(") :]
            key_diff = diffs[diff + 2]
            if "GetNthComponent" in key_diff:
                print(key_diff)
            key_diff = key_diff[key_diff.find("(") :]

            line = (
                    diffs[diff + 1][1:] + "\n"
            )  # remove +/- symbol from beginning add back newline

            root_diff_params = root_diff[1 : (root_diff.find(")"))].split(", ") + [
                root_diff.split(" -> ")[-1].replace(":", "")
            ]

            key_diff_params = key_diff[1 : (key_diff.find(")"))].split(", ") + [
                key_diff.split(" -> ")[-1].replace(":", "")
            ]

            for i in range(len(root_diff_params)):
                if (
                        root_diff_params[i].split(": ")[-1]
                        != key_diff_params[i].split(": ")[-1]
                ):
                    if line not in differences:
                        differences[line] = {}
                    if root_diff_params[i].split(": ")[-1] not in differences[line]:
                        differences[line][root_diff_params[i].split(": ")[-1]] = [
                            key_diff_params[i].split(": ")[-1]
                        ]
                    elif (
                            key_diff_params[i].split(": ")[-1]
                            not in differences[line][root_diff_params[i].split(": ")[-1]]
                    ):
                        differences[line][root_diff_params[i].split(": ")[-1]].append(
                            key_diff_params[i].split(": ")[-1]
                        )

    for line in differences.keys():
        original_value = line
        for diff in differences[original_value].keys():
            replacement = (
                    "Union[" + ", ".join([diff] + differences[original_value][diff]) + "]"
            )
            line = line.replace(diff, replacement)
        result = result.replace(original_value, line)

    # Write gathered data to the output file
    if file_name is not None:
        text_file = open(file_name, "w")
        text_file.write(result)
        text_file.close()
        print(f"Results written to {file_name}")
    if verbose:
        print(result)
    return result


def get_method_definitions(itk_object):
    """return a dictionary of useful information regarding the passed object.
        This includes method name, the object's parent class, and method headers."""
    inheritance_tree = {}
    str_help = render_doc(itk_object, "Help on %s")
    str_help = str_help.replace(" |  ", "")
    str_help = str_help.split("-" * 70 + "\n")

    # save the name of the object's parent
    inheritance_list = (
        str_help[0][
        str_help[0]
            .find("Method resolution order:") : str_help[0]
            .find("Methods defined here:\n")
        ]
            .strip()
            .split("\n")
    )
    if len(inheritance_list) > 2:
        inheritance_tree["__parent__"] = python_type(
            inheritance_list[2].split(".")[-1].strip(), typed=False
        )
    else:
        inheritance_tree["__parent__"] = ""

    inheritance_tree["object"] = itk_object  # save the object that was used
    inheritance_tree["__headers__"] = {}  # saves complete method headers

    # The first block is formatted differently, correct it.
    str_help[0] = str_help[0][str_help[0].find("Methods defined here:\n") :]

    # iterate through each section
    for section in str_help:
        section_name = section.split("\n")[0]
        section_methods = (
            section.replace(section.split("\n")[0], "", 1).strip("\n\n").split("\n\n")
        )

        inheritance_tree[section_name] = {}
        for method in section_methods:
            # There are three forms of methods
            # Clone = itkMedianImageFilterISS2ISS2_Clone(...)       | This form always has " = " and "("
            # New(self, *args, **kwargs)                            | This form has ()'s (but not the above)
            # thisown                                               | If not one of the above, it is not a method
            # We want to ignore methods with __'s

            method_header = method.splitlines()[0]
            if "__" in method_header:
                continue
            elif " = " in method_header and "(" in method_header:
                name = correct_name(method_header.split(" = ")[0])
                inheritance_tree[section_name][name] = method.replace(
                    method_header, "", 1
                )
                inheritance_tree["__headers__"][name + "_header"] = method_header
            elif "(" in method_header:
                name = correct_name(method_header.split("(")[0])
                inheritance_tree[section_name][name] = method.replace(
                    method_header, "", 1
                )
                inheritance_tree["__headers__"][name + "_header"] = method_header
            else:
                continue

        # remove the section if no useful methods exist within it
        # occurs when section contained only attributes or __'ed methods
        if inheritance_tree[section_name] == {}:
            del inheritance_tree[section_name]

    return inheritance_tree


def correct_name(n):
    """# correct name formatting produced by the help function so that string compares will work properly
    i.e.    C\x08Cl\x08lo\x08on\x08ne\x08e -> Clone"""
    name = ""
    if "\x08" in n:
        temp_name = n.replace("\x08", "")
        index = 0
        while index < len(temp_name):
            name = name + temp_name[index]
            index = index + 2
    return name


def parse_defs(
        inheritance_tree,
        inherited_methods=False,
        method_name=None,
        verbose=True,
        file_name=None,
        _site_data=None,
        _section_name=None,
):
    """
    Parse the help text provided for method definitions to provide complete typehinted method definitions
        with Doxygen comments to be used for .pyi files.
    :param inheritance_tree: The dictionary of method definitions produced by the method 'getMethodDefs'
    :param inherited_methods: If True, will also return method definitions for all inherited class methods
    :param method_name: When specified, will only return the method definition for a method of the given name
    :param verbose: When True, will print the generated code to the console.
    :param file_name: When given, will save the generated code to the specified file.
    :param _site_data: For internal recursive use only. Preserves Doxygen website info to allow fewer calls to be made.
    :param _section_name: For internal recursive use only. Used to iterate between help text sections.
    :return: A string containing the class and method typehints.
    """

    if file_name is not None:
        text_file = open(file_name, "w")

    output = ""
    if _section_name is None:
        for section in inheritance_tree.keys():
            # skip non-class sections (this is manually added infomation)
            if section in ["object", "__headers__", "__parent__"]:
                continue
            # output is part of the class object
            if "defined here" in section:
                output += parse_defs(
                    inheritance_tree,
                    method_name=method_name,
                    verbose=verbose,
                    _section_name=section,
                )
            # other sections contain inherited methods
            elif inherited_methods:
                if verbose:
                    print(BOLD + YELLOW + "###" + section + END)
                output += "###" + section + "\n"
                output += parse_defs(
                    inheritance_tree,
                    method_name=method_name,
                    verbose=verbose,
                    _section_name=section,
                )

        # Write gathered data to the output file
        if file_name is not None:
            text_file.write(output)
            text_file.close()
            print(f"Results written to {file_name}")
        return output

    # Gather the name of the class that this section represents
    method_class = ""
    if "defined here" in _section_name:
        method_class = python_type(
            type(inheritance_tree["object"]).__name__, typed=False
        )
    else:
        method_class = python_type(
            _section_name.strip().replace(":", "").split(".")[-1], typed=False
        )

    # site data passed to recursive method calls to reduce number of web requests and lower runtime
    if _site_data is None:
        web_url = urlopen(
            f"https://itk.org/Doxygen/html/classitk_1_1"
            f'{python_type("itk" + method_class, typed=False)}.html'
        )
        _site_data = web_url.read().decode("utf-8")

        # add the class definition here as this will only be entered once per class
        if (
                _section_name == "Methods defined here:"
        ):  # the beginning of the class definition
            class_header = generate_class_header(
                method_class, _site_data, inheritance_tree["__parent__"]
            )
            if verbose:
                print(class_header)
            output += class_header

    # display all methods if one is not given
    if method_name is None:
        for method in inheritance_tree[_section_name].keys():
            output += parse_defs(
                inheritance_tree,
                method_name=method,
                verbose=verbose,
                _site_data=_site_data,
                _section_name=_section_name,
            )
        if file_name is not None:
            text_file.write(output)
            text_file.close()
            print(f"Results written to {file_name}")
        return output
    elif method_name not in inheritance_tree[_section_name].keys():
        return ""

    method_body = inheritance_tree[_section_name][method_name]

    # strange formatting case. Help text does not provide enough info, so we don't know the types
    if len(method_body) == 0 or method_name + "(" not in method_body:
        data = get_doxygen_comment(_site_data, method_name)
        header = inheritance_tree["__headers__"][method_name + "_header"]
        header = (
                correct_name(header[: header.rfind("\x08") + 2])
                + header[header.rfind("\x08") + 2 :]
        )
        output = (
            f"    def {header} -> Any:\n" f'        """{data}"""\n' f"        ...\n\n"
        )
        if "static" in _section_name or "Static" in _section_name:
            output = "@staticmethod\n" + output
        if verbose:
            print(output)
        if file_name is not None:
            text_file.write(output)
            text_file.close()
            print(f"Results written to {file_name}")
        return output

    # The New method has a comment block that creates problems.
    # reduce to only the first line
    if method_name == "New":
        method_body = method_body.strip().split("\n")[0]

    # methods may be overloaded. Create a definition for each
    defs = method_body.strip().split(method_name)
    result = ""
    for definition in defs:
        params = definition.split("\n")[0]
        if definition == "":
            continue
        definition = definition[
                     (
                             definition.find("    Parameters\n    ----------\n")
                             + len("    Parameters\n    ----------\n")
                     ) :
                     ]

        """        for parm in definition.split("\n"):
            parm = parm.replace("    ", "")
            params = params.replace(parm.split(":")[0].replace("\t", ""), parm)"""

        params = convert_type_hints(params, definition.split("\n"))
        data = get_doxygen_comment(_site_data, method_name)
        result = (
            f"    def {method_name}{params}:\n"
            f'        """{data}"""\n'
            f"        ...\n"
        )
        if "static" in _section_name or "Static" in _section_name:
            result = "    @staticmethod\n" + result
        if verbose:
            print(result)
        output += result + "\n"

    if file_name is not None:
        text_file.write(output)
        text_file.close()
        print(f"Results written to {file_name}")

    return output


def generate_class_header(class_name, site_data, parent):
    """Return a string containing the definition of a pyi class header. Supports both typed and non-typed classes."""

    # test if class is typed
    try:
        eval(f"itk.{class_name}.keys()")
    except:
        return f"class {class_name}({parent}):\n"

    # gather generic type names as stated in Doxygen documentation
    types = site_data[site_data.find('<div class="title">') :]
    types = types[: types.find("\n")]
    parser = DefinitionParser()
    parser.feed(types)
    types = parser.dataList
    types = types[types.find("< ") + 2 : types.find(" >")]
    has_new_method = "New()" in site_data

    class_header = (
            f"class _{class_name}():\n"
            f"    def __getitem__(self, parameters) -> {class_name}:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {class_name}\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class {class_name}(_itkTemplate, metaclass=_{class_name}):\n"
            f'    """Interface for instantiating itk::{class_name}< {types} >\n'
            + has_new_method
            * f"        Create a new {class_name} Object (of default type):\n"
              f"            'itk.{class_name}.New()\n"
            + f"        Supports type specification through dictionary access:\n"
              f"            'itk.{class_name}[{types}]{'.New()' if has_new_method else '()'}\"\"\"\n"
              f"\n"
              f"    @staticmethod\n"
              f"    def {'New(self)' if has_new_method else '__init__()'} -> Typed{class_name}PROXY:\n"
              f'        """Instantiate itk::{class_name}< {types} >"""\n'
              f"        ...\n"
              f"\n"
              f"\n"
              f"class Typed{class_name}PROXY({parent}):\n"
    )
    return class_header


def get_doxygen_comment(site_data, method_name):
    """parse the decription of an itk method given html doxygen data.
    This heavily relies on the structure of the webpage."""
    data = site_data[site_data.find(f">{method_name}()") :]
    data = data[(data.find('<div class="memdoc">') + 21) :]
    data = data[: data.find("</div>")]

    # remove undesired portions of description
    data = data[: data.find("\n<p>Reimplemented ")]
    data = data[: data.find('\n<p class="reference">Referenced by ')]
    data = data[: data.find('\n<p class="definition">Definition at')]
    data = data[: data.find('\n<dl class="section examples"><dt>Examples')]

    # parse the html data
    parser = DefinitionParser()
    parser.feed(data)
    data = parser.dataList
    data = fill(data, 100, subsequent_indent=" " * 8, break_long_words=True)
    return data


def convert_type_hints(params, param_hints):
    """Given a list of C-Type parameters, convert them to their typehinted Python equivalent.
    Will not work properly if the conversion has not already been defined below."""
    if "->" not in params:
        params = params + " -> None"

    seperated_params = params[1 : (params.find(")"))].split(", ") + [
        params.split(" -> ")[-1]
    ]
    result = "("
    index = 0
    for param in seperated_params[:-1]:
        if param == "":
            continue

        if param == "self":
            result = result + param + ", "
            continue

        split_param = [param, param_hints[index].replace("    ", "")]
        index += 1

        if len(split_param) <= 1:
            # no hint available
            result = result + param + ", "
        else:
            variable = split_param[0].strip()
            typehint = split_param[1].strip().split(": ")[-1]
            if "=" in variable:
                default = variable.split("=")[1].strip()
                typehint = python_type(typehint)
                variable = variable.split("=")[0].strip()
                result = result + variable + ": " + typehint + " = " + default + ", "
            else:
                typehint = python_type(typehint)
                result = result + variable + ": " + typehint + ", "

    # correct for extra comma and add return type
    if result[-2:] == ", ":
        result = result[:-2]
    result = result + ") -> " + python_type(seperated_params[-1])
    return result


# must know the first and last class in the sight
def generate_itk_class_typehint_conversion_code(first_class, last_class):
    """Generate the python code to convert C-typehints to Python Typed PROXY classes.
    Can be used to update the 'python_type' method when a new class is added.
    The name of the first and last class in the documentation must be known.
    Find it here: https://itk.org/Doxygen/html/classes.html"""
    web_url = urlopen("https://itk.org/Doxygen/html/classes.html")
    data = web_url.read().decode("utf-8")
    parser = ClassNameHTMLParser()
    parser.feed(data)
    parsed_data = parser.dataList
    parsed_data.reverse()  # reverse array. This ensures that longer classes with similar names go before shorted ones
    text_file = open("help_text.txt", "w")
    help_text = ""

    start = False
    end = False
    index = 0
    while not end:
        itk_class = parsed_data[index]
        if index >= len(parsed_data) and not start:
            print("Start class not found!")
            break
        if (
                itk_class == "itk" or "itk::" in itk_class
        ):  # skip the portion in parenthesis
            index = index + 1
            continue

        itk_class = itk_class.split("::")[-1]

        if start:
            help_text += (
                f'elif "itk{itk_class}" in typehint:\n'
                f'    hint = "Typed{itk_class}PROXY"\n'
            )
            index = index + 1
            if itk_class == first_class:
                end = True
        else:
            if itk_class == last_class:
                start = True
            else:
                index = index + 1
    text_file.write(help_text)
    text_file.close()


# https://docs.python.org/3/library/html.parser.html
class ClassNameHTMLParser(HTMLParser):
    """Parse itk class names from doxygen class data."""
    def __init__(self):
        HTMLParser.__init__(self)
        self.dataList = []

    def handle_data(self, data):
        data = data.strip()
        # length check prevents adding of alphabet labels
        if data not in ["", "(", ")"] and len(data) > 1:
            self.dataList.append(data)


class DefinitionParser(HTMLParser):
    """Parse generic HTML data and save it to the string dataList"""
    def __init__(self):
        HTMLParser.__init__(self)
        self.dataList = ""

    def handle_data(self, data):
        self.dataList += data


# This needs to be updated!
def python_type(typehint, typed=True):
    """Convert C-types to python types.
    Not all possible C-types are here, this code must be edited when new types appear.
    Types that do not have a conversion will remain as their c-type but will
        display as blue when printed in the console.
    Autogenerated code is used to convert itk class types to the python Typed PROXY classes"""
    typehint = typehint.replace("std::", "")
    typehint = typehint.replace("itk::", "itk")

    # removes anything after name if typed to prevent errors with in usage
    # e.g.
    # itk::NeighborhoodAccessorFunctor< itk::Image< itk::RGBPixel< ...
    # becomes
    # NeighborhoodAccessorFunctor
    if typehint.find("<") != -1:
        typehint = typehint[: typehint.find("<")]

    hint = ""
    # basic types
    if "self" == typehint:
        hint = "self"
    elif "None" == typehint:
        hint = "None"
    elif typehint in ["bool", "bool const", "bool const &"]:
        hint = "bool"
    elif typehint in [
        "int",
        "int &",
        "int const",
        "int const &",
        "unsigned int",
        "unsigned int &",
        "unsigned int const",
        "unsigned int const &",
        "unsigned long",
        "unsigned long const &",
        "unsigned long const",
        "long",
        "short",
        "short const &",
    ]:
        hint = "int"
    elif typehint in ["float", "float const", "float const *", "float const &"]:
        hint = "float"
    elif typehint in ["double", "double const", "double const *", "double const &"]:
        hint = "float"  # Python floats are double precision
    elif typehint in ["string", "string const", "string const &"]:
        hint = "str"
    elif typehint in ["vectorstring"]:
        hint = "Tuple[string, ...]"

    # Non-typed itk classes
    # These must override the autogenerated code below that adds the Typed PROXY wording
    elif "itkLightObject" in typehint:  # be careful with usage of in in this way
        hint = "LightObject"
    elif "itkDataObject" in typehint:
        hint = "DataObject"
    elif "itkProcessObject" in typehint:
        hint = "ProcessObject"
    elif "itkImageSource" in typehint:
        hint = "ImageSource"
    elif "itkImageToImageFilter" in typehint:
        hint = "ImageToImageFilter"
    elif "itkBoxImageFilter" in typehint:
        hint = "BoxImageFilter"
    elif "itkMetaDataDictionary" in typehint:
        hint = "MetaDataDictionary"
    elif "itkRealTimeStamp" in typehint:
        hint = "RealTimeStamp"
    elif "itkCommand " in typehint:
        hint = "Command"
    elif "itkEventObject" in typehint:
        hint = "EventObject"

    # typed itk classes
    # it may not be desirable for some of the following classes to be Typed PROXYs
    # to override their definition here, create a separate case above
    # START AUTOGENERATED CODE
    elif "itkLightObject" in typehint:  # be careful with usage of in in this way
        hint = "LightObject"
    elif "itkDataObject" in typehint:
        hint = "DataObject"
    elif "itkProcessObject" in typehint:
        hint = "ProcessObject"
    elif "itkImageSource" in typehint:
        hint = "ImageSource"
    elif "itkImageToImageFilter" in typehint:
        hint = "ImageToImageFilter"
    elif "itkBoxImageFilter" in typehint:
        hint = "BoxImageFilter"
    elif "itkMetaDataDictionary" in typehint:
        hint = "MetaDataDictionary"
    elif "itkRealTimeStamp" in typehint:
        hint = "RealTimeStamp"
    elif "itkCommand " in typehint:
        hint = "Command"
    elif "itkEventObject" in typehint:
        hint = "EventObject"

    # typed itk classes
    # it may not be desirable for some of the following classes to be Typed PROXYs
    # to override their definition here, create a separate case above
    # There are many typed forms of each class. This works around that.
    # Lists of objects etc would not be processed properly
    # START AUTOGENERATED CODE
    elif "itkZeroIndex" in typehint:
        hint = "TypedZeroIndexPROXY"
    elif "itkZeroFluxNeumannPadImageFilter" in typehint:
        hint = "TypedZeroFluxNeumannPadImageFilterPROXY"
    elif "itkZeroFluxNeumannImageNeighborhoodPixelAccessPolicy" in typehint:
        hint = "TypedZeroFluxNeumannImageNeighborhoodPixelAccessPolicyPROXY"
    elif "itkZeroFluxNeumannBoundaryCondition" in typehint:
        hint = "TypedZeroFluxNeumannBoundaryConditionPROXY"
    elif "itkZeroCrossingImageFilter" in typehint:
        hint = "TypedZeroCrossingImageFilterPROXY"
    elif "itkZeroCrossingBasedEdgeDetectionImageFilter" in typehint:
        hint = "TypedZeroCrossingBasedEdgeDetectionImageFilterPROXY"
    elif "itkYenThresholdImageFilter" in typehint:
        hint = "TypedYenThresholdImageFilterPROXY"
    elif "itkYenThresholdCalculator" in typehint:
        hint = "TypedYenThresholdCalculatorPROXY"
    elif "itkXorImageFilter" in typehint:
        hint = "TypedXorImageFilterPROXY"
    elif "itkXorC" in typehint:
        hint = "TypedXorCPROXY"
    elif "itkXor" in typehint:
        hint = "TypedXorPROXY"
    elif "itkXOR" in typehint:
        hint = "TypedXORPROXY"
    elif "itkXMLWriterBase" in typehint:
        hint = "TypedXMLWriterBasePROXY"
    elif "itkXMLReaderBase" in typehint:
        hint = "TypedXMLReaderBasePROXY"
    elif "itkXMLReader" in typehint:
        hint = "TypedXMLReaderPROXY"
    elif "itkXMLFilterWatcher" in typehint:
        hint = "TypedXMLFilterWatcherPROXY"
    elif "itkXMLFileOutputWindow" in typehint:
        hint = "TypedXMLFileOutputWindowPROXY"
    elif "itkWrapPadImageFilter" in typehint:
        hint = "TypedWrapPadImageFilterPROXY"
    elif "itkWorkUnitInfo" in typehint:
        hint = "TypedWorkUnitInfoPROXY"
    elif "itkWorkUnitData" in typehint:
        hint = "TypedWorkUnitDataPROXY"
    elif "itkWisdomFilenameGeneratorBase" in typehint:
        hint = "TypedWisdomFilenameGeneratorBasePROXY"
    elif "itkWinterColormapFunction" in typehint:
        hint = "TypedWinterColormapFunctionPROXY"
    elif "itkWindowedSincInterpolateImageFunction" in typehint:
        hint = "TypedWindowedSincInterpolateImageFunctionPROXY"
    elif "itkWindowConvergenceMonitoringFunction" in typehint:
        hint = "TypedWindowConvergenceMonitoringFunctionPROXY"
    elif "itkWienerDeconvolutionImageFilter" in typehint:
        hint = "TypedWienerDeconvolutionImageFilterPROXY"
    elif "itkWienerDeconvolutionFunctor" in typehint:
        hint = "TypedWienerDeconvolutionFunctorPROXY"
    elif "itkWhiteTopHatImageFilter" in typehint:
        hint = "TypedWhiteTopHatImageFilterPROXY"
    elif "itkWhitakerSparseLevelSetImage" in typehint:
        hint = "TypedWhitakerSparseLevelSetImagePROXY"
    elif "itkWelchWindowFunction" in typehint:
        hint = "TypedWelchWindowFunctionPROXY"
    elif "itkWeightEnum" in typehint:
        hint = "TypedWeightEnumPROXY"
    elif "itkWeightedPrincipalMomentsLabelObjectAccessor" in typehint:
        hint = "TypedWeightedPrincipalMomentsLabelObjectAccessorPROXY"
    elif "itkWeightedPrincipalAxesLabelObjectAccessor" in typehint:
        hint = "TypedWeightedPrincipalAxesLabelObjectAccessorPROXY"
    elif "itkWeightedMeanSampleFilter" in typehint:
        hint = "TypedWeightedMeanSampleFilterPROXY"
    elif "itkWeightedFlatnessLabelObjectAccessor" in typehint:
        hint = "TypedWeightedFlatnessLabelObjectAccessorPROXY"
    elif "itkWeightedElongationLabelObjectAccessor" in typehint:
        hint = "TypedWeightedElongationLabelObjectAccessorPROXY"
    elif "itkWeightedCovarianceSampleFilter" in typehint:
        hint = "TypedWeightedCovarianceSampleFilterPROXY"
    elif "itkWeightedCentroidKdTreeGenerator" in typehint:
        hint = "TypedWeightedCentroidKdTreeGeneratorPROXY"
    elif "itkWeightedAddImageFilter" in typehint:
        hint = "TypedWeightedAddImageFilterPROXY"
    elif "itkWeightedAdd2" in typehint:
        hint = "TypedWeightedAdd2PROXY"
    elif "itkWeakPointer" in typehint:
        hint = "TypedWeakPointerPROXY"
    elif "itkWatershedMiniPipelineProgressCommand" in typehint:
        hint = "TypedWatershedMiniPipelineProgressCommandPROXY"
    elif "itkWatershedImageFilter" in typehint:
        hint = "TypedWatershedImageFilterPROXY"
    elif "itkWarpVectorImageFilter" in typehint:
        hint = "TypedWarpVectorImageFilterPROXY"
    elif "itkWarpMeshFilter" in typehint:
        hint = "TypedWarpMeshFilterPROXY"
    elif "itkWarpImageFilter" in typehint:
        hint = "TypedWarpImageFilterPROXY"
    elif "itkWarpHarmonicEnergyCalculator" in typehint:
        hint = "TypedWarpHarmonicEnergyCalculatorPROXY"
    elif "itkVXLVideoIOFactory" in typehint:
        hint = "TypedVXLVideoIOFactoryPROXY"
    elif "itkVXLVideoIO" in typehint:
        hint = "TypedVXLVideoIOPROXY"
    elif "itkVTKPolyDataWriter" in typehint:
        hint = "TypedVTKPolyDataWriterPROXY"
    elif "itkVTKPolyDataReader" in typehint:
        hint = "TypedVTKPolyDataReaderPROXY"
    elif "itkVTKPolyDataMeshIOFactory" in typehint:
        hint = "TypedVTKPolyDataMeshIOFactoryPROXY"
    elif "itkVTKPolyDataMeshIO" in typehint:
        hint = "TypedVTKPolyDataMeshIOPROXY"
    elif "itkVTKImageToImageFilter" in typehint:
        hint = "TypedVTKImageToImageFilterPROXY"
    elif "itkVTKImageIOFactory" in typehint:
        hint = "TypedVTKImageIOFactoryPROXY"
    elif "itkVTKImageIO" in typehint:
        hint = "TypedVTKImageIOPROXY"
    elif "itkVTKImageImport" in typehint:
        hint = "TypedVTKImageImportPROXY"
    elif "itkVTKImageExportBase" in typehint:
        hint = "TypedVTKImageExportBasePROXY"
    elif "itkVTKImageExport" in typehint:
        hint = "TypedVTKImageExportPROXY"
    elif "itkvtkCaptureScreen" in typehint:
        hint = "TypedvtkCaptureScreenPROXY"
    elif "itkVoxBoCUBImageIOFactory" in typehint:
        hint = "TypedVoxBoCUBImageIOFactoryPROXY"
    elif "itkVoxBoCUBImageIO" in typehint:
        hint = "TypedVoxBoCUBImageIOPROXY"
    elif "itkVotingBinaryIterativeHoleFillingImageFilter" in typehint:
        hint = "TypedVotingBinaryIterativeHoleFillingImageFilterPROXY"
    elif "itkVotingBinaryImageFilter" in typehint:
        hint = "TypedVotingBinaryImageFilterPROXY"
    elif "itkVotingBinaryHoleFillingImageFilter" in typehint:
        hint = "TypedVotingBinaryHoleFillingImageFilterPROXY"
    elif "itkVoronoiSegmentationRGBImageFilter" in typehint:
        hint = "TypedVoronoiSegmentationRGBImageFilterPROXY"
    elif "itkVoronoiSegmentationImageFilterBase" in typehint:
        hint = "TypedVoronoiSegmentationImageFilterBasePROXY"
    elif "itkVoronoiSegmentationImageFilter" in typehint:
        hint = "TypedVoronoiSegmentationImageFilterPROXY"
    elif "itkVoronoiPartitioningImageFilter" in typehint:
        hint = "TypedVoronoiPartitioningImageFilterPROXY"
    elif "itkVoronoiEdge" in typehint:
        hint = "TypedVoronoiEdgePROXY"
    elif "itkVoronoiDiagram2DGenerator" in typehint:
        hint = "TypedVoronoiDiagram2DGeneratorPROXY"
    elif "itkVoronoiDiagram2D" in typehint:
        hint = "TypedVoronoiDiagram2DPROXY"
    elif "itkVolumeSplineKernelTransform" in typehint:
        hint = "TypedVolumeSplineKernelTransformPROXY"
    elif "itkVNLSparseLUSolverTraits" in typehint:
        hint = "TypedVNLSparseLUSolverTraitsPROXY"
    elif "itkVnlRealToHalfHermitianForwardFFTImageFilter" in typehint:
        hint = "TypedVnlRealToHalfHermitianForwardFFTImageFilterPROXY"
    elif "itkVNLIterativeSparseSolverTraits" in typehint:
        hint = "TypedVNLIterativeSparseSolverTraitsPROXY"
    elif "itkVnlInverseFFTImageFilter" in typehint:
        hint = "TypedVnlInverseFFTImageFilterPROXY"
    elif "itkVnlHalfHermitianToRealInverseFFTImageFilter" in typehint:
        hint = "TypedVnlHalfHermitianToRealInverseFFTImageFilterPROXY"
    elif "itkVnlForwardFFTImageFilter" in typehint:
        hint = "TypedVnlForwardFFTImageFilterPROXY"
    elif "itkVnlFFTTransform" in typehint:
        hint = "TypedVnlFFTTransformPROXY"
    elif "itkVnlFFTCommon" in typehint:
        hint = "TypedVnlFFTCommonPROXY"
    elif "itkVnlComplexToComplexFFTImageFilter" in typehint:
        hint = "TypedVnlComplexToComplexFFTImageFilterPROXY"
    elif "itkVMMapSummaryRecord" in typehint:
        hint = "TypedVMMapSummaryRecordPROXY"
    elif "itkVMMapRecord" in typehint:
        hint = "TypedVMMapRecordPROXY"
    elif "itkVMMapFileParser" in typehint:
        hint = "TypedVMMapFileParserPROXY"
    elif "itkVMMapData_10_2" in typehint:
        hint = "TypedVMMapData_10_2PROXY"
    elif "itkViewImage" in typehint:
        hint = "TypedViewImagePROXY"
    elif "itkvidl_itk_istream" in typehint:
        hint = "Typedvidl_itk_istreamPROXY"
    elif "itkVideoToVideoFilter" in typehint:
        hint = "TypedVideoToVideoFilterPROXY"
    elif "itkVideoStream" in typehint:
        hint = "TypedVideoStreamPROXY"
    elif "itkVideoSource" in typehint:
        hint = "TypedVideoSourcePROXY"
    elif "itkVideoIOFactoryEnums" in typehint:
        hint = "TypedVideoIOFactoryEnumsPROXY"
    elif "itkVideoIOFactory" in typehint:
        hint = "TypedVideoIOFactoryPROXY"
    elif "itkVideoIOBaseEnums" in typehint:
        hint = "TypedVideoIOBaseEnumsPROXY"
    elif "itkVideoIOBase" in typehint:
        hint = "TypedVideoIOBasePROXY"
    elif "itkVideoFileWriter" in typehint:
        hint = "TypedVideoFileWriterPROXY"
    elif "itkVideoFileReader" in typehint:
        hint = "TypedVideoFileReaderPROXY"
    elif "itkVertexHash" in typehint:
        hint = "TypedVertexHashPROXY"
    elif "itkVertexCell" in typehint:
        hint = "TypedVertexCellPROXY"
    elif "itkVersorTransformOptimizer" in typehint:
        hint = "TypedVersorTransformOptimizerPROXY"
    elif "itkVersorTransform" in typehint:
        hint = "TypedVersorTransformPROXY"
    elif "itkVersorRigid3DTransformOptimizer" in typehint:
        hint = "TypedVersorRigid3DTransformOptimizerPROXY"
    elif "itkVersorRigid3DTransform" in typehint:
        hint = "TypedVersorRigid3DTransformPROXY"
    elif "itkVersor" in typehint:
        hint = "TypedVersorPROXY"
    elif "itkVersion" in typehint:
        hint = "TypedVersionPROXY"
    elif "itkVelocityFieldTransform" in typehint:
        hint = "TypedVelocityFieldTransformPROXY"
    elif "itkVectorToRGBPixelAccessor" in typehint:
        hint = "TypedVectorToRGBPixelAccessorPROXY"
    elif "itkVectorToRGBImageAdaptor" in typehint:
        hint = "TypedVectorToRGBImageAdaptorPROXY"
    elif "itkVectorThresholdSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedVectorThresholdSegmentationLevelSetImageFilterPROXY"
    elif "itkVectorThresholdSegmentationLevelSetFunction" in typehint:
        hint = "TypedVectorThresholdSegmentationLevelSetFunctionPROXY"
    elif "itkVectorRescaleIntensityImageFilter" in typehint:
        hint = "TypedVectorRescaleIntensityImageFilterPROXY"
    elif "itkVectorRankHistogram" in typehint:
        hint = "TypedVectorRankHistogramPROXY"
    elif "itkVectorNeighborhoodOperatorImageFilter" in typehint:
        hint = "TypedVectorNeighborhoodOperatorImageFilterPROXY"
    elif "itkVectorNeighborhoodInnerProduct" in typehint:
        hint = "TypedVectorNeighborhoodInnerProductPROXY"
    elif "itkVectorNearestNeighborInterpolateImageFunction" in typehint:
        hint = "TypedVectorNearestNeighborInterpolateImageFunctionPROXY"
    elif "itkVectorMorphologyHistogram" in typehint:
        hint = "TypedVectorMorphologyHistogramPROXY"
    elif "itkVectorMorphologicalGradientHistogram" in typehint:
        hint = "TypedVectorMorphologicalGradientHistogramPROXY"
    elif "itkVectorMeanImageFunction" in typehint:
        hint = "TypedVectorMeanImageFunctionPROXY"
    elif "itkVectorMagnitudeLinearTransform" in typehint:
        hint = "TypedVectorMagnitudeLinearTransformPROXY"
    elif "itkVectorMagnitudeImageFilter" in typehint:
        hint = "TypedVectorMagnitudeImageFilterPROXY"
    elif "itkVectorMagnitude" in typehint:
        hint = "TypedVectorMagnitudePROXY"
    elif "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction" in typehint:
        hint = "TypedVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionPROXY"
    elif "itkVectorLinearInterpolateImageFunction" in typehint:
        hint = "TypedVectorLinearInterpolateImageFunctionPROXY"
    elif "itkVectorInterpolateImageFunction" in typehint:
        hint = "TypedVectorInterpolateImageFunctionPROXY"
    elif "itkVectorIndexSelectionCastImageFilter" in typehint:
        hint = "TypedVectorIndexSelectionCastImageFilterPROXY"
    elif "itkVectorIndexSelectionCast" in typehint:
        hint = "TypedVectorIndexSelectionCastPROXY"
    elif "itkVectorImageToImagePixelAccessor" in typehint:
        hint = "TypedVectorImageToImagePixelAccessorPROXY"
    elif "itkVectorImageToImageMetricTraitsv4" in typehint:
        hint = "TypedVectorImageToImageMetricTraitsv4PROXY"
    elif "itkVectorImageToImageAdaptor" in typehint:
        hint = "TypedVectorImageToImageAdaptorPROXY"
    elif "itkVectorImageNeighborhoodAccessorFunctor" in typehint:
        hint = "TypedVectorImageNeighborhoodAccessorFunctorPROXY"
    elif "itkVectorImage" in typehint:
        hint = "TypedVectorImagePROXY"
    elif "itkVectorGradientNDAnisotropicDiffusionFunction" in typehint:
        hint = "TypedVectorGradientNDAnisotropicDiffusionFunctionPROXY"
    elif "itkVectorGradientMagnitudeImageFilter" in typehint:
        hint = "TypedVectorGradientMagnitudeImageFilterPROXY"
    elif "itkVectorGradientAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedVectorGradientAnisotropicDiffusionImageFilterPROXY"
    elif "itkVectorCurvatureNDAnisotropicDiffusionFunction" in typehint:
        hint = "TypedVectorCurvatureNDAnisotropicDiffusionFunctionPROXY"
    elif "itkVectorCurvatureAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedVectorCurvatureAnisotropicDiffusionImageFilterPROXY"
    elif "itkVectorContainerToListSampleAdaptor" in typehint:
        hint = "TypedVectorContainerToListSampleAdaptorPROXY"
    elif "itkVectorContainer" in typehint:
        hint = "TypedVectorContainerPROXY"
    elif "itkVectorConnectedComponentImageFilter" in typehint:
        hint = "TypedVectorConnectedComponentImageFilterPROXY"
    elif "itkVectorConfidenceConnectedImageFilter" in typehint:
        hint = "TypedVectorConfidenceConnectedImageFilterPROXY"
    elif "itkVectorAnisotropicDiffusionFunction" in typehint:
        hint = "TypedVectorAnisotropicDiffusionFunctionPROXY"
    elif "itkVector" in typehint:
        hint = "TypedVectorPROXY"
    elif "itkVarianceLabelObjectAccessor" in typehint:
        hint = "TypedVarianceLabelObjectAccessorPROXY"
    elif "itkVarianceImageFunction" in typehint:
        hint = "TypedVarianceImageFunctionPROXY"
    elif "itkVariableSizeMatrix" in typehint:
        hint = "TypedVariableSizeMatrixPROXY"
    elif "itkVariableLengthVectorExpression" in typehint:
        hint = "TypedVariableLengthVectorExpressionPROXY"
    elif "itkVariableLengthVectorExpression" in typehint:
        hint = "TypedVariableLengthVectorExpressionPROXY"
    elif "itkVariableLengthVector" in typehint:
        hint = "TypedVariableLengthVectorPROXY"
    elif "itkVanHerkGilWermanUtilities" in typehint:
        hint = "TypedVanHerkGilWermanUtilitiesPROXY"
    elif "itkVanHerkGilWermanErodeImageFilter" in typehint:
        hint = "TypedVanHerkGilWermanErodeImageFilterPROXY"
    elif "itkVanHerkGilWermanErodeDilateImageFilter" in typehint:
        hint = "TypedVanHerkGilWermanErodeDilateImageFilterPROXY"
    elif "itkVanHerkGilWermanDilateImageFilter" in typehint:
        hint = "TypedVanHerkGilWermanDilateImageFilterPROXY"
    elif "itkValuedRegionalMinimaImageFilter" in typehint:
        hint = "TypedValuedRegionalMinimaImageFilterPROXY"
    elif "itkValuedRegionalMaximaImageFilter" in typehint:
        hint = "TypedValuedRegionalMaximaImageFilterPROXY"
    elif "itkValuedRegionalExtremaImageFilter" in typehint:
        hint = "TypedValuedRegionalExtremaImageFilterPROXY"
    elif "itkValarrayImageContainer" in typehint:
        hint = "TypedValarrayImageContainerPROXY"
    elif "itkUpdateWhitakerSparseLevelSet" in typehint:
        hint = "TypedUpdateWhitakerSparseLevelSetPROXY"
    elif "itkUpdateShiSparseLevelSet" in typehint:
        hint = "TypedUpdateShiSparseLevelSetPROXY"
    elif "itkUpdateMalcolmSparseLevelSet" in typehint:
        hint = "TypedUpdateMalcolmSparseLevelSetPROXY"
    elif "itkUpdateCluster" in typehint:
        hint = "TypedUpdateClusterPROXY"
    elif "itkUnsignedIntDispatch" in typehint:
        hint = "TypedUnsignedIntDispatchPROXY"
    elif "itkUnsharpMaskLevelSetImageFilter" in typehint:
        hint = "TypedUnsharpMaskLevelSetImageFilterPROXY"
    elif "itkUnsharpMaskingFunctor" in typehint:
        hint = "TypedUnsharpMaskingFunctorPROXY"
    elif "itkUnsharpMaskImageFilter" in typehint:
        hint = "TypedUnsharpMaskImageFilterPROXY"
    elif "itkUnknownType" in typehint:
        hint = "TypedUnknownTypePROXY"
    elif "itkUnknownType" in typehint:
        hint = "TypedUnknownTypePROXY"
    elif "itkUniqueType_unsigned_int" in typehint:
        hint = "TypedUniqueType_unsigned_intPROXY"
    elif "itkUniqueType_int" in typehint:
        hint = "TypedUniqueType_intPROXY"
    elif "itkUniqueType_bool" in typehint:
        hint = "TypedUniqueType_boolPROXY"
    elif "itkUniqueType" in typehint:
        hint = "TypedUniqueTypePROXY"
    elif "itkUniformRandomSpatialNeighborSubsampler" in typehint:
        hint = "TypedUniformRandomSpatialNeighborSubsamplerPROXY"
    elif "itkUnconstrainedRegionBasedLevelSetFunctionSharedData" in typehint:
        hint = "TypedUnconstrainedRegionBasedLevelSetFunctionSharedDataPROXY"
    elif "itkUnaryMinus" in typehint:
        hint = "TypedUnaryMinusPROXY"
    elif "itkUnaryGeneratorImageFilter" in typehint:
        hint = "TypedUnaryGeneratorImageFilterPROXY"
    elif "itkUnaryFunctorImageFilter" in typehint:
        hint = "TypedUnaryFunctorImageFilterPROXY"
    elif "itkUnaryFrequencyDomainFilter" in typehint:
        hint = "TypedUnaryFrequencyDomainFilterPROXY"
    elif "itkUnaryCorrespondenceMatrix" in typehint:
        hint = "TypedUnaryCorrespondenceMatrixPROXY"
    elif "itkTxtTransformIOTemplate" in typehint:
        hint = "TypedTxtTransformIOTemplatePROXY"
    elif "itkTxtTransformIOFactory" in typehint:
        hint = "TypedTxtTransformIOFactoryPROXY"
    elif "itkTubeSpatialObjectPoint" in typehint:
        hint = "TypedTubeSpatialObjectPointPROXY"
    elif "itkTubeSpatialObject" in typehint:
        hint = "TypedTubeSpatialObjectPROXY"
    elif "itkTriple" in typehint:
        hint = "TypedTriplePROXY"
    elif "itkTriangleThresholdImageFilter" in typehint:
        hint = "TypedTriangleThresholdImageFilterPROXY"
    elif "itkTriangleThresholdCalculator" in typehint:
        hint = "TypedTriangleThresholdCalculatorPROXY"
    elif "itkTriangleMeshToSimplexMeshFilter" in typehint:
        hint = "TypedTriangleMeshToSimplexMeshFilterPROXY"
    elif "itkTriangleMeshToBinaryImageFilter" in typehint:
        hint = "TypedTriangleMeshToBinaryImageFilterPROXY"
    elif "itkTriangleHelper" in typehint:
        hint = "TypedTriangleHelperPROXY"
    elif "itkTriangleCellTopology" in typehint:
        hint = "TypedTriangleCellTopologyPROXY"
    elif "itkTriangleCell" in typehint:
        hint = "TypedTriangleCellPROXY"
    elif "itkTranslationTransform" in typehint:
        hint = "TypedTranslationTransformPROXY"
    elif "itkTransformToDisplacementFieldFilter" in typehint:
        hint = "TypedTransformToDisplacementFieldFilterPROXY"
    elif "itkTransformParametersAdaptorBase" in typehint:
        hint = "TypedTransformParametersAdaptorBasePROXY"
    elif "itkTransformParametersAdaptor" in typehint:
        hint = "TypedTransformParametersAdaptorPROXY"
    elif "itkTransformMeshFilter" in typehint:
        hint = "TypedTransformMeshFilterPROXY"
    elif "itkTransformIOFactoryTemplate" in typehint:
        hint = "TypedTransformIOFactoryTemplatePROXY"
    elif "itkTransformIOBaseTemplate" in typehint:
        hint = "TypedTransformIOBaseTemplatePROXY"
    elif "itkTransformFileWriterTemplate" in typehint:
        hint = "TypedTransformFileWriterTemplatePROXY"
    elif "itkTransformFileReaderTemplate" in typehint:
        hint = "TypedTransformFileReaderTemplatePROXY"
    elif "itkTransformFactoryBase" in typehint:
        hint = "TypedTransformFactoryBasePROXY"
    elif "itkTransformFactory" in typehint:
        hint = "TypedTransformFactoryPROXY"
    elif "itkTransformDirection" in typehint:
        hint = "TypedTransformDirectionPROXY"
    elif "itkTransformCategory" in typehint:
        hint = "TypedTransformCategoryPROXY"
    elif "itkTransformBaseTemplateEnums" in typehint:
        hint = "TypedTransformBaseTemplateEnumsPROXY"
    elif "itkTransformBaseTemplate" in typehint:
        hint = "TypedTransformBaseTemplatePROXY"
    elif "itkTransform" in typehint:
        hint = "TypedTransformPROXY"
    elif "itkTotalProgressReporter" in typehint:
        hint = "TypedTotalProgressReporterPROXY"
    elif "itkTorusInteriorExteriorSpatialFunction" in typehint:
        hint = "TypedTorusInteriorExteriorSpatialFunctionPROXY"
    elif "itkTopologyCheck" in typehint:
        hint = "TypedTopologyCheckPROXY"
    elif "itkTobogganImageFilter" in typehint:
        hint = "TypedTobogganImageFilterPROXY"
    elif "itkTimeVaryingVelocityFieldTransformParametersAdaptor" in typehint:
        hint = "TypedTimeVaryingVelocityFieldTransformParametersAdaptorPROXY"
    elif "itkTimeVaryingVelocityFieldTransform" in typehint:
        hint = "TypedTimeVaryingVelocityFieldTransformPROXY"
    elif "itkTimeVaryingVelocityFieldIntegrationImageFilter" in typehint:
        hint = "TypedTimeVaryingVelocityFieldIntegrationImageFilterPROXY"
    elif "itkTimeVaryingVelocityFieldImageRegistrationMethodv4" in typehint:
        hint = "TypedTimeVaryingVelocityFieldImageRegistrationMethodv4PROXY"
    elif "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor" in typehint:
        hint = "TypedTimeVaryingBSplineVelocityFieldTransformParametersAdaptorPROXY"
    elif "itkTimeVaryingBSplineVelocityFieldTransform" in typehint:
        hint = "TypedTimeVaryingBSplineVelocityFieldTransformPROXY"
    elif "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod" in typehint:
        hint = "TypedTimeVaryingBSplineVelocityFieldImageRegistrationMethodPROXY"
    elif "itkTimeStampFormat" in typehint:
        hint = "TypedTimeStampFormatPROXY"
    elif "itkTimeStamp" in typehint:
        hint = "TypedTimeStampPROXY"
    elif "itkTimeProbesCollectorBase" in typehint:
        hint = "TypedTimeProbesCollectorBasePROXY"
    elif "itkTimeProbe" in typehint:
        hint = "TypedTimeProbePROXY"
    elif "itkTileInfo" in typehint:
        hint = "TypedTileInfoPROXY"
    elif "itkTileImageFilter" in typehint:
        hint = "TypedTileImageFilterPROXY"
    elif "itkTikhonovDeconvolutionImageFilter" in typehint:
        hint = "TypedTikhonovDeconvolutionImageFilterPROXY"
    elif "itkTikhonovDeconvolutionFunctor" in typehint:
        hint = "TypedTikhonovDeconvolutionFunctorPROXY"
    elif "itkTIFFImageIOFactory" in typehint:
        hint = "TypedTIFFImageIOFactoryPROXY"
    elif "itkTIFFImageIO" in typehint:
        hint = "TypedTIFFImageIOPROXY"
    elif "itkThresholdSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedThresholdSegmentationLevelSetImageFilterPROXY"
    elif "itkThresholdSegmentationLevelSetFunction" in typehint:
        hint = "TypedThresholdSegmentationLevelSetFunctionPROXY"
    elif "itkThresholdMaximumConnectedComponentsImageFilter" in typehint:
        hint = "TypedThresholdMaximumConnectedComponentsImageFilterPROXY"
    elif "itkThresholdLabelerImageFilter" in typehint:
        hint = "TypedThresholdLabelerImageFilterPROXY"
    elif "itkThresholdLabeler" in typehint:
        hint = "TypedThresholdLabelerPROXY"
    elif "itkThresholdImageFilter" in typehint:
        hint = "TypedThresholdImageFilterPROXY"
    elif "itkThreadStruct" in typehint:
        hint = "TypedThreadStructPROXY"
    elif "itkThreadStruct" in typehint:
        hint = "TypedThreadStructPROXY"
    elif "itkThreadStruct" in typehint:
        hint = "TypedThreadStructPROXY"
    elif "itkThreadStruct" in typehint:
        hint = "TypedThreadStructPROXY"
    elif "itkThreadStruct" in typehint:
        hint = "TypedThreadStructPROXY"
    elif "itkThreadRegionType" in typehint:
        hint = "TypedThreadRegionTypePROXY"
    elif "itkThreadRegionType" in typehint:
        hint = "TypedThreadRegionTypePROXY"
    elif "itkThreadPoolInfoStruct" in typehint:
        hint = "TypedThreadPoolInfoStructPROXY"
    elif "itkThreadPool" in typehint:
        hint = "TypedThreadPoolPROXY"
    elif "itkThreadLogger" in typehint:
        hint = "TypedThreadLoggerPROXY"
    elif "itkThreadFilterStruct" in typehint:
        hint = "TypedThreadFilterStructPROXY"
    elif "itkThreadExitCode" in typehint:
        hint = "TypedThreadExitCodePROXY"
    elif "itkThreader" in typehint:
        hint = "TypedThreaderPROXY"
    elif "itkThreadedIteratorRangePartitionerDomain" in typehint:
        hint = "TypedThreadedIteratorRangePartitionerDomainPROXY"
    elif "itkThreadedIteratorRangePartitioner" in typehint:
        hint = "TypedThreadedIteratorRangePartitionerPROXY"
    elif "itkThreadedIndexedContainerPartitioner" in typehint:
        hint = "TypedThreadedIndexedContainerPartitionerPROXY"
    elif "itkThreadedImageRegionPartitioner" in typehint:
        hint = "TypedThreadedImageRegionPartitionerPROXY"
    elif "itkThreadedDomainPartitioner" in typehint:
        hint = "TypedThreadedDomainPartitionerPROXY"
    elif "itkThreadDataUnaligned" in typehint:
        hint = "TypedThreadDataUnalignedPROXY"
    elif "itkThreadDataStruct" in typehint:
        hint = "TypedThreadDataStructPROXY"
    elif "itkThinPlateSplineKernelTransform" in typehint:
        hint = "TypedThinPlateSplineKernelTransformPROXY"
    elif "itkThinPlateR2LogRSplineKernelTransform" in typehint:
        hint = "TypedThinPlateR2LogRSplineKernelTransformPROXY"
    elif "itkTextureFeature" in typehint:
        hint = "TypedTextureFeaturePROXY"
    elif "itkTextOutput" in typehint:
        hint = "TypedTextOutputPROXY"
    elif "itkTetrahedronCellTopology" in typehint:
        hint = "TypedTetrahedronCellTopologyPROXY"
    elif "itkTetrahedronCell" in typehint:
        hint = "TypedTetrahedronCellPROXY"
    elif "itkTestExtractSliceImageFilterCollapseStrategy" in typehint:
        hint = "TypedTestExtractSliceImageFilterCollapseStrategyPROXY"
    elif "itkTernaryOperatorImageFilter" in typehint:
        hint = "TypedTernaryOperatorImageFilterPROXY"
    elif "itkTernaryOperator" in typehint:
        hint = "TypedTernaryOperatorPROXY"
    elif "itkTernaryMagnitudeSquaredImageFilter" in typehint:
        hint = "TypedTernaryMagnitudeSquaredImageFilterPROXY"
    elif "itkTernaryMagnitudeImageFilter" in typehint:
        hint = "TypedTernaryMagnitudeImageFilterPROXY"
    elif "itkTernaryGeneratorImageFilter" in typehint:
        hint = "TypedTernaryGeneratorImageFilterPROXY"
    elif "itkTernaryFunctorImageFilter" in typehint:
        hint = "TypedTernaryFunctorImageFilterPROXY"
    elif "itkTernaryAddImageFilter" in typehint:
        hint = "TypedTernaryAddImageFilterPROXY"
    elif "itkTERMINATION_CODE" in typehint:
        hint = "TypedTERMINATION_CODEPROXY"
    elif "itkTensorRelativeAnisotropyImageFilter" in typehint:
        hint = "TypedTensorRelativeAnisotropyImageFilterPROXY"
    elif "itkTensorRelativeAnisotropyFunction" in typehint:
        hint = "TypedTensorRelativeAnisotropyFunctionPROXY"
    elif "itkTensorFractionalAnisotropyImageFilter" in typehint:
        hint = "TypedTensorFractionalAnisotropyImageFilterPROXY"
    elif "itkTensorFractionalAnisotropyFunction" in typehint:
        hint = "TypedTensorFractionalAnisotropyFunctionPROXY"
    elif "itkTemporalUnit" in typehint:
        hint = "TypedTemporalUnitPROXY"
    elif "itkTemporalRegion" in typehint:
        hint = "TypedTemporalRegionPROXY"
    elif "itkTemporalProcessObject" in typehint:
        hint = "TypedTemporalProcessObjectPROXY"
    elif "itkTemporalDataObjectEnums" in typehint:
        hint = "TypedTemporalDataObjectEnumsPROXY"
    elif "itkTemporalDataObject" in typehint:
        hint = "TypedTemporalDataObjectPROXY"
    elif "itkTDistribution" in typehint:
        hint = "TypedTDistributionPROXY"
    elif "itkTBBMultiThreader" in typehint:
        hint = "TypedTBBMultiThreaderPROXY"
    elif "itkTargetCondition" in typehint:
        hint = "TypedTargetConditionPROXY"
    elif "itkTanPixelAccessor" in typehint:
        hint = "TypedTanPixelAccessorPROXY"
    elif "itkTanImageFilter" in typehint:
        hint = "TypedTanImageFilterPROXY"
    elif "itkTanImageAdaptor" in typehint:
        hint = "TypedTanImageAdaptorPROXY"
    elif "itkTan" in typehint:
        hint = "TypedTanPROXY"
    elif "itkSysResourceMemoryUsageObserver" in typehint:
        hint = "TypedSysResourceMemoryUsageObserverPROXY"
    elif "itkSyNImageRegistrationMethod" in typehint:
        hint = "TypedSyNImageRegistrationMethodPROXY"
    elif "itkSymmetricSecondRankTensor" in typehint:
        hint = "TypedSymmetricSecondRankTensorPROXY"
    elif "itkSymmetricForcesDemonsRegistrationFunction" in typehint:
        hint = "TypedSymmetricForcesDemonsRegistrationFunctionPROXY"
    elif "itkSymmetricForcesDemonsRegistrationFilter" in typehint:
        hint = "TypedSymmetricForcesDemonsRegistrationFilterPROXY"
    elif "itkSymmetricEllipsoidInteriorExteriorSpatialFunction" in typehint:
        hint = "TypedSymmetricEllipsoidInteriorExteriorSpatialFunctionPROXY"
    elif "itkSymmetricEigenAnalysisImageFilter" in typehint:
        hint = "TypedSymmetricEigenAnalysisImageFilterPROXY"
    elif "itkSymmetricEigenAnalysisFunction" in typehint:
        hint = "TypedSymmetricEigenAnalysisFunctionPROXY"
    elif "itkSymmetricEigenAnalysisFixedDimensionImageFilter" in typehint:
        hint = "TypedSymmetricEigenAnalysisFixedDimensionImageFilterPROXY"
    elif "itkSymmetricEigenAnalysisFixedDimensionFunction" in typehint:
        hint = "TypedSymmetricEigenAnalysisFixedDimensionFunctionPROXY"
    elif "itkSymmetricEigenAnalysisFixedDimension" in typehint:
        hint = "TypedSymmetricEigenAnalysisFixedDimensionPROXY"
    elif "itkSymmetricEigenAnalysisEnums" in typehint:
        hint = "TypedSymmetricEigenAnalysisEnumsPROXY"
    elif "itkSymmetricEigenAnalysis" in typehint:
        hint = "TypedSymmetricEigenAnalysisPROXY"
    elif "itkSurfaceSpatialObjectPoint" in typehint:
        hint = "TypedSurfaceSpatialObjectPointPROXY"
    elif "itkSurfaceSpatialObject" in typehint:
        hint = "TypedSurfaceSpatialObjectPROXY"
    elif "itkSumProjectionImageFilter" in typehint:
        hint = "TypedSumProjectionImageFilterPROXY"
    elif "itkSumOfSquaresImageFunction" in typehint:
        hint = "TypedSumOfSquaresImageFunctionPROXY"
    elif "itkSummerColormapFunction" in typehint:
        hint = "TypedSummerColormapFunctionPROXY"
    elif "itkSumLabelObjectAccessor" in typehint:
        hint = "TypedSumLabelObjectAccessorPROXY"
    elif "itkSumAccumulator" in typehint:
        hint = "TypedSumAccumulatorPROXY"
    elif "itkSubtractImageFilter" in typehint:
        hint = "TypedSubtractImageFilterPROXY"
    elif "itkSubsamplerBase" in typehint:
        hint = "TypedSubsamplerBasePROXY"
    elif "itkSubsample" in typehint:
        hint = "TypedSubsamplePROXY"
    elif "itkSub2" in typehint:
        hint = "TypedSub2PROXY"
    elif "itkStructuringElementFacet" in typehint:
        hint = "TypedStructuringElementFacetPROXY"
    elif "itkStructHashFunction" in typehint:
        hint = "TypedStructHashFunctionPROXY"
    elif "itkStringTools" in typehint:
        hint = "TypedStringToolsPROXY"
    elif "itkStretchIntensityImageFilter" in typehint:
        hint = "TypedStretchIntensityImageFilterPROXY"
    elif "itkStreamingProcessObject" in typehint:
        hint = "TypedStreamingProcessObjectPROXY"
    elif "itkStreamingImageIOBase" in typehint:
        hint = "TypedStreamingImageIOBasePROXY"
    elif "itkStreamingImageFilter" in typehint:
        hint = "TypedStreamingImageFilterPROXY"
    elif "itkStoppingCriterionBase" in typehint:
        hint = "TypedStoppingCriterionBasePROXY"
    elif "itkStopConditionSPSAOptimizer" in typehint:
        hint = "TypedStopConditionSPSAOptimizerPROXY"
    elif "itkStopConditionObjectToObjectOptimizer" in typehint:
        hint = "TypedStopConditionObjectToObjectOptimizerPROXY"
    elif "itkStopConditionGradientDescentOptimizer" in typehint:
        hint = "TypedStopConditionGradientDescentOptimizerPROXY"
    elif "itkStopCondition" in typehint:
        hint = "TypedStopConditionPROXY"
    elif "itkStochasticFractalDimensionImageFilter" in typehint:
        hint = "TypedStochasticFractalDimensionImageFilterPROXY"
    elif "itkSTLContainerAdaptor" in typehint:
        hint = "TypedSTLContainerAdaptorPROXY"
    elif "itkSTLConstContainerAdaptor" in typehint:
        hint = "TypedSTLConstContainerAdaptorPROXY"
    elif "itkStimulateImageIOFactory" in typehint:
        hint = "TypedStimulateImageIOFactoryPROXY"
    elif "itkStimulateImageIO" in typehint:
        hint = "TypedStimulateImageIOPROXY"
    elif "itkStdStreamStateSave" in typehint:
        hint = "TypedStdStreamStateSavePROXY"
    elif "itkStdStreamLogOutput" in typehint:
        hint = "TypedStdStreamLogOutputPROXY"
    elif "itkStatisticsUniqueLabelMapFilter" in typehint:
        hint = "TypedStatisticsUniqueLabelMapFilterPROXY"
    elif "itkStatisticsRelabelLabelMapFilter" in typehint:
        hint = "TypedStatisticsRelabelLabelMapFilterPROXY"
    elif "itkStatisticsRelabelImageFilter" in typehint:
        hint = "TypedStatisticsRelabelImageFilterPROXY"
    elif "itkStatisticsPositionLabelMapFilter" in typehint:
        hint = "TypedStatisticsPositionLabelMapFilterPROXY"
    elif "itkStatisticsOpeningLabelMapFilter" in typehint:
        hint = "TypedStatisticsOpeningLabelMapFilterPROXY"
    elif "itkStatisticsLabelObject" in typehint:
        hint = "TypedStatisticsLabelObjectPROXY"
    elif "itkStatisticsLabelMapFilter" in typehint:
        hint = "TypedStatisticsLabelMapFilterPROXY"
    elif "itkStatisticsKeepNObjectsLabelMapFilter" in typehint:
        hint = "TypedStatisticsKeepNObjectsLabelMapFilterPROXY"
    elif "itkStatisticsImageFilter" in typehint:
        hint = "TypedStatisticsImageFilterPROXY"
    elif "itkStaticCast" in typehint:
        hint = "TypedStaticCastPROXY"
    elif "itkSTAPLEImageFilter" in typehint:
        hint = "TypedSTAPLEImageFilterPROXY"
    elif "itkStandardDeviationProjectionImageFilter" in typehint:
        hint = "TypedStandardDeviationProjectionImageFilterPROXY"
    elif "itkStandardDeviationPerComponentSampleFilter" in typehint:
        hint = "TypedStandardDeviationPerComponentSampleFilterPROXY"
    elif "itkStandardDeviationLabelObjectAccessor" in typehint:
        hint = "TypedStandardDeviationLabelObjectAccessorPROXY"
    elif "itkStandardDeviationAccumulator" in typehint:
        hint = "TypedStandardDeviationAccumulatorPROXY"
    elif "itkSquareImageFilter" in typehint:
        hint = "TypedSquareImageFilterPROXY"
    elif "itkSquaredEdgeLengthDecimationQuadEdgeMeshFilter" in typehint:
        hint = "TypedSquaredEdgeLengthDecimationQuadEdgeMeshFilterPROXY"
    elif "itkSquaredDifferenceImageFilter" in typehint:
        hint = "TypedSquaredDifferenceImageFilterPROXY"
    elif "itkSquaredDifference2" in typehint:
        hint = "TypedSquaredDifference2PROXY"
    elif "itkSquare" in typehint:
        hint = "TypedSquarePROXY"
    elif "itkSqrtPixelAccessor" in typehint:
        hint = "TypedSqrtPixelAccessorPROXY"
    elif "itkSqrtImageFilter" in typehint:
        hint = "TypedSqrtImageFilterPROXY"
    elif "itkSqrtImageAdaptor" in typehint:
        hint = "TypedSqrtImageAdaptorPROXY"
    elif "itkSqrt" in typehint:
        hint = "TypedSqrtPROXY"
    elif "itkSPSAOptimizerEnums" in typehint:
        hint = "TypedSPSAOptimizerEnumsPROXY"
    elif "itkSPSAOptimizer" in typehint:
        hint = "TypedSPSAOptimizerPROXY"
    elif "itkSpringColormapFunction" in typehint:
        hint = "TypedSpringColormapFunctionPROXY"
    elif "itkSphereSpatialFunction" in typehint:
        hint = "TypedSphereSpatialFunctionPROXY"
    elif "itkSphereSignedDistanceFunction" in typehint:
        hint = "TypedSphereSignedDistanceFunctionPROXY"
    elif "itkSpeckleNoiseImageFilter" in typehint:
        hint = "TypedSpeckleNoiseImageFilterPROXY"
    elif "itkSpecialCoordinatesImage" in typehint:
        hint = "TypedSpecialCoordinatesImagePROXY"
    elif "itkSpatialSample" in typehint:
        hint = "TypedSpatialSamplePROXY"
    elif "itkSpatialOrientationAdapter" in typehint:
        hint = "TypedSpatialOrientationAdapterPROXY"
    elif "itkSpatialObjectWriter" in typehint:
        hint = "TypedSpatialObjectWriterPROXY"
    elif "itkSpatialObjectToPointSetFilter" in typehint:
        hint = "TypedSpatialObjectToPointSetFilterPROXY"
    elif "itkSpatialObjectToImageStatisticsCalculator" in typehint:
        hint = "TypedSpatialObjectToImageStatisticsCalculatorPROXY"
    elif "itkSpatialObjectToImageFilter" in typehint:
        hint = "TypedSpatialObjectToImageFilterPROXY"
    elif "itkSpatialObjectReader" in typehint:
        hint = "TypedSpatialObjectReaderPROXY"
    elif "itkSpatialObjectProperty" in typehint:
        hint = "TypedSpatialObjectPropertyPROXY"
    elif "itkSpatialObjectPoint" in typehint:
        hint = "TypedSpatialObjectPointPROXY"
    elif "itkSpatialObjectFactoryBase" in typehint:
        hint = "TypedSpatialObjectFactoryBasePROXY"
    elif "itkSpatialObjectFactory" in typehint:
        hint = "TypedSpatialObjectFactoryPROXY"
    elif "itkSpatialObjectDuplicator" in typehint:
        hint = "TypedSpatialObjectDuplicatorPROXY"
    elif "itkSpatialObject" in typehint:
        hint = "TypedSpatialObjectPROXY"
    elif "itkSpatialNeighborSubsampler" in typehint:
        hint = "TypedSpatialNeighborSubsamplerPROXY"
    elif "itkSpatialFunctionImageEvaluatorFilter" in typehint:
        hint = "TypedSpatialFunctionImageEvaluatorFilterPROXY"
    elif "itkSpatialFunction" in typehint:
        hint = "TypedSpatialFunctionPROXY"
    elif "itkSparseImage" in typehint:
        hint = "TypedSparseImagePROXY"
    elif "itkSparseFrequencyContainer2" in typehint:
        hint = "TypedSparseFrequencyContainer2PROXY"
    elif "itkSparseFieldLevelSetNode" in typehint:
        hint = "TypedSparseFieldLevelSetNodePROXY"
    elif "itkSparseFieldLevelSetImageFilter" in typehint:
        hint = "TypedSparseFieldLevelSetImageFilterPROXY"
    elif "itkSparseFieldLayerIterator" in typehint:
        hint = "TypedSparseFieldLayerIteratorPROXY"
    elif "itkSparseFieldLayer" in typehint:
        hint = "TypedSparseFieldLayerPROXY"
    elif "itkSparseFieldFourthOrderLevelSetImageFilter" in typehint:
        hint = "TypedSparseFieldFourthOrderLevelSetImageFilterPROXY"
    elif "itkSparseFieldCityBlockNeighborList" in typehint:
        hint = "TypedSparseFieldCityBlockNeighborListPROXY"
    elif "itkSparseDataStruct" in typehint:
        hint = "TypedSparseDataStructPROXY"
    elif "itksort_comp" in typehint:
        hint = "Typedsort_compPROXY"
    elif "itkSobelOperator" in typehint:
        hint = "TypedSobelOperatorPROXY"
    elif "itkSobelEdgeDetectionImageFilter" in typehint:
        hint = "TypedSobelEdgeDetectionImageFilterPROXY"
    elif "itkSmoothingRecursiveGaussianImageFilter" in typehint:
        hint = "TypedSmoothingRecursiveGaussianImageFilterPROXY"
    elif "itkSmoothingQuadEdgeMeshFilter" in typehint:
        hint = "TypedSmoothingQuadEdgeMeshFilterPROXY"
    elif "itkSmartPointer" in typehint:
        hint = "TypedSmartPointerPROXY"
    elif "itkSmapsRecord" in typehint:
        hint = "TypedSmapsRecordPROXY"
    elif "itkSmapsFileParser" in typehint:
        hint = "TypedSmapsFileParserPROXY"
    elif "itkSmapsData_2_6" in typehint:
        hint = "TypedSmapsData_2_6PROXY"
    elif "itkSLICImageFilter" in typehint:
        hint = "TypedSLICImageFilterPROXY"
    elif "itkSliceIterator" in typehint:
        hint = "TypedSliceIteratorPROXY"
    elif "itkSliceImageFilter" in typehint:
        hint = "TypedSliceImageFilterPROXY"
    elif "itkSliceBySliceImageFilter" in typehint:
        hint = "TypedSliceBySliceImageFilterPROXY"
    elif "itkSkewnessLabelObjectAccessor" in typehint:
        hint = "TypedSkewnessLabelObjectAccessorPROXY"
    elif "itkSize" in typehint:
        hint = "TypedSizePROXY"
    elif "itkSinRegularizedHeavisideStepFunction" in typehint:
        hint = "TypedSinRegularizedHeavisideStepFunctionPROXY"
    elif "itkSinPixelAccessor" in typehint:
        hint = "TypedSinPixelAccessorPROXY"
    elif "itkSinImageFilter" in typehint:
        hint = "TypedSinImageFilterPROXY"
    elif "itkSinImageAdaptor" in typehint:
        hint = "TypedSinImageAdaptorPROXY"
    elif "itkSingleValuedVnlCostFunctionAdaptorv4" in typehint:
        hint = "TypedSingleValuedVnlCostFunctionAdaptorv4PROXY"
    elif "itkSingleValuedVnlCostFunctionAdaptor" in typehint:
        hint = "TypedSingleValuedVnlCostFunctionAdaptorPROXY"
    elif "itkSingleValuedNonLinearVnlOptimizerv4" in typehint:
        hint = "TypedSingleValuedNonLinearVnlOptimizerv4PROXY"
    elif "itkSingleValuedNonLinearVnlOptimizer" in typehint:
        hint = "TypedSingleValuedNonLinearVnlOptimizerPROXY"
    elif "itkSingleValuedNonLinearOptimizer" in typehint:
        hint = "TypedSingleValuedNonLinearOptimizerPROXY"
    elif "itkSingleValuedCostFunctionv4Template" in typehint:
        hint = "TypedSingleValuedCostFunctionv4TemplatePROXY"
    elif "itkSingleValuedCostFunction" in typehint:
        hint = "TypedSingleValuedCostFunctionPROXY"
    elif "itkSingletonIndex" in typehint:
        hint = "TypedSingletonIndexPROXY"
    elif "itkSin" in typehint:
        hint = "TypedSinPROXY"
    elif "itkSimplexMeshVolumeCalculator" in typehint:
        hint = "TypedSimplexMeshVolumeCalculatorPROXY"
    elif "itkSimplexMeshToTriangleMeshFilter" in typehint:
        hint = "TypedSimplexMeshToTriangleMeshFilterPROXY"
    elif "itkSimplexMeshGeometry" in typehint:
        hint = "TypedSimplexMeshGeometryPROXY"
    elif "itkSimplexMeshAdaptTopologyFilter" in typehint:
        hint = "TypedSimplexMeshAdaptTopologyFilterPROXY"
    elif "itkSimplexMesh" in typehint:
        hint = "TypedSimplexMeshPROXY"
    elif "itkSimplexCellVisitor" in typehint:
        hint = "TypedSimplexCellVisitorPROXY"
    elif "itkSimplexCellVisitor" in typehint:
        hint = "TypedSimplexCellVisitorPROXY"
    elif "itkSimplexCellVisitor" in typehint:
        hint = "TypedSimplexCellVisitorPROXY"
    elif "itkSimpleWisdomFilenameGenerator" in typehint:
        hint = "TypedSimpleWisdomFilenameGeneratorPROXY"
    elif "itkSimpleMultiResolutionImageRegistrationUI2" in typehint:
        hint = "TypedSimpleMultiResolutionImageRegistrationUI2PROXY"
    elif "itkSimpleMultiResolutionImageRegistrationUI" in typehint:
        hint = "TypedSimpleMultiResolutionImageRegistrationUIPROXY"
    elif "itkSimpleMemberCommand" in typehint:
        hint = "TypedSimpleMemberCommandPROXY"
    elif "itkSimpleForwardIterator" in typehint:
        hint = "TypedSimpleForwardIteratorPROXY"
    elif "itkSimpleFilterWatcher" in typehint:
        hint = "TypedSimpleFilterWatcherPROXY"
    elif "itkSimpleDataObjectDecorator" in typehint:
        hint = "TypedSimpleDataObjectDecoratorPROXY"
    elif "itkSimpleContourExtractorImageFilter" in typehint:
        hint = "TypedSimpleContourExtractorImageFilterPROXY"
    elif "itkSimpleConstMemberCommand" in typehint:
        hint = "TypedSimpleConstMemberCommandPROXY"
    elif "itkSimilarVectorsFunctor" in typehint:
        hint = "TypedSimilarVectorsFunctorPROXY"
    elif "itkSimilarPixelsFunctor" in typehint:
        hint = "TypedSimilarPixelsFunctorPROXY"
    elif "itkSimilarityIndexImageFilter" in typehint:
        hint = "TypedSimilarityIndexImageFilterPROXY"
    elif "itkSimilarity3DTransform" in typehint:
        hint = "TypedSimilarity3DTransformPROXY"
    elif "itkSimilarity2DTransform" in typehint:
        hint = "TypedSimilarity2DTransformPROXY"
    elif "itkSignedMaurerDistanceMapImageFilter" in typehint:
        hint = "TypedSignedMaurerDistanceMapImageFilterPROXY"
    elif "itkSignedDanielssonDistanceMapImageFilter" in typehint:
        hint = "TypedSignedDanielssonDistanceMapImageFilterPROXY"
    elif "itkSigned" in typehint:
        hint = "TypedSignedPROXY"
    elif "itkSigmoidImageFilter" in typehint:
        hint = "TypedSigmoidImageFilterPROXY"
    elif "itkSigmoid" in typehint:
        hint = "TypedSigmoidPROXY"
    elif "itkSigmaStepMethod" in typehint:
        hint = "TypedSigmaStepMethodPROXY"
    elif "itkSiemensVisionImageIOFactory" in typehint:
        hint = "TypedSiemensVisionImageIOFactoryPROXY"
    elif "itkSiemensVisionImageIO" in typehint:
        hint = "TypedSiemensVisionImageIOPROXY"
    elif "itkSIDE" in typehint:
        hint = "TypedSIDEPROXY"
    elif "itkShrinkToFit" in typehint:
        hint = "TypedShrinkToFitPROXY"
    elif "itkShrinkImageFilter" in typehint:
        hint = "TypedShrinkImageFilterPROXY"
    elif "itkShotNoiseImageFilter" in typehint:
        hint = "TypedShotNoiseImageFilterPROXY"
    elif "itkShiSparseLevelSetImage" in typehint:
        hint = "TypedShiSparseLevelSetImagePROXY"
    elif "itkShiftScaleLabelMapFilter" in typehint:
        hint = "TypedShiftScaleLabelMapFilterPROXY"
    elif "itkShiftScaleImageFilter" in typehint:
        hint = "TypedShiftScaleImageFilterPROXY"
    elif "itkShapeUniqueLabelMapFilter" in typehint:
        hint = "TypedShapeUniqueLabelMapFilterPROXY"
    elif "itkShapeSignedDistanceFunction" in typehint:
        hint = "TypedShapeSignedDistanceFunctionPROXY"
    elif "itkShapeRelabelLabelMapFilter" in typehint:
        hint = "TypedShapeRelabelLabelMapFilterPROXY"
    elif "itkShapeRelabelImageFilter" in typehint:
        hint = "TypedShapeRelabelImageFilterPROXY"
    elif "itkShapePriorSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedShapePriorSegmentationLevelSetImageFilterPROXY"
    elif "itkShapePriorSegmentationLevelSetFunction" in typehint:
        hint = "TypedShapePriorSegmentationLevelSetFunctionPROXY"
    elif "itkShapePriorMAPCostFunctionBase" in typehint:
        hint = "TypedShapePriorMAPCostFunctionBasePROXY"
    elif "itkShapePriorMAPCostFunction" in typehint:
        hint = "TypedShapePriorMAPCostFunctionPROXY"
    elif "itkShapePriorGlobalDataStruct" in typehint:
        hint = "TypedShapePriorGlobalDataStructPROXY"
    elif "itkShapePositionLabelMapFilter" in typehint:
        hint = "TypedShapePositionLabelMapFilterPROXY"
    elif "itkShapeOpeningLabelMapFilter" in typehint:
        hint = "TypedShapeOpeningLabelMapFilterPROXY"
    elif "itkShapeLabelObject" in typehint:
        hint = "TypedShapeLabelObjectPROXY"
    elif "itkShapeLabelMapFilter" in typehint:
        hint = "TypedShapeLabelMapFilterPROXY"
    elif "itkShapeKeepNObjectsLabelMapFilter" in typehint:
        hint = "TypedShapeKeepNObjectsLabelMapFilterPROXY"
    elif "itkShapedNeighborhoodIterator" in typehint:
        hint = "TypedShapedNeighborhoodIteratorPROXY"
    elif "itkShapedImageNeighborhoodRange" in typehint:
        hint = "TypedShapedImageNeighborhoodRangePROXY"
    elif "itkShapedFloodFilledImageFunctionConditionalIterator" in typehint:
        hint = "TypedShapedFloodFilledImageFunctionConditionalIteratorPROXY"
    elif "itkShapedFloodFilledImageFunctionConditionalConstIterator" in typehint:
        hint = "TypedShapedFloodFilledImageFunctionConditionalConstIteratorPROXY"
    elif "itkShapedFloodFilledFunctionConditionalConstIterator" in typehint:
        hint = "TypedShapedFloodFilledFunctionConditionalConstIteratorPROXY"
    elif "itkShapeDetectionLevelSetImageFilter" in typehint:
        hint = "TypedShapeDetectionLevelSetImageFilterPROXY"
    elif "itkShapeDetectionLevelSetFunction" in typehint:
        hint = "TypedShapeDetectionLevelSetFunctionPROXY"
    elif "itkShanbhagThresholdImageFilter" in typehint:
        hint = "TypedShanbhagThresholdImageFilterPROXY"
    elif "itkShanbhagThresholdCalculator" in typehint:
        hint = "TypedShanbhagThresholdCalculatorPROXY"
    elif "itkSegmentTreeGenerator" in typehint:
        hint = "TypedSegmentTreeGeneratorPROXY"
    elif "itkSegmentTree" in typehint:
        hint = "TypedSegmentTreePROXY"
    elif "itkSegmentTable" in typehint:
        hint = "TypedSegmentTablePROXY"
    elif "itkSegmenter" in typehint:
        hint = "TypedSegmenterPROXY"
    elif "itkSegmentationRegion" in typehint:
        hint = "TypedSegmentationRegionPROXY"
    elif "itkSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedSegmentationLevelSetImageFilterPROXY"
    elif "itkSegmentationLevelSetFunction" in typehint:
        hint = "TypedSegmentationLevelSetFunctionPROXY"
    elif "itkSegmentationBorder" in typehint:
        hint = "TypedSegmentationBorderPROXY"
    elif "itksegment_t" in typehint:
        hint = "Typedsegment_tPROXY"
    elif "itkScatterMatrixImageFunction" in typehint:
        hint = "TypedScatterMatrixImageFunctionPROXY"
    elif "itkScanlineFilterCommon" in typehint:
        hint = "TypedScanlineFilterCommonPROXY"
    elif "itkScaleVersor3DTransform" in typehint:
        hint = "TypedScaleVersor3DTransformPROXY"
    elif "itkScaleTransform" in typehint:
        hint = "TypedScaleTransformPROXY"
    elif "itkScaleSkewVersor3DTransform" in typehint:
        hint = "TypedScaleSkewVersor3DTransformPROXY"
    elif "itkScaleLogarithmicTransform" in typehint:
        hint = "TypedScaleLogarithmicTransformPROXY"
    elif "itkScalarToRGBPixelFunctor" in typehint:
        hint = "TypedScalarToRGBPixelFunctorPROXY"
    elif "itkScalarToRGBColormapImageFilterEnums" in typehint:
        hint = "TypedScalarToRGBColormapImageFilterEnumsPROXY"
    elif "itkScalarToRGBColormapImageFilter" in typehint:
        hint = "TypedScalarToRGBColormapImageFilterPROXY"
    elif "itkScalarRegionBasedLevelSetFunction" in typehint:
        hint = "TypedScalarRegionBasedLevelSetFunctionPROXY"
    elif "itkScalarImageToTextureFeaturesFilter" in typehint:
        hint = "TypedScalarImageToTextureFeaturesFilterPROXY"
    elif "itkScalarImageToRunLengthMatrixFilter" in typehint:
        hint = "TypedScalarImageToRunLengthMatrixFilterPROXY"
    elif "itkScalarImageToRunLengthFeaturesFilter" in typehint:
        hint = "TypedScalarImageToRunLengthFeaturesFilterPROXY"
    elif "itkScalarImageToHistogramGenerator" in typehint:
        hint = "TypedScalarImageToHistogramGeneratorPROXY"
    elif "itkScalarImageToCooccurrenceMatrixFilter" in typehint:
        hint = "TypedScalarImageToCooccurrenceMatrixFilterPROXY"
    elif "itkScalarImageToCooccurrenceListSampleFilter" in typehint:
        hint = "TypedScalarImageToCooccurrenceListSampleFilterPROXY"
    elif "itkScalarImageKmeansImageFilter" in typehint:
        hint = "TypedScalarImageKmeansImageFilterPROXY"
    elif "itkScalarConnectedComponentImageFilter" in typehint:
        hint = "TypedScalarConnectedComponentImageFilterPROXY"
    elif "itkScalarChanAndVeseSparseLevelSetImageFilter" in typehint:
        hint = "TypedScalarChanAndVeseSparseLevelSetImageFilterPROXY"
    elif "itkScalarChanAndVeseLevelSetFunctionData" in typehint:
        hint = "TypedScalarChanAndVeseLevelSetFunctionDataPROXY"
    elif "itkScalarChanAndVeseLevelSetFunction" in typehint:
        hint = "TypedScalarChanAndVeseLevelSetFunctionPROXY"
    elif "itkScalarChanAndVeseDenseLevelSetImageFilter" in typehint:
        hint = "TypedScalarChanAndVeseDenseLevelSetImageFilterPROXY"
    elif "itkScalarAnisotropicDiffusionFunction" in typehint:
        hint = "TypedScalarAnisotropicDiffusionFunctionPROXY"
    elif "itkScalableAffineTransform" in typehint:
        hint = "TypedScalableAffineTransformPROXY"
    elif "itkSamplingStrategy" in typehint:
        hint = "TypedSamplingStrategyPROXY"
    elif "itkSampleToSubsampleFilter" in typehint:
        hint = "TypedSampleToSubsampleFilterPROXY"
    elif "itkSampleToHistogramFilterException" in typehint:
        hint = "TypedSampleToHistogramFilterExceptionPROXY"
    elif "itkSampleToHistogramFilter" in typehint:
        hint = "TypedSampleToHistogramFilterPROXY"
    elif "itkSampleClassifierFilter" in typehint:
        hint = "TypedSampleClassifierFilterPROXY"
    elif "itkSample" in typehint:
        hint = "TypedSamplePROXY"
    elif "itkSameType" in typehint:
        hint = "TypedSameTypePROXY"
    elif "itkSameDimensionOrMinusOneOrTwo" in typehint:
        hint = "TypedSameDimensionOrMinusOneOrTwoPROXY"
    elif "itkSameDimensionOrMinusOne" in typehint:
        hint = "TypedSameDimensionOrMinusOnePROXY"
    elif "itkSameDimension" in typehint:
        hint = "TypedSameDimensionPROXY"
    elif "itkSaltAndPepperNoiseImageFilter" in typehint:
        hint = "TypedSaltAndPepperNoiseImageFilterPROXY"
    elif "itkRunLengthFeature" in typehint:
        hint = "TypedRunLengthFeaturePROXY"
    elif "itkRunLength" in typehint:
        hint = "TypedRunLengthPROXY"
    elif "itkRoundnessLabelObjectAccessor" in typehint:
        hint = "TypedRoundnessLabelObjectAccessorPROXY"
    elif "itkRoundImageFilter" in typehint:
        hint = "TypedRoundImageFilterPROXY"
    elif "itkRound" in typehint:
        hint = "TypedRoundPROXY"
    elif "itkRotationPlane" in typehint:
        hint = "TypedRotationPlanePROXY"
    elif "itkRobustAutomaticThresholdImageFilter" in typehint:
        hint = "TypedRobustAutomaticThresholdImageFilterPROXY"
    elif "itkRobustAutomaticThresholdCalculator" in typehint:
        hint = "TypedRobustAutomaticThresholdCalculatorPROXY"
    elif "itkRingBuffer" in typehint:
        hint = "TypedRingBufferPROXY"
    elif "itkRigid3DTransform" in typehint:
        hint = "TypedRigid3DTransformPROXY"
    elif "itkRigid3DTransform" in typehint:
        hint = "TypedRigid3DTransformPROXY"
    elif "itkRigid3DPerspectiveTransform" in typehint:
        hint = "TypedRigid3DPerspectiveTransformPROXY"
    elif "itkRigid2DTransform" in typehint:
        hint = "TypedRigid2DTransformPROXY"
    elif "itkRichardsonLucyDeconvolutionImageFilter" in typehint:
        hint = "TypedRichardsonLucyDeconvolutionImageFilterPROXY"
    elif "itkRGBToVectorPixelAccessor" in typehint:
        hint = "TypedRGBToVectorPixelAccessorPROXY"
    elif "itkRGBToVectorImageAdaptor" in typehint:
        hint = "TypedRGBToVectorImageAdaptorPROXY"
    elif "itkRGBToLuminancePixelAccessor" in typehint:
        hint = "TypedRGBToLuminancePixelAccessorPROXY"
    elif "itkRGBToLuminanceImageFilter" in typehint:
        hint = "TypedRGBToLuminanceImageFilterPROXY"
    elif "itkRGBToLuminanceImageAdaptor" in typehint:
        hint = "TypedRGBToLuminanceImageAdaptorPROXY"
    elif "itkRGBToLuminance" in typehint:
        hint = "TypedRGBToLuminancePROXY"
    elif "itkRGBPixel" in typehint:
        hint = "TypedRGBPixelPROXY"
    elif "itkRGBImageInfo" in typehint:
        hint = "TypedRGBImageInfoPROXY"
    elif "itkRGBGibbsPriorFilter" in typehint:
        hint = "TypedRGBGibbsPriorFilterPROXY"
    elif "itkRGBColormapFilter" in typehint:
        hint = "TypedRGBColormapFilterPROXY"
    elif "itkRGBAPixel" in typehint:
        hint = "TypedRGBAPixelPROXY"
    elif "itkReverseIterator" in typehint:
        hint = "TypedReverseIteratorPROXY"
    elif "itkReverseComparator" in typehint:
        hint = "TypedReverseComparatorPROXY"
    elif "itkReverseComparator" in typehint:
        hint = "TypedReverseComparatorPROXY"
    elif "itkResult" in typehint:
        hint = "TypedResultPROXY"
    elif "itkResourceProbesCollectorBase" in typehint:
        hint = "TypedResourceProbesCollectorBasePROXY"
    elif "itkResourceProbe" in typehint:
        hint = "TypedResourceProbePROXY"
    elif "itkRescaleIntensityImageFilter" in typehint:
        hint = "TypedRescaleIntensityImageFilterPROXY"
    elif "itkResampleInPlaceImageFilter" in typehint:
        hint = "TypedResampleInPlaceImageFilterPROXY"
    elif "itkResampleImageFilter" in typehint:
        hint = "TypedResampleImageFilterPROXY"
    elif "itkRenyiEntropyThresholdImageFilter" in typehint:
        hint = "TypedRenyiEntropyThresholdImageFilterPROXY"
    elif "itkRenyiEntropyThresholdCalculator" in typehint:
        hint = "TypedRenyiEntropyThresholdCalculatorPROXY"
    elif "itkRelabelLabelMapFilter" in typehint:
        hint = "TypedRelabelLabelMapFilterPROXY"
    elif "itkRelabeler" in typehint:
        hint = "TypedRelabelerPROXY"
    elif "itkRelabelComponentObjectType" in typehint:
        hint = "TypedRelabelComponentObjectTypePROXY"
    elif "itkRelabelComponentImageFilter" in typehint:
        hint = "TypedRelabelComponentImageFilterPROXY"
    elif "itkReinitializeLevelSetImageFilter" in typehint:
        hint = "TypedReinitializeLevelSetImageFilterPROXY"
    elif "itkRegularStepGradientDescentOptimizerv4" in typehint:
        hint = "TypedRegularStepGradientDescentOptimizerv4PROXY"
    elif "itkRegularStepGradientDescentOptimizer" in typehint:
        hint = "TypedRegularStepGradientDescentOptimizerPROXY"
    elif "itkRegularStepGradientDescentBaseOptimizerEnums" in typehint:
        hint = "TypedRegularStepGradientDescentBaseOptimizerEnumsPROXY"
    elif "itkRegularStepGradientDescentBaseOptimizer" in typehint:
        hint = "TypedRegularStepGradientDescentBaseOptimizerPROXY"
    elif "itkRegularSphereMeshSource" in typehint:
        hint = "TypedRegularSphereMeshSourcePROXY"
    elif "itkRegularizedHeavisideStepFunction" in typehint:
        hint = "TypedRegularizedHeavisideStepFunctionPROXY"
    elif "itkRegularExpressionSeriesFileNames" in typehint:
        hint = "TypedRegularExpressionSeriesFileNamesPROXY"
    elif "itkRegistrationParameterScalesFromShiftBase" in typehint:
        hint = "TypedRegistrationParameterScalesFromShiftBasePROXY"
    elif "itkRegistrationParameterScalesFromPhysicalShift" in typehint:
        hint = "TypedRegistrationParameterScalesFromPhysicalShiftPROXY"
    elif "itkRegistrationParameterScalesFromJacobian" in typehint:
        hint = "TypedRegistrationParameterScalesFromJacobianPROXY"
    elif "itkRegistrationParameterScalesFromIndexShift" in typehint:
        hint = "TypedRegistrationParameterScalesFromIndexShiftPROXY"
    elif "itkRegistrationParameterScalesEstimatorEnums" in typehint:
        hint = "TypedRegistrationParameterScalesEstimatorEnumsPROXY"
    elif "itkRegistrationParameterScalesEstimator" in typehint:
        hint = "TypedRegistrationParameterScalesEstimatorPROXY"
    elif "itkRegionType" in typehint:
        hint = "TypedRegionTypePROXY"
    elif "itkRegionOfInterestImageFilter" in typehint:
        hint = "TypedRegionOfInterestImageFilterPROXY"
    elif "itkRegionGrowImageFilter" in typehint:
        hint = "TypedRegionGrowImageFilterPROXY"
    elif "itkRegionFromReferenceLabelMapFilter" in typehint:
        hint = "TypedRegionFromReferenceLabelMapFilterPROXY"
    elif "itkRegionEnum" in typehint:
        hint = "TypedRegionEnumPROXY"
    elif "itkRegionData" in typehint:
        hint = "TypedRegionDataPROXY"
    elif "itkRegionConstrainedSubsampler" in typehint:
        hint = "TypedRegionConstrainedSubsamplerPROXY"
    elif "itkRegionBasedLevelSetFunctionSharedData" in typehint:
        hint = "TypedRegionBasedLevelSetFunctionSharedDataPROXY"
    elif "itkRegionBasedLevelSetFunctionData" in typehint:
        hint = "TypedRegionBasedLevelSetFunctionDataPROXY"
    elif "itkRegionBasedLevelSetFunction" in typehint:
        hint = "TypedRegionBasedLevelSetFunctionPROXY"
    elif "itkRegionAndCallback" in typehint:
        hint = "TypedRegionAndCallbackPROXY"
    elif "itkRegionalMinimaImageFilter" in typehint:
        hint = "TypedRegionalMinimaImageFilterPROXY"
    elif "itkRegionalMaximaImageFilter" in typehint:
        hint = "TypedRegionalMaximaImageFilterPROXY"
    elif "itkRegion" in typehint:
        hint = "TypedRegionPROXY"
    elif "itkReflectiveImageRegionIterator" in typehint:
        hint = "TypedReflectiveImageRegionIteratorPROXY"
    elif "itkReflectiveImageRegionConstIterator" in typehint:
        hint = "TypedReflectiveImageRegionConstIteratorPROXY"
    elif "itkRedPixelAccessor" in typehint:
        hint = "TypedRedPixelAccessorPROXY"
    elif "itkRedColormapFunction" in typehint:
        hint = "TypedRedColormapFunctionPROXY"
    elif "itkRecursiveSeparableImageFilter" in typehint:
        hint = "TypedRecursiveSeparableImageFilterPROXY"
    elif "itkRecursiveMultiResolutionPyramidImageFilter" in typehint:
        hint = "TypedRecursiveMultiResolutionPyramidImageFilterPROXY"
    elif "itkRecursiveGaussianImageFilterEnums" in typehint:
        hint = "TypedRecursiveGaussianImageFilterEnumsPROXY"
    elif "itkRecursiveGaussianImageFilter" in typehint:
        hint = "TypedRecursiveGaussianImageFilterPROXY"
    elif "itkRectangularImageNeighborhoodShape" in typehint:
        hint = "TypedRectangularImageNeighborhoodShapePROXY"
    elif "itkReconstructionImageFilter" in typehint:
        hint = "TypedReconstructionImageFilterPROXY"
    elif "itkReconstructionByErosionImageFilter" in typehint:
        hint = "TypedReconstructionByErosionImageFilterPROXY"
    elif "itkReconstructionByDilationImageFilter" in typehint:
        hint = "TypedReconstructionByDilationImageFilterPROXY"
    elif "itkReceptorMemberCommand" in typehint:
        hint = "TypedReceptorMemberCommandPROXY"
    elif "itkRebind" in typehint:
        hint = "TypedRebindPROXY"
    elif "itkRebind" in typehint:
        hint = "TypedRebindPROXY"
    elif "itkRebind" in typehint:
        hint = "TypedRebindPROXY"
    elif "itkRebind" in typehint:
        hint = "TypedRebindPROXY"
    elif "itkRebind" in typehint:
        hint = "TypedRebindPROXY"
    elif "itkRealToHalfHermitianForwardFFTImageFilter" in typehint:
        hint = "TypedRealToHalfHermitianForwardFFTImageFilterPROXY"
    elif "itkRealTimeStamp" in typehint:
        hint = "TypedRealTimeStampPROXY"
    elif "itkRealTimeInterval" in typehint:
        hint = "TypedRealTimeIntervalPROXY"
    elif "itkRealTimeClock" in typehint:
        hint = "TypedRealTimeClockPROXY"
    elif "itkReadFrom" in typehint:
        hint = "TypedReadFromPROXY"
    elif "itkRayCastInterpolateImageFunction" in typehint:
        hint = "TypedRayCastInterpolateImageFunctionPROXY"
    elif "itkRawImageIOFactory" in typehint:
        hint = "TypedRawImageIOFactoryPROXY"
    elif "itkRawImageIO" in typehint:
        hint = "TypedRawImageIOPROXY"
    elif "itkRankImageFilter" in typehint:
        hint = "TypedRankImageFilterPROXY"
    elif "itkRankHistogram" in typehint:
        hint = "TypedRankHistogramPROXY"
    elif "itkRandomVariateGeneratorBase" in typehint:
        hint = "TypedRandomVariateGeneratorBasePROXY"
    elif "itkRandomPermutation" in typehint:
        hint = "TypedRandomPermutationPROXY"
    elif "itkRandomImageSource" in typehint:
        hint = "TypedRandomImageSourcePROXY"
    elif "itkQuickView" in typehint:
        hint = "TypedQuickViewPROXY"
    elif "itkQuaternionRigidTransformGradientDescentOptimizer" in typehint:
        hint = "TypedQuaternionRigidTransformGradientDescentOptimizerPROXY"
    elif "itkQuaternionRigidTransform" in typehint:
        hint = "TypedQuaternionRigidTransformPROXY"
    elif "itkQuasiNewtonOptimizerv4Template" in typehint:
        hint = "TypedQuasiNewtonOptimizerv4TemplatePROXY"
    elif "itkQuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate" in typehint:
        hint = "TypedQuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplatePROXY"
    elif "itkQualifiedIterator" in typehint:
        hint = "TypedQualifiedIteratorPROXY"
    elif "itkQualifiedIterator" in typehint:
        hint = "TypedQualifiedIteratorPROXY"
    elif "itkQualifiedIterator" in typehint:
        hint = "TypedQualifiedIteratorPROXY"
    elif "itkQuadrilateralCellTopology" in typehint:
        hint = "TypedQuadrilateralCellTopologyPROXY"
    elif "itkQuadrilateralCell" in typehint:
        hint = "TypedQuadrilateralCellPROXY"
    elif "itkQuadricDecimationQuadEdgeMeshFilter" in typehint:
        hint = "TypedQuadricDecimationQuadEdgeMeshFilterPROXY"
    elif "itkQuadraticTriangleCellTopology" in typehint:
        hint = "TypedQuadraticTriangleCellTopologyPROXY"
    elif "itkQuadraticTriangleCell" in typehint:
        hint = "TypedQuadraticTriangleCellPROXY"
    elif "itkQuadraticEdgeCell" in typehint:
        hint = "TypedQuadraticEdgeCellPROXY"
    elif "itkQuadEdgeMeshZipMeshFunction" in typehint:
        hint = "TypedQuadEdgeMeshZipMeshFunctionPROXY"
    elif "itkQuadEdgeMeshTraits" in typehint:
        hint = "TypedQuadEdgeMeshTraitsPROXY"
    elif "itkQuadEdgeMeshToQuadEdgeMeshFilter" in typehint:
        hint = "TypedQuadEdgeMeshToQuadEdgeMeshFilterPROXY"
    elif "itkQuadEdgeMeshTopologyChecker" in typehint:
        hint = "TypedQuadEdgeMeshTopologyCheckerPROXY"
    elif "itkQuadEdgeMeshScalarDataVTKPolyDataWriter" in typehint:
        hint = "TypedQuadEdgeMeshScalarDataVTKPolyDataWriterPROXY"
    elif "itkQuadEdgeMeshScalarDataVTKPolyData" in typehint:
        hint = "TypedQuadEdgeMeshScalarDataVTKPolyDataPROXY"
    elif "itkQuadEdgeMeshPolygonCell" in typehint:
        hint = "TypedQuadEdgeMeshPolygonCellPROXY"
    elif "itkQuadEdgeMeshPoint" in typehint:
        hint = "TypedQuadEdgeMeshPointPROXY"
    elif "itkQuadEdgeMeshLineCell" in typehint:
        hint = "TypedQuadEdgeMeshLineCellPROXY"
    elif "itkQuadEdgeMeshIteratorGeom" in typehint:
        hint = "TypedQuadEdgeMeshIteratorGeomPROXY"
    elif "itkQuadEdgeMeshIterator" in typehint:
        hint = "TypedQuadEdgeMeshIteratorPROXY"
    elif "itkQuadEdgeMeshFunctionBase" in typehint:
        hint = "TypedQuadEdgeMeshFunctionBasePROXY"
    elif "itkQuadEdgeMeshFrontIterator" in typehint:
        hint = "TypedQuadEdgeMeshFrontIteratorPROXY"
    elif "itkQuadEdgeMeshFrontBaseIterator" in typehint:
        hint = "TypedQuadEdgeMeshFrontBaseIteratorPROXY"
    elif "itkQuadEdgeMeshExtendedTraits" in typehint:
        hint = "TypedQuadEdgeMeshExtendedTraitsPROXY"
    elif "itkQuadEdgeMeshEulerOperatorSplitVertexFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorSplitVertexFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorSplitFacetFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorSplitFacetFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorSplitEdgeFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorJoinVertexFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorJoinVertexFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorJoinFacetFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorJoinFacetFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorFlipEdgeFunctionEnumsPROXY"
    elif "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorFlipEdgeFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorDeleteCenterVertexFunctionPROXY"
    elif "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction" in typehint:
        hint = "TypedQuadEdgeMeshEulerOperatorCreateCenterVertexFunctionPROXY"
    elif "itkQuadEdgeMeshDecimationQuadricElementHelper" in typehint:
        hint = "TypedQuadEdgeMeshDecimationQuadricElementHelperPROXY"
    elif "itkQuadEdgeMeshDecimationCriterion" in typehint:
        hint = "TypedQuadEdgeMeshDecimationCriterionPROXY"
    elif "itkQuadEdgeMeshConstIteratorGeom" in typehint:
        hint = "TypedQuadEdgeMeshConstIteratorGeomPROXY"
    elif "itkQuadEdgeMeshConstIterator" in typehint:
        hint = "TypedQuadEdgeMeshConstIteratorPROXY"
    elif "itkQuadEdgeMeshConstFrontIterator" in typehint:
        hint = "TypedQuadEdgeMeshConstFrontIteratorPROXY"
    elif "itkQuadEdgeMeshBoundaryEdgesMeshFunction" in typehint:
        hint = "TypedQuadEdgeMeshBoundaryEdgesMeshFunctionPROXY"
    elif "itkQuadEdgeMeshBaseIterator" in typehint:
        hint = "TypedQuadEdgeMeshBaseIteratorPROXY"
    elif "itkQuadEdgeMesh" in typehint:
        hint = "TypedQuadEdgeMeshPROXY"
    elif "itkQuadEdge" in typehint:
        hint = "TypedQuadEdgePROXY"
    elif "itkProxy" in typehint:
        hint = "TypedProxyPROXY"
    elif "itkProxy" in typehint:
        hint = "TypedProxyPROXY"
    elif "itkProxy" in typehint:
        hint = "TypedProxyPROXY"
    elif "itkProjectionImageFilter" in typehint:
        hint = "TypedProjectionImageFilterPROXY"
    elif "itkProjectedLandweberDeconvolutionImageFilter" in typehint:
        hint = "TypedProjectedLandweberDeconvolutionImageFilterPROXY"
    elif "itkProjectedIterativeDeconvolutionImageFilter" in typehint:
        hint = "TypedProjectedIterativeDeconvolutionImageFilterPROXY"
    elif "itkProgressTransformer" in typehint:
        hint = "TypedProgressTransformerPROXY"
    elif "itkProgressReporter" in typehint:
        hint = "TypedProgressReporterPROXY"
    elif "itkProgressAccumulator" in typehint:
        hint = "TypedProgressAccumulatorPROXY"
    elif "itkProcessObject" in typehint:
        hint = "TypedProcessObjectPROXY"
    elif "itkProcessedOutputType" in typehint:
        hint = "TypedProcessedOutputTypePROXY"
    elif "itkProbabilityDistribution" in typehint:
        hint = "TypedProbabilityDistributionPROXY"
    elif "itkPriorityQueueContainer" in typehint:
        hint = "TypedPriorityQueueContainerPROXY"
    elif "itkPriorityLevel" in typehint:
        hint = "TypedPriorityLevelPROXY"
    elif "itkPrincipalMomentsLabelObjectAccessor" in typehint:
        hint = "TypedPrincipalMomentsLabelObjectAccessorPROXY"
    elif "itkPrincipalAxesLabelObjectAccessor" in typehint:
        hint = "TypedPrincipalAxesLabelObjectAccessorPROXY"
    elif "itkPowImageFilter" in typehint:
        hint = "TypedPowImageFilterPROXY"
    elif "itkPowellOptimizerv4" in typehint:
        hint = "TypedPowellOptimizerv4PROXY"
    elif "itkPowellOptimizer" in typehint:
        hint = "TypedPowellOptimizerPROXY"
    elif "itkPow" in typehint:
        hint = "TypedPowPROXY"
    elif "itkPoolMultiThreader" in typehint:
        hint = "TypedPoolMultiThreaderPROXY"
    elif "itkPolyLineParametricPath" in typehint:
        hint = "TypedPolyLineParametricPathPROXY"
    elif "itkPolylineMaskImageFilter" in typehint:
        hint = "TypedPolylineMaskImageFilterPROXY"
    elif "itkPolylineMask2DImageFilter" in typehint:
        hint = "TypedPolylineMask2DImageFilterPROXY"
    elif "itkPolygonSpatialObject" in typehint:
        hint = "TypedPolygonSpatialObjectPROXY"
    elif "itkPolygonGroupSpatialObjectXMLFileWriter" in typehint:
        hint = "TypedPolygonGroupSpatialObjectXMLFileWriterPROXY"
    elif "itkPolygonGroupSpatialObjectXMLFileReader" in typehint:
        hint = "TypedPolygonGroupSpatialObjectXMLFileReaderPROXY"
    elif "itkPolygonCell" in typehint:
        hint = "TypedPolygonCellPROXY"
    elif "itkPointsLocator" in typehint:
        hint = "TypedPointsLocatorPROXY"
    elif "itkPointSetToSpatialObjectDemonsRegistration" in typehint:
        hint = "TypedPointSetToSpatialObjectDemonsRegistrationPROXY"
    elif "itkPointSetToPointSetRegistrationMethod" in typehint:
        hint = "TypedPointSetToPointSetRegistrationMethodPROXY"
    elif "itkPointSetToPointSetMetricWithIndexv4" in typehint:
        hint = "TypedPointSetToPointSetMetricWithIndexv4PROXY"
    elif "itkPointSetToPointSetMetricv4" in typehint:
        hint = "TypedPointSetToPointSetMetricv4PROXY"
    elif "itkPointSetToPointSetMetric" in typehint:
        hint = "TypedPointSetToPointSetMetricPROXY"
    elif "itkPointSetToListSampleAdaptor" in typehint:
        hint = "TypedPointSetToListSampleAdaptorPROXY"
    elif "itkPointSetToImageRegistrationMethod" in typehint:
        hint = "TypedPointSetToImageRegistrationMethodPROXY"
    elif "itkPointSetToImageMetric" in typehint:
        hint = "TypedPointSetToImageMetricPROXY"
    elif "itkPointSetToImageFilter" in typehint:
        hint = "TypedPointSetToImageFilterPROXY"
    elif "itkPointSetFunction" in typehint:
        hint = "TypedPointSetFunctionPROXY"
    elif "itkPointSet" in typehint:
        hint = "TypedPointSetPROXY"
    elif "itkPointBasedSpatialObject" in typehint:
        hint = "TypedPointBasedSpatialObjectPROXY"
    elif "itkPoint1D" in typehint:
        hint = "TypedPoint1DPROXY"
    elif "itkPoint" in typehint:
        hint = "TypedPointPROXY"
    elif "itkPNGImageIOFactory" in typehint:
        hint = "TypedPNGImageIOFactoryPROXY"
    elif "itkPNGImageIO" in typehint:
        hint = "TypedPNGImageIOPROXY"
    elif "itkPixelTraits" in typehint:
        hint = "TypedPixelTraitsPROXY"
    elif "itkPixelSize" in typehint:
        hint = "TypedPixelSizePROXY"
    elif "itkPixelReferenceWrapper" in typehint:
        hint = "TypedPixelReferenceWrapperPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelProxy" in typehint:
        hint = "TypedPixelProxyPROXY"
    elif "itkPixelAccessor" in typehint:
        hint = "TypedPixelAccessorPROXY"
    elif "itkPipelineMonitorImageFilter" in typehint:
        hint = "TypedPipelineMonitorImageFilterPROXY"
    elif "itkPhysicalSizeLabelObjectAccessor" in typehint:
        hint = "TypedPhysicalSizeLabelObjectAccessorPROXY"
    elif "itkPhysicalPointImageSource" in typehint:
        hint = "TypedPhysicalPointImageSourcePROXY"
    elif "itkPhasedArray3DSpecialCoordinatesImage" in typehint:
        hint = "TypedPhasedArray3DSpecialCoordinatesImagePROXY"
    elif "itkPerThreadS" in typehint:
        hint = "TypedPerThreadSPROXY"
    elif "itkPermuteAxesImageFilter" in typehint:
        hint = "TypedPermuteAxesImageFilterPROXY"
    elif "itkPeriodicBoundaryCondition" in typehint:
        hint = "TypedPeriodicBoundaryConditionPROXY"
    elif "itkPerimeterOnBorderRatioLabelObjectAccessor" in typehint:
        hint = "TypedPerimeterOnBorderRatioLabelObjectAccessorPROXY"
    elif "itkPerimeterOnBorderLabelObjectAccessor" in typehint:
        hint = "TypedPerimeterOnBorderLabelObjectAccessorPROXY"
    elif "itkPerimeterLabelObjectAccessor" in typehint:
        hint = "TypedPerimeterLabelObjectAccessorPROXY"
    elif "itkPDEDeformableRegistrationFunction" in typehint:
        hint = "TypedPDEDeformableRegistrationFunctionPROXY"
    elif "itkPDEDeformableRegistrationFilter" in typehint:
        hint = "TypedPDEDeformableRegistrationFilterPROXY"
    elif "itkPCAShapeSignedDistanceFunction" in typehint:
        hint = "TypedPCAShapeSignedDistanceFunctionPROXY"
    elif "itkPathToPathFilter" in typehint:
        hint = "TypedPathToPathFilterPROXY"
    elif "itkPathToImageFilter" in typehint:
        hint = "TypedPathToImageFilterPROXY"
    elif "itkPathToChainCodePathFilter" in typehint:
        hint = "TypedPathToChainCodePathFilterPROXY"
    elif "itkPathSource" in typehint:
        hint = "TypedPathSourcePROXY"
    elif "itkPathIterator" in typehint:
        hint = "TypedPathIteratorPROXY"
    elif "itkPathConstIterator" in typehint:
        hint = "TypedPathConstIteratorPROXY"
    elif "itkPathAndImageToPathFilter" in typehint:
        hint = "TypedPathAndImageToPathFilterPROXY"
    elif "itkPath" in typehint:
        hint = "TypedPathPROXY"
    elif "itkPatchBasedDenoisingImageFilter" in typehint:
        hint = "TypedPatchBasedDenoisingImageFilterPROXY"
    elif "itkPatchBasedDenoisingBaseImageFilterEnums" in typehint:
        hint = "TypedPatchBasedDenoisingBaseImageFilterEnumsPROXY"
    elif "itkPatchBasedDenoisingBaseImageFilter" in typehint:
        hint = "TypedPatchBasedDenoisingBaseImageFilterPROXY"
    elif "itkPasteImageFilter" in typehint:
        hint = "TypedPasteImageFilterPROXY"
    elif "itkParticleSwarmOptimizerBase" in typehint:
        hint = "TypedParticleSwarmOptimizerBasePROXY"
    elif "itkParticleSwarmOptimizer" in typehint:
        hint = "TypedParticleSwarmOptimizerPROXY"
    elif "itkParticleData" in typehint:
        hint = "TypedParticleDataPROXY"
    elif "itkParametricSpaceToImageSpaceMeshFilter" in typehint:
        hint = "TypedParametricSpaceToImageSpaceMeshFilterPROXY"
    elif "itkParametricPath" in typehint:
        hint = "TypedParametricPathPROXY"
    elif "itkParametricImageSource" in typehint:
        hint = "TypedParametricImageSourcePROXY"
    elif "itkParametricBlindLeastSquaresDeconvolutionImageFilter" in typehint:
        hint = "TypedParametricBlindLeastSquaresDeconvolutionImageFilterPROXY"
    elif "itkParameterizationQuadEdgeMeshFilter" in typehint:
        hint = "TypedParameterizationQuadEdgeMeshFilterPROXY"
    elif "itkParallelSparseFieldLevelSetNode" in typehint:
        hint = "TypedParallelSparseFieldLevelSetNodePROXY"
    elif "itkParallelSparseFieldLevelSetImageFilter" in typehint:
        hint = "TypedParallelSparseFieldLevelSetImageFilterPROXY"
    elif "itkParallelSparseFieldCityBlockNeighborList" in typehint:
        hint = "TypedParallelSparseFieldCityBlockNeighborListPROXY"
    elif "itkPadLabelMapFilter" in typehint:
        hint = "TypedPadLabelMapFilterPROXY"
    elif "itkPadImageFilterBase" in typehint:
        hint = "TypedPadImageFilterBasePROXY"
    elif "itkPadImageFilter" in typehint:
        hint = "TypedPadImageFilterPROXY"
    elif "itkOverUnderColormapFunction" in typehint:
        hint = "TypedOverUnderColormapFunctionPROXY"
    elif "itkOverrideInformation" in typehint:
        hint = "TypedOverrideInformationPROXY"
    elif "itkOutputWindow" in typehint:
        hint = "TypedOutputWindowPROXY"
    elif "itkOutputTypeSpecializationStructType" in typehint:
        hint = "TypedOutputTypeSpecializationStructTypePROXY"
    elif "itkOutputDataObjectIterator" in typehint:
        hint = "TypedOutputDataObjectIteratorPROXY"
    elif "itkOutputDataObjectConstIterator" in typehint:
        hint = "TypedOutputDataObjectConstIteratorPROXY"
    elif "itkOtsuThresholdImageFilter" in typehint:
        hint = "TypedOtsuThresholdImageFilterPROXY"
    elif "itkOtsuThresholdCalculator" in typehint:
        hint = "TypedOtsuThresholdCalculatorPROXY"
    elif "itkOtsuMultipleThresholdsImageFilter" in typehint:
        hint = "TypedOtsuMultipleThresholdsImageFilterPROXY"
    elif "itkOtsuMultipleThresholdsCalculator" in typehint:
        hint = "TypedOtsuMultipleThresholdsCalculatorPROXY"
    elif "itkOStreamWritable" in typehint:
        hint = "TypedOStreamWritablePROXY"
    elif "itkOrthogonalSwath2DPathFilter" in typehint:
        hint = "TypedOrthogonalSwath2DPathFilterPROXY"
    elif "itkOrthogonallyCorrected2DParametricPath" in typehint:
        hint = "TypedOrthogonallyCorrected2DParametricPathPROXY"
    elif "itkOrImageFilter" in typehint:
        hint = "TypedOrImageFilterPROXY"
    elif "itkOrientImageFilter" in typehint:
        hint = "TypedOrientImageFilterPROXY"
    elif "itkOrC" in typehint:
        hint = "TypedOrCPROXY"
    elif "itkOr" in typehint:
        hint = "TypedOrPROXY"
    elif "itkOR" in typehint:
        hint = "TypedORPROXY"
    elif "itkOptionalPixelAccessParameter" in typehint:
        hint = "TypedOptionalPixelAccessParameterPROXY"
    elif "itkOptionalPixelAccessParameter" in typehint:
        hint = "TypedOptionalPixelAccessParameterPROXY"
    elif "itkOptimizerParametersHelper" in typehint:
        hint = "TypedOptimizerParametersHelperPROXY"
    elif "itkOptimizerParameterScalesEstimatorTemplate" in typehint:
        hint = "TypedOptimizerParameterScalesEstimatorTemplatePROXY"
    elif "itkOptimizerParameters" in typehint:
        hint = "TypedOptimizerParametersPROXY"
    elif "itkOptimizer" in typehint:
        hint = "TypedOptimizerPROXY"
    elif "itkOptimization" in typehint:
        hint = "TypedOptimizationPROXY"
    elif "itkOperation" in typehint:
        hint = "TypedOperationPROXY"
    elif "itkOpeningByReconstructionImageFilter" in typehint:
        hint = "TypedOpeningByReconstructionImageFilterPROXY"
    elif "itkOpenCVVideoIOFactory" in typehint:
        hint = "TypedOpenCVVideoIOFactoryPROXY"
    elif "itkOpenCVVideoIO" in typehint:
        hint = "TypedOpenCVVideoIOPROXY"
    elif "itkOpenCVVideoCapture" in typehint:
        hint = "TypedOpenCVVideoCapturePROXY"
    elif "itkOpenCVImageBridge" in typehint:
        hint = "TypedOpenCVImageBridgePROXY"
    elif "itkOpenCVBasicTypeBridge" in typehint:
        hint = "TypedOpenCVBasicTypeBridgePROXY"
    elif "itkOpenCVBasicTypeBridge" in typehint:
        hint = "TypedOpenCVBasicTypeBridgePROXY"
    elif "itkOpenCVBasicTypeBridge" in typehint:
        hint = "TypedOpenCVBasicTypeBridgePROXY"
    elif "itkOpenCVBasicTypeBridge" in typehint:
        hint = "TypedOpenCVBasicTypeBridgePROXY"
    elif "itkOneWayEquivalencyTable" in typehint:
        hint = "TypedOneWayEquivalencyTablePROXY"
    elif "itkOnesMatrixCoefficients" in typehint:
        hint = "TypedOnesMatrixCoefficientsPROXY"
    elif "itkOnePlusOneEvolutionaryOptimizerv4" in typehint:
        hint = "TypedOnePlusOneEvolutionaryOptimizerv4PROXY"
    elif "itkOnePlusOneEvolutionaryOptimizer" in typehint:
        hint = "TypedOnePlusOneEvolutionaryOptimizerPROXY"
    elif "itkOffset" in typehint:
        hint = "TypedOffsetPROXY"
    elif "itkOFFMeshIOFactory" in typehint:
        hint = "TypedOFFMeshIOFactoryPROXY"
    elif "itkOFFMeshIO" in typehint:
        hint = "TypedOFFMeshIOPROXY"
    elif "itkOctreeNodeBranch" in typehint:
        hint = "TypedOctreeNodeBranchPROXY"
    elif "itkOctreeNode" in typehint:
        hint = "TypedOctreeNodePROXY"
    elif "itkOctreeEnums" in typehint:
        hint = "TypedOctreeEnumsPROXY"
    elif "itkOctreeBase" in typehint:
        hint = "TypedOctreeBasePROXY"
    elif "itkOctree" in typehint:
        hint = "TypedOctreePROXY"
    elif "itkOctree" in typehint:
        hint = "TypedOctreePROXY"
    elif "itkOBJMeshIOFactory" in typehint:
        hint = "TypedOBJMeshIOFactoryPROXY"
    elif "itkOBJMeshIO" in typehint:
        hint = "TypedOBJMeshIOPROXY"
    elif "itkObjectToObjectOptimizerBaseTemplateEnums" in typehint:
        hint = "TypedObjectToObjectOptimizerBaseTemplateEnumsPROXY"
    elif "itkObjectToObjectOptimizerBaseTemplate" in typehint:
        hint = "TypedObjectToObjectOptimizerBaseTemplatePROXY"
    elif "itkObjectToObjectMultiMetricv4" in typehint:
        hint = "TypedObjectToObjectMultiMetricv4PROXY"
    elif "itkObjectToObjectMetricBaseTemplateEnums" in typehint:
        hint = "TypedObjectToObjectMetricBaseTemplateEnumsPROXY"
    elif "itkObjectToObjectMetricBaseTemplate" in typehint:
        hint = "TypedObjectToObjectMetricBaseTemplatePROXY"
    elif "itkObjectToObjectMetric" in typehint:
        hint = "TypedObjectToObjectMetricPROXY"
    elif "itkObjectStoreEnums" in typehint:
        hint = "TypedObjectStoreEnumsPROXY"
    elif "itkObjectStore" in typehint:
        hint = "TypedObjectStorePROXY"
    elif "itkObjectMorphologyImageFilter" in typehint:
        hint = "TypedObjectMorphologyImageFilterPROXY"
    elif "itkObjectFactoryEnums" in typehint:
        hint = "TypedObjectFactoryEnumsPROXY"
    elif "itkObjectFactoryBase" in typehint:
        hint = "TypedObjectFactoryBasePROXY"
    elif "itkObjectFactory" in typehint:
        hint = "TypedObjectFactoryPROXY"
    elif "itkObjectEnums" in typehint:
        hint = "TypedObjectEnumsPROXY"
    elif "itkObjectByObjectLabelMapFilter" in typehint:
        hint = "TypedObjectByObjectLabelMapFilterPROXY"
    elif "itkObject" in typehint:
        hint = "TypedObjectPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericTraits" in typehint:
        hint = "TypedNumericTraitsPROXY"
    elif "itkNumericSeriesFileNames" in typehint:
        hint = "TypedNumericSeriesFileNamesPROXY"
    elif "itkNumberToString" in typehint:
        hint = "TypedNumberToStringPROXY"
    elif "itkNumberOfPointsCriterion" in typehint:
        hint = "TypedNumberOfPointsCriterionPROXY"
    elif "itkNumberOfPixelsOnBorderLabelObjectAccessor" in typehint:
        hint = "TypedNumberOfPixelsOnBorderLabelObjectAccessorPROXY"
    elif "itkNumberOfPixelsLabelObjectAccessor" in typehint:
        hint = "TypedNumberOfPixelsLabelObjectAccessorPROXY"
    elif "itkNumberOfLinesLabelObjectAccessor" in typehint:
        hint = "TypedNumberOfLinesLabelObjectAccessorPROXY"
    elif "itkNumberOfFacesCriterion" in typehint:
        hint = "TypedNumberOfFacesCriterionPROXY"
    elif "itkNullSizeHistogramInputMeasurementVectorSize" in typehint:
        hint = "TypedNullSizeHistogramInputMeasurementVectorSizePROXY"
    elif "itkNthElementPixelAccessor" in typehint:
        hint = "TypedNthElementPixelAccessorPROXY"
    elif "itkNthElementImageAdaptorHelper" in typehint:
        hint = "TypedNthElementImageAdaptorHelperPROXY"
    elif "itkNthElementImageAdaptor" in typehint:
        hint = "TypedNthElementImageAdaptorPROXY"
    elif "itkNrrdImageIOFactory" in typehint:
        hint = "TypedNrrdImageIOFactoryPROXY"
    elif "itkNrrdImageIO" in typehint:
        hint = "TypedNrrdImageIOPROXY"
    elif "itkNotOperator" in typehint:
        hint = "TypedNotOperatorPROXY"
    elif "itkNotImageFilter" in typehint:
        hint = "TypedNotImageFilterPROXY"
    elif "itkNotEqual" in typehint:
        hint = "TypedNotEqualPROXY"
    elif "itkNotC" in typehint:
        hint = "TypedNotCPROXY"
    elif "itkNot" in typehint:
        hint = "TypedNotPROXY"
    elif "itkNOT" in typehint:
        hint = "TypedNOTPROXY"
    elif "itkNormalVectorFunctionBase" in typehint:
        hint = "TypedNormalVectorFunctionBasePROXY"
    elif "itkNormalVectorDiffusionFunction" in typehint:
        hint = "TypedNormalVectorDiffusionFunctionPROXY"
    elif "itkNormalVariateGenerator" in typehint:
        hint = "TypedNormalVariateGeneratorPROXY"
    elif "itkNormalQuadEdgeMeshFilterEnums" in typehint:
        hint = "TypedNormalQuadEdgeMeshFilterEnumsPROXY"
    elif "itkNormalQuadEdgeMeshFilter" in typehint:
        hint = "TypedNormalQuadEdgeMeshFilterPROXY"
    elif "itkNormalizeToConstantImageFilter" in typehint:
        hint = "TypedNormalizeToConstantImageFilterPROXY"
    elif "itkNormalizeImageFilter" in typehint:
        hint = "TypedNormalizeImageFilterPROXY"
    elif "itkNormalizedMutualInformationHistogramImageToImageMetric" in typehint:
        hint = "TypedNormalizedMutualInformationHistogramImageToImageMetricPROXY"
    elif "itkNormalizedCorrelationPointSetToImageMetric" in typehint:
        hint = "TypedNormalizedCorrelationPointSetToImageMetricPROXY"
    elif "itkNormalizedCorrelationImageToImageMetric" in typehint:
        hint = "TypedNormalizedCorrelationImageToImageMetricPROXY"
    elif "itkNormalizedCorrelationImageFilter" in typehint:
        hint = "TypedNormalizedCorrelationImageFilterPROXY"
    elif "itkNormalBandNode" in typehint:
        hint = "TypedNormalBandNodePROXY"
    elif "itkNonLinearOptimizer" in typehint:
        hint = "TypedNonLinearOptimizerPROXY"
    elif "itkNoiseModel" in typehint:
        hint = "TypedNoiseModelPROXY"
    elif "itkNoiseImageFilter" in typehint:
        hint = "TypedNoiseImageFilterPROXY"
    elif "itkNoiseBaseImageFilter" in typehint:
        hint = "TypedNoiseBaseImageFilterPROXY"
    elif "itkNodePair" in typehint:
        hint = "TypedNodePairPROXY"
    elif "itkNodeOfPermutation" in typehint:
        hint = "TypedNodeOfPermutationPROXY"
    elif "itkNiftiImageIOFactory" in typehint:
        hint = "TypedNiftiImageIOFactoryPROXY"
    elif "itkNiftiImageIOEnums" in typehint:
        hint = "TypedNiftiImageIOEnumsPROXY"
    elif "itkNiftiImageIO" in typehint:
        hint = "TypedNiftiImageIOPROXY"
    elif "itkNiftiFileEnum" in typehint:
        hint = "TypedNiftiFileEnumPROXY"
    elif "itkNeverReallocate" in typehint:
        hint = "TypedNeverReallocatePROXY"
    elif "itkNeighborhoodSampler" in typehint:
        hint = "TypedNeighborhoodSamplerPROXY"
    elif "itkNeighborhoodOperatorImageFunction" in typehint:
        hint = "TypedNeighborhoodOperatorImageFunctionPROXY"
    elif "itkNeighborhoodOperatorImageFilter" in typehint:
        hint = "TypedNeighborhoodOperatorImageFilterPROXY"
    elif "itkNeighborhoodOperator" in typehint:
        hint = "TypedNeighborhoodOperatorPROXY"
    elif "itkNeighborhoodIterator" in typehint:
        hint = "TypedNeighborhoodIteratorPROXY"
    elif "itkNeighborhoodInnerProduct" in typehint:
        hint = "TypedNeighborhoodInnerProductPROXY"
    elif "itkNeighborhoodConnectedImageFilter" in typehint:
        hint = "TypedNeighborhoodConnectedImageFilterPROXY"
    elif "itkNeighborhoodBinaryThresholdImageFunction" in typehint:
        hint = "TypedNeighborhoodBinaryThresholdImageFunctionPROXY"
    elif "itkNeighborhoodAllocator" in typehint:
        hint = "TypedNeighborhoodAllocatorPROXY"
    elif "itkNeighborhoodAccessorFunctor" in typehint:
        hint = "TypedNeighborhoodAccessorFunctorPROXY"
    elif "itkNeighborhood" in typehint:
        hint = "TypedNeighborhoodPROXY"
    elif "itkNearestNeighbors" in typehint:
        hint = "TypedNearestNeighborsPROXY"
    elif "itkNearestNeighborInterpolateImageFunction" in typehint:
        hint = "TypedNearestNeighborInterpolateImageFunctionPROXY"
    elif "itkNearestNeighborExtrapolateImageFunction" in typehint:
        hint = "TypedNearestNeighborExtrapolateImageFunctionPROXY"
    elif "itkNaryMaximumImageFilter" in typehint:
        hint = "TypedNaryMaximumImageFilterPROXY"
    elif "itkNaryFunctorImageFilter" in typehint:
        hint = "TypedNaryFunctorImageFilterPROXY"
    elif "itkNaryAddImageFilter" in typehint:
        hint = "TypedNaryAddImageFilterPROXY"
    elif "itkNarrowBandThresholdSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedNarrowBandThresholdSegmentationLevelSetImageFilterPROXY"
    elif "itkNarrowBandLevelSetImageFilter" in typehint:
        hint = "TypedNarrowBandLevelSetImageFilterPROXY"
    elif "itkNarrowBandImageFilterBase" in typehint:
        hint = "TypedNarrowBandImageFilterBasePROXY"
    elif "itkNarrowBandCurvesLevelSetImageFilter" in typehint:
        hint = "TypedNarrowBandCurvesLevelSetImageFilterPROXY"
    elif "itkNarrowBand" in typehint:
        hint = "TypedNarrowBandPROXY"
    elif "itkN4BiasFieldCorrectionImageFilter" in typehint:
        hint = "TypedN4BiasFieldCorrectionImageFilterPROXY"
    elif "itkMutualInformationImageToImageMetric" in typehint:
        hint = "TypedMutualInformationImageToImageMetricPROXY"
    elif "itkMutualInformationHistogramImageToImageMetric" in typehint:
        hint = "TypedMutualInformationHistogramImageToImageMetricPROXY"
    elif "itkMultiVisitor" in typehint:
        hint = "TypedMultiVisitorPROXY"
    elif "itkMultivariateLegendrePolynomial" in typehint:
        hint = "TypedMultivariateLegendrePolynomialPROXY"
    elif "itkMultiTransform" in typehint:
        hint = "TypedMultiTransformPROXY"
    elif "itkMultiThreaderWorkUnitInfoImageToImageMetricWrapper" in typehint:
        hint = "TypedMultiThreaderWorkUnitInfoImageToImageMetricWrapperPROXY"
    elif "itkMultiThreaderBaseEnums" in typehint:
        hint = "TypedMultiThreaderBaseEnumsPROXY"
    elif "itkMultiThreaderBase" in typehint:
        hint = "TypedMultiThreaderBasePROXY"
    elif "itkMultiStartOptimizerv4Template" in typehint:
        hint = "TypedMultiStartOptimizerv4TemplatePROXY"
    elif "itkMultiScaleHessianBasedMeasureImageFilterEnums" in typehint:
        hint = "TypedMultiScaleHessianBasedMeasureImageFilterEnumsPROXY"
    elif "itkMultiScaleHessianBasedMeasureImageFilter" in typehint:
        hint = "TypedMultiScaleHessianBasedMeasureImageFilterPROXY"
    elif "itkMultiResolutionPyramidImageFilter" in typehint:
        hint = "TypedMultiResolutionPyramidImageFilterPROXY"
    elif "itkMultiResolutionPDEDeformableRegistration" in typehint:
        hint = "TypedMultiResolutionPDEDeformableRegistrationPROXY"
    elif "itkMultiResolutionImageRegistrationMethod" in typehint:
        hint = "TypedMultiResolutionImageRegistrationMethodPROXY"
    elif "itkMultiplyOperator" in typehint:
        hint = "TypedMultiplyOperatorPROXY"
    elif "itkMultiplyImageFilter" in typehint:
        hint = "TypedMultiplyImageFilterPROXY"
    elif "itkMultiplyAndAssignOperator" in typehint:
        hint = "TypedMultiplyAndAssignOperatorPROXY"
    elif "itkMultipleValuedVnlCostFunctionAdaptor" in typehint:
        hint = "TypedMultipleValuedVnlCostFunctionAdaptorPROXY"
    elif "itkMultipleValuedNonLinearVnlOptimizer" in typehint:
        hint = "TypedMultipleValuedNonLinearVnlOptimizerPROXY"
    elif "itkMultipleValuedNonLinearOptimizer" in typehint:
        hint = "TypedMultipleValuedNonLinearOptimizerPROXY"
    elif "itkMultipleValuedCostFunction" in typehint:
        hint = "TypedMultipleValuedCostFunctionPROXY"
    elif "itkMultipleLogOutput" in typehint:
        hint = "TypedMultipleLogOutputPROXY"
    elif "itkMultiphaseSparseFiniteDifferenceImageFilter" in typehint:
        hint = "TypedMultiphaseSparseFiniteDifferenceImageFilterPROXY"
    elif "itkMultiphaseFiniteDifferenceImageFilter" in typehint:
        hint = "TypedMultiphaseFiniteDifferenceImageFilterPROXY"
    elif "itkMultiphaseDenseFiniteDifferenceImageFilter" in typehint:
        hint = "TypedMultiphaseDenseFiniteDifferenceImageFilterPROXY"
    elif "itkMultiLabelSTAPLEImageFilter" in typehint:
        hint = "TypedMultiLabelSTAPLEImageFilterPROXY"
    elif "itkMultiGradientOptimizerv4Template" in typehint:
        hint = "TypedMultiGradientOptimizerv4TemplatePROXY"
    elif "itkMult" in typehint:
        hint = "TypedMultPROXY"
    elif "itkMRIBiasFieldCorrectionFilter" in typehint:
        hint = "TypedMRIBiasFieldCorrectionFilterPROXY"
    elif "itkMRIBiasEnergyFunction" in typehint:
        hint = "TypedMRIBiasEnergyFunctionPROXY"
    elif "itkMRFStop" in typehint:
        hint = "TypedMRFStopPROXY"
    elif "itkMRFImageFilterEnums" in typehint:
        hint = "TypedMRFImageFilterEnumsPROXY"
    elif "itkMRFImageFilter" in typehint:
        hint = "TypedMRFImageFilterPROXY"
    elif "itkMRCImageIOFactory" in typehint:
        hint = "TypedMRCImageIOFactoryPROXY"
    elif "itkMRCImageIO" in typehint:
        hint = "TypedMRCImageIOPROXY"
    elif "itkMRCHeaderObject" in typehint:
        hint = "TypedMRCHeaderObjectPROXY"
    elif "itkMRASlabIdentifier" in typehint:
        hint = "TypedMRASlabIdentifierPROXY"
    elif "itkMovingHistogramMorphologyImageFilter" in typehint:
        hint = "TypedMovingHistogramMorphologyImageFilterPROXY"
    elif "itkMovingHistogramMorphologicalGradientImageFilter" in typehint:
        hint = "TypedMovingHistogramMorphologicalGradientImageFilterPROXY"
    elif "itkMovingHistogramImageFilterBase" in typehint:
        hint = "TypedMovingHistogramImageFilterBasePROXY"
    elif "itkMovingHistogramImageFilter" in typehint:
        hint = "TypedMovingHistogramImageFilterPROXY"
    elif "itkMovingHistogramErodeImageFilter" in typehint:
        hint = "TypedMovingHistogramErodeImageFilterPROXY"
    elif "itkMovingHistogramDilateImageFilter" in typehint:
        hint = "TypedMovingHistogramDilateImageFilterPROXY"
    elif "itkMorphologyImageFilter" in typehint:
        hint = "TypedMorphologyImageFilterPROXY"
    elif "itkMorphologyHistogram" in typehint:
        hint = "TypedMorphologyHistogramPROXY"
    elif "itkMorphologicalWatershedImageFilter" in typehint:
        hint = "TypedMorphologicalWatershedImageFilterPROXY"
    elif "itkMorphologicalWatershedFromMarkersImageFilter" in typehint:
        hint = "TypedMorphologicalWatershedFromMarkersImageFilterPROXY"
    elif "itkMorphologicalGradientImageFilter" in typehint:
        hint = "TypedMorphologicalGradientImageFilterPROXY"
    elif "itkMorphologicalGradientHistogram" in typehint:
        hint = "TypedMorphologicalGradientHistogramPROXY"
    elif "itkMomentsThresholdImageFilter" in typehint:
        hint = "TypedMomentsThresholdImageFilterPROXY"
    elif "itkMomentsThresholdCalculator" in typehint:
        hint = "TypedMomentsThresholdCalculatorPROXY"
    elif "itkModulusSquare3" in typehint:
        hint = "TypedModulusSquare3PROXY"
    elif "itkModulusImageFilter" in typehint:
        hint = "TypedModulusImageFilterPROXY"
    elif "itkModulus3" in typehint:
        hint = "TypedModulus3PROXY"
    elif "itkModulus2" in typehint:
        hint = "TypedModulus2PROXY"
    elif "itkModulus" in typehint:
        hint = "TypedModulusPROXY"
    elif "itkMMIMetricPerThreadStruct" in typehint:
        hint = "TypedMMIMetricPerThreadStructPROXY"
    elif "itkMixtureModelComponentBase" in typehint:
        hint = "TypedMixtureModelComponentBasePROXY"
    elif "itkMissingHistogramSizeInput" in typehint:
        hint = "TypedMissingHistogramSizeInputPROXY"
    elif "itkMissingHistogramMarginalScaleInput" in typehint:
        hint = "TypedMissingHistogramMarginalScaleInputPROXY"
    elif "itkMissingHistogramBinMinimumInput" in typehint:
        hint = "TypedMissingHistogramBinMinimumInputPROXY"
    elif "itkMissingHistogramBinMaximumInput" in typehint:
        hint = "TypedMissingHistogramBinMaximumInputPROXY"
    elif "itkMirrorPadImageFilter" in typehint:
        hint = "TypedMirrorPadImageFilterPROXY"
    elif "itkMinPriorityQueueElementWrapper" in typehint:
        hint = "TypedMinPriorityQueueElementWrapperPROXY"
    elif "itkMinMeasureBoundCriterion" in typehint:
        hint = "TypedMinMeasureBoundCriterionPROXY"
    elif "itkMinMaxCurvatureFlowImageFilter" in typehint:
        hint = "TypedMinMaxCurvatureFlowImageFilterPROXY"
    elif "itkMinMaxCurvatureFlowFunction" in typehint:
        hint = "TypedMinMaxCurvatureFlowFunctionPROXY"
    elif "itkMiniPipelineSeparableImageFilter" in typehint:
        hint = "TypedMiniPipelineSeparableImageFilterPROXY"
    elif "itkMinimumProjectionImageFilter" in typehint:
        hint = "TypedMinimumProjectionImageFilterPROXY"
    elif "itkMinimumMaximumImageFilter" in typehint:
        hint = "TypedMinimumMaximumImageFilterPROXY"
    elif "itkMinimumMaximumImageCalculator" in typehint:
        hint = "TypedMinimumMaximumImageCalculatorPROXY"
    elif "itkMinimumLabelObjectAccessor" in typehint:
        hint = "TypedMinimumLabelObjectAccessorPROXY"
    elif "itkMinimumIndexLabelObjectAccessor" in typehint:
        hint = "TypedMinimumIndexLabelObjectAccessorPROXY"
    elif "itkMinimumImageFilter" in typehint:
        hint = "TypedMinimumImageFilterPROXY"
    elif "itkMinimumDecisionRule" in typehint:
        hint = "TypedMinimumDecisionRulePROXY"
    elif "itkMinimumAccumulator" in typehint:
        hint = "TypedMinimumAccumulatorPROXY"
    elif "itkMinimum" in typehint:
        hint = "TypedMinimumPROXY"
    elif "itkMinFunctor" in typehint:
        hint = "TypedMinFunctorPROXY"
    elif "itkMINCImageIOFactory" in typehint:
        hint = "TypedMINCImageIOFactoryPROXY"
    elif "itkMINCImageIO" in typehint:
        hint = "TypedMINCImageIOPROXY"
    elif "itkMetricSamplingStrategy" in typehint:
        hint = "TypedMetricSamplingStrategyPROXY"
    elif "itkMetricCategory" in typehint:
        hint = "TypedMetricCategoryPROXY"
    elif "itkMetaVesselTubeConverter" in typehint:
        hint = "TypedMetaVesselTubeConverterPROXY"
    elif "itkMetaTubeConverter" in typehint:
        hint = "TypedMetaTubeConverterPROXY"
    elif "itkMetaSurfaceConverter" in typehint:
        hint = "TypedMetaSurfaceConverterPROXY"
    elif "itkMetaSceneConverter" in typehint:
        hint = "TypedMetaSceneConverterPROXY"
    elif "itkMetaMeshConverter" in typehint:
        hint = "TypedMetaMeshConverterPROXY"
    elif "itkMetaLineConverter" in typehint:
        hint = "TypedMetaLineConverterPROXY"
    elif "itkMetaLandmarkConverter" in typehint:
        hint = "TypedMetaLandmarkConverterPROXY"
    elif "itkMetaImageMaskConverter" in typehint:
        hint = "TypedMetaImageMaskConverterPROXY"
    elif "itkMetaImageIOFactory" in typehint:
        hint = "TypedMetaImageIOFactoryPROXY"
    elif "itkMetaImageIO" in typehint:
        hint = "TypedMetaImageIOPROXY"
    elif "itkMetaImageConverter" in typehint:
        hint = "TypedMetaImageConverterPROXY"
    elif "itkMetaGroupConverter" in typehint:
        hint = "TypedMetaGroupConverterPROXY"
    elif "itkMetaGaussianConverter" in typehint:
        hint = "TypedMetaGaussianConverterPROXY"
    elif "itkMetaEvent" in typehint:
        hint = "TypedMetaEventPROXY"
    elif "itkMetaEllipseConverter" in typehint:
        hint = "TypedMetaEllipseConverterPROXY"
    elif "itkMetaDTITubeConverter" in typehint:
        hint = "TypedMetaDTITubeConverterPROXY"
    elif "itkMetaDataObjectBase" in typehint:
        hint = "TypedMetaDataObjectBasePROXY"
    elif "itkMetaDataObject" in typehint:
        hint = "TypedMetaDataObjectPROXY"
    elif "itkMetaDataDictionary" in typehint:
        hint = "TypedMetaDataDictionaryPROXY"
    elif "itkMetaConverterBase" in typehint:
        hint = "TypedMetaConverterBasePROXY"
    elif "itkMetaContourConverter" in typehint:
        hint = "TypedMetaContourConverterPROXY"
    elif "itkMetaBlobConverter" in typehint:
        hint = "TypedMetaBlobConverterPROXY"
    elif "itkMetaArrowConverter" in typehint:
        hint = "TypedMetaArrowConverterPROXY"
    elif "itkMetaArrayWriter" in typehint:
        hint = "TypedMetaArrayWriterPROXY"
    elif "itkMetaArrayReader" in typehint:
        hint = "TypedMetaArrayReaderPROXY"
    elif "itkMeshToMeshFilter" in typehint:
        hint = "TypedMeshToMeshFilterPROXY"
    elif "itkMeshSpatialObject" in typehint:
        hint = "TypedMeshSpatialObjectPROXY"
    elif "itkMeshSource" in typehint:
        hint = "TypedMeshSourcePROXY"
    elif "itkMeshRegion" in typehint:
        hint = "TypedMeshRegionPROXY"
    elif "itkMeshIOFactory" in typehint:
        hint = "TypedMeshIOFactoryPROXY"
    elif "itkMeshIOBase" in typehint:
        hint = "TypedMeshIOBasePROXY"
    elif "itkMeshFunctionBase" in typehint:
        hint = "TypedMeshFunctionBasePROXY"
    elif "itkMeshFileWriterException" in typehint:
        hint = "TypedMeshFileWriterExceptionPROXY"
    elif "itkMeshFileWriter" in typehint:
        hint = "TypedMeshFileWriterPROXY"
    elif "itkMeshFileReaderException" in typehint:
        hint = "TypedMeshFileReaderExceptionPROXY"
    elif "itkMeshFileReader" in typehint:
        hint = "TypedMeshFileReaderPROXY"
    elif "itkMeshEnums" in typehint:
        hint = "TypedMeshEnumsPROXY"
    elif "itkMeshConvertPixelTraits" in typehint:
        hint = "TypedMeshConvertPixelTraitsPROXY"
    elif "itkMeshClassCellsAllocationMethod" in typehint:
        hint = "TypedMeshClassCellsAllocationMethodPROXY"
    elif "itkMesh" in typehint:
        hint = "TypedMeshPROXY"
    elif "itkMersenneTwisterRandomVariateGenerator" in typehint:
        hint = "TypedMersenneTwisterRandomVariateGeneratorPROXY"
    elif "itkMergeLabelMapFilterEnums" in typehint:
        hint = "TypedMergeLabelMapFilterEnumsPROXY"
    elif "itkMergeLabelMapFilter" in typehint:
        hint = "TypedMergeLabelMapFilterPROXY"
    elif "itkmerge_t" in typehint:
        hint = "Typedmerge_tPROXY"
    elif "itkmerge_comp" in typehint:
        hint = "Typedmerge_compPROXY"
    elif "itkMemoryUsageObserverBase" in typehint:
        hint = "TypedMemoryUsageObserverBasePROXY"
    elif "itkMemoryUsageObserver" in typehint:
        hint = "TypedMemoryUsageObserverPROXY"
    elif "itkMemoryProbesCollectorBase" in typehint:
        hint = "TypedMemoryProbesCollectorBasePROXY"
    elif "itkMemoryProbe" in typehint:
        hint = "TypedMemoryProbePROXY"
    elif "itkMemoryBlock" in typehint:
        hint = "TypedMemoryBlockPROXY"
    elif "itkMembershipSample" in typehint:
        hint = "TypedMembershipSamplePROXY"
    elif "itkMembershipFunctionBase" in typehint:
        hint = "TypedMembershipFunctionBasePROXY"
    elif "itkMemberCommand" in typehint:
        hint = "TypedMemberCommandPROXY"
    elif "itkMedianProjectionImageFilter" in typehint:
        hint = "TypedMedianProjectionImageFilterPROXY"
    elif "itkMedianLabelObjectAccessor" in typehint:
        hint = "TypedMedianLabelObjectAccessorPROXY"
    elif "itkMedianImageFunction" in typehint:
        hint = "TypedMedianImageFunctionPROXY"
    elif "itkMedianImageFilter" in typehint:
        hint = "TypedMedianImageFilterPROXY"
    elif "itkMedianAccumulator" in typehint:
        hint = "TypedMedianAccumulatorPROXY"
    elif "itkMeasurementVectorTraitsTypes" in typehint:
        hint = "TypedMeasurementVectorTraitsTypesPROXY"
    elif "itkMeasurementVectorTraitsTypes" in typehint:
        hint = "TypedMeasurementVectorTraitsTypesPROXY"
    elif "itkMeasurementVectorTraits" in typehint:
        hint = "TypedMeasurementVectorTraitsPROXY"
    elif "itkMeasurementVectorPixelTraits" in typehint:
        hint = "TypedMeasurementVectorPixelTraitsPROXY"
    elif "itkMeanSquaresPointSetToImageMetric" in typehint:
        hint = "TypedMeanSquaresPointSetToImageMetricPROXY"
    elif "itkMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedMeanSquaresImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkMeanSquaresImageToImageMetricv4" in typehint:
        hint = "TypedMeanSquaresImageToImageMetricv4PROXY"
    elif "itkMeanSquaresImageToImageMetric" in typehint:
        hint = "TypedMeanSquaresImageToImageMetricPROXY"
    elif "itkMeanSquaresHistogramImageToImageMetric" in typehint:
        hint = "TypedMeanSquaresHistogramImageToImageMetricPROXY"
    elif "itkMeanSquareRegistrationFunction" in typehint:
        hint = "TypedMeanSquareRegistrationFunctionPROXY"
    elif "itkMeanSampleFilter" in typehint:
        hint = "TypedMeanSampleFilterPROXY"
    elif "itkMeanReciprocalSquareDifferencePointSetToImageMetric" in typehint:
        hint = "TypedMeanReciprocalSquareDifferencePointSetToImageMetricPROXY"
    elif "itkMeanReciprocalSquareDifferenceImageToImageMetric" in typehint:
        hint = "TypedMeanReciprocalSquareDifferenceImageToImageMetricPROXY"
    elif "itkMeanProjectionImageFilter" in typehint:
        hint = "TypedMeanProjectionImageFilterPROXY"
    elif "itkMeanLabelObjectAccessor" in typehint:
        hint = "TypedMeanLabelObjectAccessorPROXY"
    elif "itkMeanImageFunction" in typehint:
        hint = "TypedMeanImageFunctionPROXY"
    elif "itkMeanImageFilter" in typehint:
        hint = "TypedMeanImageFilterPROXY"
    elif "itkMeanAccumulator" in typehint:
        hint = "TypedMeanAccumulatorPROXY"
    elif "itkMaxPriorityQueueElementWrapper" in typehint:
        hint = "TypedMaxPriorityQueueElementWrapperPROXY"
    elif "itkMaxMeasureBoundCriterion" in typehint:
        hint = "TypedMaxMeasureBoundCriterionPROXY"
    elif "itkMaximumRatioDecisionRule" in typehint:
        hint = "TypedMaximumRatioDecisionRulePROXY"
    elif "itkMaximumProjectionImageFilter" in typehint:
        hint = "TypedMaximumProjectionImageFilterPROXY"
    elif "itkMaximumLabelObjectAccessor" in typehint:
        hint = "TypedMaximumLabelObjectAccessorPROXY"
    elif "itkMaximumIndexLabelObjectAccessor" in typehint:
        hint = "TypedMaximumIndexLabelObjectAccessorPROXY"
    elif "itkMaximumImageFilter" in typehint:
        hint = "TypedMaximumImageFilterPROXY"
    elif "itkMaximumEntropyThresholdImageFilter" in typehint:
        hint = "TypedMaximumEntropyThresholdImageFilterPROXY"
    elif "itkMaximumEntropyThresholdCalculator" in typehint:
        hint = "TypedMaximumEntropyThresholdCalculatorPROXY"
    elif "itkMaximumDecisionRule" in typehint:
        hint = "TypedMaximumDecisionRulePROXY"
    elif "itkMaximumAccumulator" in typehint:
        hint = "TypedMaximumAccumulatorPROXY"
    elif "itkMaximum1" in typehint:
        hint = "TypedMaximum1PROXY"
    elif "itkMaximum" in typehint:
        hint = "TypedMaximumPROXY"
    elif "itkMaxFunctor" in typehint:
        hint = "TypedMaxFunctorPROXY"
    elif "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkMattesMutualInformationImageToImageMetricv4" in typehint:
        hint = "TypedMattesMutualInformationImageToImageMetricv4PROXY"
    elif "itkMattesMutualInformationImageToImageMetric" in typehint:
        hint = "TypedMattesMutualInformationImageToImageMetricPROXY"
    elif "itkMatrixResizeableDataObject" in typehint:
        hint = "TypedMatrixResizeableDataObjectPROXY"
    elif "itkMatrixOrthogonalityTolerance" in typehint:
        hint = "TypedMatrixOrthogonalityTolerancePROXY"
    elif "itkMatrixOrthogonalityTolerance" in typehint:
        hint = "TypedMatrixOrthogonalityTolerancePROXY"
    elif "itkMatrixOrthogonalityTolerance" in typehint:
        hint = "TypedMatrixOrthogonalityTolerancePROXY"
    elif "itkMatrixOffsetTransformBase" in typehint:
        hint = "TypedMatrixOffsetTransformBasePROXY"
    elif "itkMatrixIndexSelectionImageFilter" in typehint:
        hint = "TypedMatrixIndexSelectionImageFilterPROXY"
    elif "itkMatrixIndexSelection" in typehint:
        hint = "TypedMatrixIndexSelectionPROXY"
    elif "itkMatrixCoefficients" in typehint:
        hint = "TypedMatrixCoefficientsPROXY"
    elif "itkMatrix" in typehint:
        hint = "TypedMatrixPROXY"
    elif "itkMatlabTransformIOTemplate" in typehint:
        hint = "TypedMatlabTransformIOTemplatePROXY"
    elif "itkMatlabTransformIOFactory" in typehint:
        hint = "TypedMatlabTransformIOFactoryPROXY"
    elif "itkMathematicalMorphologyEnums" in typehint:
        hint = "TypedMathematicalMorphologyEnumsPROXY"
    elif "itkMatchCardinalityImageToImageMetric" in typehint:
        hint = "TypedMatchCardinalityImageToImageMetricPROXY"
    elif "itkMaskNeighborhoodOperatorImageFilter" in typehint:
        hint = "TypedMaskNeighborhoodOperatorImageFilterPROXY"
    elif "itkMaskNegatedInput" in typehint:
        hint = "TypedMaskNegatedInputPROXY"
    elif "itkMaskNegatedImageFilter" in typehint:
        hint = "TypedMaskNegatedImageFilterPROXY"
    elif "itkMaskInput" in typehint:
        hint = "TypedMaskInputPROXY"
    elif "itkMaskImageFilter" in typehint:
        hint = "TypedMaskImageFilterPROXY"
    elif "itkMaskFeaturePointSelectionFilter" in typehint:
        hint = "TypedMaskFeaturePointSelectionFilterPROXY"
    elif "itkMaskedRankImageFilter" in typehint:
        hint = "TypedMaskedRankImageFilterPROXY"
    elif "itkMaskedMovingHistogramImageFilter" in typehint:
        hint = "TypedMaskedMovingHistogramImageFilterPROXY"
    elif "itkMaskedImageToHistogramFilter" in typehint:
        hint = "TypedMaskedImageToHistogramFilterPROXY"
    elif "itkMaskedFFTNormalizedCorrelationImageFilter" in typehint:
        hint = "TypedMaskedFFTNormalizedCorrelationImageFilterPROXY"
    elif "itkMapRecord" in typehint:
        hint = "TypedMapRecordPROXY"
    elif "itkMapPixelType" in typehint:
        hint = "TypedMapPixelTypePROXY"
    elif "itkMapFileParser" in typehint:
        hint = "TypedMapFileParserPROXY"
    elif "itkMapData" in typehint:
        hint = "TypedMapDataPROXY"
    elif "itkMapContainer" in typehint:
        hint = "TypedMapContainerPROXY"
    elif "itkMapComponentType" in typehint:
        hint = "TypedMapComponentTypePROXY"
    elif "itkManualWisdomFilenameGenerator" in typehint:
        hint = "TypedManualWisdomFilenameGeneratorPROXY"
    elif "itkManifoldParzenWindowsPointSetFunction" in typehint:
        hint = "TypedManifoldParzenWindowsPointSetFunctionPROXY"
    elif "itkManhattanDistanceMetric" in typehint:
        hint = "TypedManhattanDistanceMetricPROXY"
    elif "itkMalcolmSparseLevelSetImage" in typehint:
        hint = "TypedMalcolmSparseLevelSetImagePROXY"
    elif "itkMakeJoin" in typehint:
        hint = "TypedMakeJoinPROXY"
    elif "itkMahalanobisDistanceThresholdImageFunction" in typehint:
        hint = "TypedMahalanobisDistanceThresholdImageFunctionPROXY"
    elif "itkMahalanobisDistanceMetric" in typehint:
        hint = "TypedMahalanobisDistanceMetricPROXY"
    elif "itkMahalanobisDistanceMembershipFunction" in typehint:
        hint = "TypedMahalanobisDistanceMembershipFunctionPROXY"
    elif "itkMagnitudeAndPhaseToComplexImageFilter" in typehint:
        hint = "TypedMagnitudeAndPhaseToComplexImageFilterPROXY"
    elif "itkMagnitudeAndPhaseToComplex" in typehint:
        hint = "TypedMagnitudeAndPhaseToComplexPROXY"
    elif "itkLSMImageIOFactory" in typehint:
        hint = "TypedLSMImageIOFactoryPROXY"
    elif "itkLSMImageIO" in typehint:
        hint = "TypedLSMImageIOPROXY"
    elif "itkLogPixelAccessor" in typehint:
        hint = "TypedLogPixelAccessorPROXY"
    elif "itkLogOutput" in typehint:
        hint = "TypedLogOutputPROXY"
    elif "itkLogLevel" in typehint:
        hint = "TypedLogLevelPROXY"
    elif "itkLogImageFilter" in typehint:
        hint = "TypedLogImageFilterPROXY"
    elif "itkLogImageAdaptor" in typehint:
        hint = "TypedLogImageAdaptorPROXY"
    elif "itkLogicOpBase" in typehint:
        hint = "TypedLogicOpBasePROXY"
    elif "itkLoggerThreadWrapperEnums" in typehint:
        hint = "TypedLoggerThreadWrapperEnumsPROXY"
    elif "itkLoggerThreadWrapper" in typehint:
        hint = "TypedLoggerThreadWrapperPROXY"
    elif "itkLoggerOutput" in typehint:
        hint = "TypedLoggerOutputPROXY"
    elif "itkLoggerManager" in typehint:
        hint = "TypedLoggerManagerPROXY"
    elif "itkLoggerBaseEnums" in typehint:
        hint = "TypedLoggerBaseEnumsPROXY"
    elif "itkLoggerBase" in typehint:
        hint = "TypedLoggerBasePROXY"
    elif "itkLogger" in typehint:
        hint = "TypedLoggerPROXY"
    elif "itkLog10PixelAccessor" in typehint:
        hint = "TypedLog10PixelAccessorPROXY"
    elif "itkLog10ImageFilter" in typehint:
        hint = "TypedLog10ImageFilterPROXY"
    elif "itkLog10ImageAdaptor" in typehint:
        hint = "TypedLog10ImageAdaptorPROXY"
    elif "itkLog10" in typehint:
        hint = "TypedLog10PROXY"
    elif "itkLog" in typehint:
        hint = "TypedLogPROXY"
    elif "itkLiThresholdImageFilter" in typehint:
        hint = "TypedLiThresholdImageFilterPROXY"
    elif "itkLiThresholdCalculator" in typehint:
        hint = "TypedLiThresholdCalculatorPROXY"
    elif "itkListSample" in typehint:
        hint = "TypedListSamplePROXY"
    elif "itkListNode" in typehint:
        hint = "TypedListNodePROXY"
    elif "itkLineSpatialObjectPoint" in typehint:
        hint = "TypedLineSpatialObjectPointPROXY"
    elif "itkLineSpatialObject" in typehint:
        hint = "TypedLineSpatialObjectPROXY"
    elif "itkLineOfLabelObjectComparator" in typehint:
        hint = "TypedLineOfLabelObjectComparatorPROXY"
    elif "itkLineOfLabelObjectComparator" in typehint:
        hint = "TypedLineOfLabelObjectComparatorPROXY"
    elif "itkLineOfLabelObject" in typehint:
        hint = "TypedLineOfLabelObjectPROXY"
    elif "itkLineOfLabelObject" in typehint:
        hint = "TypedLineOfLabelObjectPROXY"
    elif "itkLineIterator" in typehint:
        hint = "TypedLineIteratorPROXY"
    elif "itkLineConstIterator" in typehint:
        hint = "TypedLineConstIteratorPROXY"
    elif "itkLineCell" in typehint:
        hint = "TypedLineCellPROXY"
    elif "itkLinearInterpolateImageFunction" in typehint:
        hint = "TypedLinearInterpolateImageFunctionPROXY"
    elif "itkLightProcessObject" in typehint:
        hint = "TypedLightProcessObjectPROXY"
    elif "itkLightObject" in typehint:
        hint = "TypedLightObjectPROXY"
    elif "itkLexicographicCompare" in typehint:
        hint = "TypedLexicographicComparePROXY"
    elif "itkLevenbergMarquardtOptimizer" in typehint:
        hint = "TypedLevenbergMarquardtOptimizerPROXY"
    elif "itkLevelSetVelocityNeighborhoodExtractor" in typehint:
        hint = "TypedLevelSetVelocityNeighborhoodExtractorPROXY"
    elif "itkLevelSetTypeDefault" in typehint:
        hint = "TypedLevelSetTypeDefaultPROXY"
    elif "itkLevelSetSparseImage" in typehint:
        hint = "TypedLevelSetSparseImagePROXY"
    elif "itkLevelSetQuadEdgeMesh" in typehint:
        hint = "TypedLevelSetQuadEdgeMeshPROXY"
    elif "itkLevelSetNode" in typehint:
        hint = "TypedLevelSetNodePROXY"
    elif "itkLevelSetNeighborhoodExtractor" in typehint:
        hint = "TypedLevelSetNeighborhoodExtractorPROXY"
    elif "itkLevelSetMotionRegistrationFunction" in typehint:
        hint = "TypedLevelSetMotionRegistrationFunctionPROXY"
    elif "itkLevelSetMotionRegistrationFilter" in typehint:
        hint = "TypedLevelSetMotionRegistrationFilterPROXY"
    elif "itkLevelSetImage" in typehint:
        hint = "TypedLevelSetImagePROXY"
    elif "itkLevelSetFunctionWithRefitTerm" in typehint:
        hint = "TypedLevelSetFunctionWithRefitTermPROXY"
    elif "itkLevelSetFunction" in typehint:
        hint = "TypedLevelSetFunctionPROXY"
    elif "itkLevelSetEvolutionUpdateLevelSetsThreader" in typehint:
        hint = "TypedLevelSetEvolutionUpdateLevelSetsThreaderPROXY"
    elif "itkLevelSetEvolutionUpdateLevelSetsThreader" in typehint:
        hint = "TypedLevelSetEvolutionUpdateLevelSetsThreaderPROXY"
    elif "itkLevelSetEvolutionStoppingCriterion" in typehint:
        hint = "TypedLevelSetEvolutionStoppingCriterionPROXY"
    elif "itkLevelSetEvolutionComputeIterationThreader" in typehint:
        hint = "TypedLevelSetEvolutionComputeIterationThreaderPROXY"
    elif "itkLevelSetEvolutionComputeIterationThreader" in typehint:
        hint = "TypedLevelSetEvolutionComputeIterationThreaderPROXY"
    elif "itkLevelSetEvolutionComputeIterationThreader" in typehint:
        hint = "TypedLevelSetEvolutionComputeIterationThreaderPROXY"
    elif "itkLevelSetEvolutionComputeIterationThreader" in typehint:
        hint = "TypedLevelSetEvolutionComputeIterationThreaderPROXY"
    elif "itkLevelSetEvolutionBase" in typehint:
        hint = "TypedLevelSetEvolutionBasePROXY"
    elif "itkLevelSetEvolution" in typehint:
        hint = "TypedLevelSetEvolutionPROXY"
    elif "itkLevelSetEvolution" in typehint:
        hint = "TypedLevelSetEvolutionPROXY"
    elif "itkLevelSetEvolution" in typehint:
        hint = "TypedLevelSetEvolutionPROXY"
    elif "itkLevelSetEvolution" in typehint:
        hint = "TypedLevelSetEvolutionPROXY"
    elif "itkLevelSetEvolution" in typehint:
        hint = "TypedLevelSetEvolutionPROXY"
    elif "itkLevelSetEquationTermContainer" in typehint:
        hint = "TypedLevelSetEquationTermContainerPROXY"
    elif "itkLevelSetEquationTermBase" in typehint:
        hint = "TypedLevelSetEquationTermBasePROXY"
    elif "itkLevelSetEquationRegionTerm" in typehint:
        hint = "TypedLevelSetEquationRegionTermPROXY"
    elif "itkLevelSetEquationPropagationTerm" in typehint:
        hint = "TypedLevelSetEquationPropagationTermPROXY"
    elif "itkLevelSetEquationOverlapPenaltyTerm" in typehint:
        hint = "TypedLevelSetEquationOverlapPenaltyTermPROXY"
    elif "itkLevelSetEquationLaplacianTerm" in typehint:
        hint = "TypedLevelSetEquationLaplacianTermPROXY"
    elif "itkLevelSetEquationCurvatureTerm" in typehint:
        hint = "TypedLevelSetEquationCurvatureTermPROXY"
    elif "itkLevelSetEquationContainer" in typehint:
        hint = "TypedLevelSetEquationContainerPROXY"
    elif "itkLevelSetEquationChanAndVeseInternalTerm" in typehint:
        hint = "TypedLevelSetEquationChanAndVeseInternalTermPROXY"
    elif "itkLevelSetEquationChanAndVeseExternalTerm" in typehint:
        hint = "TypedLevelSetEquationChanAndVeseExternalTermPROXY"
    elif "itkLevelSetEquationBinaryMaskTerm" in typehint:
        hint = "TypedLevelSetEquationBinaryMaskTermPROXY"
    elif "itkLevelSetEquationAdvectionTerm" in typehint:
        hint = "TypedLevelSetEquationAdvectionTermPROXY"
    elif "itkLevelSetDomainPartitionMesh" in typehint:
        hint = "TypedLevelSetDomainPartitionMeshPROXY"
    elif "itkLevelSetDomainPartitionImageWithKdTree" in typehint:
        hint = "TypedLevelSetDomainPartitionImageWithKdTreePROXY"
    elif "itkLevelSetDomainPartitionImage" in typehint:
        hint = "TypedLevelSetDomainPartitionImagePROXY"
    elif "itkLevelSetDomainPartitionBase" in typehint:
        hint = "TypedLevelSetDomainPartitionBasePROXY"
    elif "itkLevelSetDomainPartition" in typehint:
        hint = "TypedLevelSetDomainPartitionPROXY"
    elif "itkLevelSetDomainMapImageFilter" in typehint:
        hint = "TypedLevelSetDomainMapImageFilterPROXY"
    elif "itkLevelSetDomain" in typehint:
        hint = "TypedLevelSetDomainPROXY"
    elif "itkLevelSetDenseImage" in typehint:
        hint = "TypedLevelSetDenseImagePROXY"
    elif "itkLevelSetDataType" in typehint:
        hint = "TypedLevelSetDataTypePROXY"
    elif "itkLevelSetContainerBase" in typehint:
        hint = "TypedLevelSetContainerBasePROXY"
    elif "itkLevelSetContainer" in typehint:
        hint = "TypedLevelSetContainerPROXY"
    elif "itkLevelSetContainer" in typehint:
        hint = "TypedLevelSetContainerPROXY"
    elif "itkLevelSetBase" in typehint:
        hint = "TypedLevelSetBasePROXY"
    elif "itkLessThanComparable" in typehint:
        hint = "TypedLessThanComparablePROXY"
    elif "itkLessEqual" in typehint:
        hint = "TypedLessEqualPROXY"
    elif "itkLess" in typehint:
        hint = "TypedLessPROXY"
    elif "itkLBFGSOptimizerv4" in typehint:
        hint = "TypedLBFGSOptimizerv4PROXY"
    elif "itkLBFGSOptimizerBasev4" in typehint:
        hint = "TypedLBFGSOptimizerBasev4PROXY"
    elif "itkLBFGSOptimizerBaseHelperv4" in typehint:
        hint = "TypedLBFGSOptimizerBaseHelperv4PROXY"
    elif "itkLBFGSOptimizer" in typehint:
        hint = "TypedLBFGSOptimizerPROXY"
    elif "itkLBFGSBOptimizerv4" in typehint:
        hint = "TypedLBFGSBOptimizerv4PROXY"
    elif "itkLBFGSBOptimizerHelperv4" in typehint:
        hint = "TypedLBFGSBOptimizerHelperv4PROXY"
    elif "itkLBFGSBOptimizerHelper" in typehint:
        hint = "TypedLBFGSBOptimizerHelperPROXY"
    elif "itkLBFGSBOptimizer" in typehint:
        hint = "TypedLBFGSBOptimizerPROXY"
    elif "itkLBFGS2Optimizerv4Enums" in typehint:
        hint = "TypedLBFGS2Optimizerv4EnumsPROXY"
    elif "itkLBFGS2Optimizerv4" in typehint:
        hint = "TypedLBFGS2Optimizerv4PROXY"
    elif "itkLaplacianSharpeningImageFilter" in typehint:
        hint = "TypedLaplacianSharpeningImageFilterPROXY"
    elif "itkLaplacianSegmentationLevelSetImageFilter" in typehint:
        hint = "TypedLaplacianSegmentationLevelSetImageFilterPROXY"
    elif "itkLaplacianSegmentationLevelSetFunction" in typehint:
        hint = "TypedLaplacianSegmentationLevelSetFunctionPROXY"
    elif "itkLaplacianRecursiveGaussianImageFilter" in typehint:
        hint = "TypedLaplacianRecursiveGaussianImageFilterPROXY"
    elif "itkLaplacianOperator" in typehint:
        hint = "TypedLaplacianOperatorPROXY"
    elif "itkLaplacianImageFilter" in typehint:
        hint = "TypedLaplacianImageFilterPROXY"
    elif "itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints" in typehint:
        hint = "TypedLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraintsPROXY"
    elif "itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraints" in typehint:
        hint = "TypedLaplacianDeformationQuadEdgeMeshFilterWithHardConstraintsPROXY"
    elif "itkLaplacianDeformationQuadEdgeMeshFilterEnums" in typehint:
        hint = "TypedLaplacianDeformationQuadEdgeMeshFilterEnumsPROXY"
    elif "itkLaplacianDeformationQuadEdgeMeshFilter" in typehint:
        hint = "TypedLaplacianDeformationQuadEdgeMeshFilterPROXY"
    elif "itkLandweberMethod" in typehint:
        hint = "TypedLandweberMethodPROXY"
    elif "itkLandweberDeconvolutionImageFilter" in typehint:
        hint = "TypedLandweberDeconvolutionImageFilterPROXY"
    elif "itkLandmarkSpatialObject" in typehint:
        hint = "TypedLandmarkSpatialObjectPROXY"
    elif "itkLandmarkDisplacementFieldSource" in typehint:
        hint = "TypedLandmarkDisplacementFieldSourcePROXY"
    elif "itkLandmarkBasedTransformInitializer" in typehint:
        hint = "TypedLandmarkBasedTransformInitializerPROXY"
    elif "itkLanczosWindowFunction" in typehint:
        hint = "TypedLanczosWindowFunctionPROXY"
    elif "itkLabelVotingImageFilter" in typehint:
        hint = "TypedLabelVotingImageFilterPROXY"
    elif "itkLabelUniqueLabelMapFilter" in typehint:
        hint = "TypedLabelUniqueLabelMapFilterPROXY"
    elif "itkLabelToRGBImageFilter" in typehint:
        hint = "TypedLabelToRGBImageFilterPROXY"
    elif "itkLabelToRGBFunctor" in typehint:
        hint = "TypedLabelToRGBFunctorPROXY"
    elif "itkLabelStatisticsOpeningImageFilter" in typehint:
        hint = "TypedLabelStatisticsOpeningImageFilterPROXY"
    elif "itkLabelStatisticsKeepNObjectsImageFilter" in typehint:
        hint = "TypedLabelStatisticsKeepNObjectsImageFilterPROXY"
    elif "itkLabelStatisticsImageFilter" in typehint:
        hint = "TypedLabelStatisticsImageFilterPROXY"
    elif "itkLabelStatistics" in typehint:
        hint = "TypedLabelStatisticsPROXY"
    elif "itkLabelShapeOpeningImageFilter" in typehint:
        hint = "TypedLabelShapeOpeningImageFilterPROXY"
    elif "itkLabelShapeKeepNObjectsImageFilter" in typehint:
        hint = "TypedLabelShapeKeepNObjectsImageFilterPROXY"
    elif "itkLabelSetMeasures" in typehint:
        hint = "TypedLabelSetMeasuresPROXY"
    elif "itkLabelSelectionLabelMapFilter" in typehint:
        hint = "TypedLabelSelectionLabelMapFilterPROXY"
    elif "itkLabelOverlayImageFilter" in typehint:
        hint = "TypedLabelOverlayImageFilterPROXY"
    elif "itkLabelOverlayFunctor" in typehint:
        hint = "TypedLabelOverlayFunctorPROXY"
    elif "itkLabelOverlapMeasuresImageFilter" in typehint:
        hint = "TypedLabelOverlapMeasuresImageFilterPROXY"
    elif "itkLabelObjectReverseComparator" in typehint:
        hint = "TypedLabelObjectReverseComparatorPROXY"
    elif "itkLabelObjectLineComparator" in typehint:
        hint = "TypedLabelObjectLineComparatorPROXY"
    elif "itkLabelObjectLine" in typehint:
        hint = "TypedLabelObjectLinePROXY"
    elif "itkLabelObjectComparator" in typehint:
        hint = "TypedLabelObjectComparatorPROXY"
    elif "itkLabelObject" in typehint:
        hint = "TypedLabelObjectPROXY"
    elif "itkLabelMapToRGBImageFilter" in typehint:
        hint = "TypedLabelMapToRGBImageFilterPROXY"
    elif "itkLabelMapToLabelImageFilter" in typehint:
        hint = "TypedLabelMapToLabelImageFilterPROXY"
    elif "itkLabelMapToBinaryImageFilter" in typehint:
        hint = "TypedLabelMapToBinaryImageFilterPROXY"
    elif "itkLabelMapToAttributeImageFilter" in typehint:
        hint = "TypedLabelMapToAttributeImageFilterPROXY"
    elif "itkLabelMapOverlayImageFilter" in typehint:
        hint = "TypedLabelMapOverlayImageFilterPROXY"
    elif "itkLabelMapMaskImageFilter" in typehint:
        hint = "TypedLabelMapMaskImageFilterPROXY"
    elif "itkLabelMapFilter" in typehint:
        hint = "TypedLabelMapFilterPROXY"
    elif "itkLabelMapContourOverlayImageFilter" in typehint:
        hint = "TypedLabelMapContourOverlayImageFilterPROXY"
    elif "itkLabelMap" in typehint:
        hint = "TypedLabelMapPROXY"
    elif "itkLabelLabelObjectAccessor" in typehint:
        hint = "TypedLabelLabelObjectAccessorPROXY"
    elif "itkLabelImageToStatisticsLabelMapFilter" in typehint:
        hint = "TypedLabelImageToStatisticsLabelMapFilterPROXY"
    elif "itkLabelImageToShapeLabelMapFilter" in typehint:
        hint = "TypedLabelImageToShapeLabelMapFilterPROXY"
    elif "itkLabelImageToLabelMapFilter" in typehint:
        hint = "TypedLabelImageToLabelMapFilterPROXY"
    elif "itkLabelImageGaussianInterpolateImageFunction" in typehint:
        hint = "TypedLabelImageGaussianInterpolateImageFunctionPROXY"
    elif "itkLabelGeometryImageFilter" in typehint:
        hint = "TypedLabelGeometryImageFilterPROXY"
    elif "itkLabelGeometry" in typehint:
        hint = "TypedLabelGeometryPROXY"
    elif "itkLabeledPointSetToPointSetMetricv4" in typehint:
        hint = "TypedLabeledPointSetToPointSetMetricv4PROXY"
    elif "itkLabelContourImageFilter" in typehint:
        hint = "TypedLabelContourImageFilterPROXY"
    elif "itkLabel" in typehint:
        hint = "TypedLabelPROXY"
    elif "itkKurtosisLabelObjectAccessor" in typehint:
        hint = "TypedKurtosisLabelObjectAccessorPROXY"
    elif "itkKullbackLeiblerCompareHistogramImageToImageMetric" in typehint:
        hint = "TypedKullbackLeiblerCompareHistogramImageToImageMetricPROXY"
    elif "itkKLMSegmentationRegion" in typehint:
        hint = "TypedKLMSegmentationRegionPROXY"
    elif "itkKLMSegmentationBorder" in typehint:
        hint = "TypedKLMSegmentationBorderPROXY"
    elif "itkKLMRegionGrowImageFilter" in typehint:
        hint = "TypedKLMRegionGrowImageFilterPROXY"
    elif "itkKLMDynamicBorderArray" in typehint:
        hint = "TypedKLMDynamicBorderArrayPROXY"
    elif "itkKittlerIllingworthThresholdImageFilter" in typehint:
        hint = "TypedKittlerIllingworthThresholdImageFilterPROXY"
    elif "itkKittlerIllingworthThresholdCalculator" in typehint:
        hint = "TypedKittlerIllingworthThresholdCalculatorPROXY"
    elif "itkKernelTransform" in typehint:
        hint = "TypedKernelTransformPROXY"
    elif "itkKernelImageFilter" in typehint:
        hint = "TypedKernelImageFilterPROXY"
    elif "itkKernelFunctionBase" in typehint:
        hint = "TypedKernelFunctionBasePROXY"
    elif "itkKernelArgumentList" in typehint:
        hint = "TypedKernelArgumentListPROXY"
    elif "itkKeepValuesRootPolicy" in typehint:
        hint = "TypedKeepValuesRootPolicyPROXY"
    elif "itkKeepOldValues" in typehint:
        hint = "TypedKeepOldValuesPROXY"
    elif "itkKdTreeWeightedCentroidNonterminalNode" in typehint:
        hint = "TypedKdTreeWeightedCentroidNonterminalNodePROXY"
    elif "itkKdTreeTerminalNode" in typehint:
        hint = "TypedKdTreeTerminalNodePROXY"
    elif "itkKdTreeNonterminalNode" in typehint:
        hint = "TypedKdTreeNonterminalNodePROXY"
    elif "itkKdTreeNode" in typehint:
        hint = "TypedKdTreeNodePROXY"
    elif "itkKdTreeGenerator" in typehint:
        hint = "TypedKdTreeGeneratorPROXY"
    elif "itkKdTreeBasedKmeansEstimator" in typehint:
        hint = "TypedKdTreeBasedKmeansEstimatorPROXY"
    elif "itkKdTree" in typehint:
        hint = "TypedKdTreePROXY"
    elif "itkKappaStatisticImageToImageMetric" in typehint:
        hint = "TypedKappaStatisticImageToImageMetricPROXY"
    elif "itkKappaSigmaThresholdImageFilter" in typehint:
        hint = "TypedKappaSigmaThresholdImageFilterPROXY"
    elif "itkKappaSigmaThresholdImageCalculator" in typehint:
        hint = "TypedKappaSigmaThresholdImageCalculatorPROXY"
    elif "itkKalmanLinearEstimator" in typehint:
        hint = "TypedKalmanLinearEstimatorPROXY"
    elif "itkJPEGImageIOFactory" in typehint:
        hint = "TypedJPEGImageIOFactoryPROXY"
    elif "itkJPEGImageIO" in typehint:
        hint = "TypedJPEGImageIOPROXY"
    elif "itkJPEG2000ImageIOInternalEnums" in typehint:
        hint = "TypedJPEG2000ImageIOInternalEnumsPROXY"
    elif "itkJPEG2000ImageIOFactory" in typehint:
        hint = "TypedJPEG2000ImageIOFactoryPROXY"
    elif "itkJPEG2000ImageIO" in typehint:
        hint = "TypedJPEG2000ImageIOPROXY"
    elif "itkJoinTraits" in typehint:
        hint = "TypedJoinTraitsPROXY"
    elif "itkJointHistogramMutualInformationImageToImageMetricv4" in typehint:
        hint = "TypedJointHistogramMutualInformationImageToImageMetricv4PROXY"
    elif "itkJointHistogramMutualInformationGetValueAndDerivativeThreader" in typehint:
        hint = "TypedJointHistogramMutualInformationGetValueAndDerivativeThreaderPROXY"
    elif "itkJointHistogramMutualInformationComputeJointPDFThreaderBase" in typehint:
        hint = "TypedJointHistogramMutualInformationComputeJointPDFThreaderBasePROXY"
    elif "itkJointHistogramMutualInformationComputeJointPDFThreader" in typehint:
        hint = "TypedJointHistogramMutualInformationComputeJointPDFThreaderPROXY"
    elif "itkJointHistogramMutualInformationComputeJointPDFThreader" in typehint:
        hint = "TypedJointHistogramMutualInformationComputeJointPDFThreaderPROXY"
    elif "itkJointHistogramMutualInformationComputeJointPDFThreader" in typehint:
        hint = "TypedJointHistogramMutualInformationComputeJointPDFThreaderPROXY"
    elif "itkJointHistogramMIPerThreadStruct" in typehint:
        hint = "TypedJointHistogramMIPerThreadStructPROXY"
    elif "itkJointHistogramMIPerThreadStruct" in typehint:
        hint = "TypedJointHistogramMIPerThreadStructPROXY"
    elif "itkJointDomainImageToListSampleAdaptor" in typehint:
        hint = "TypedJointDomainImageToListSampleAdaptorPROXY"
    elif "itkJoinSeriesImageFilter" in typehint:
        hint = "TypedJoinSeriesImageFilterPROXY"
    elif "itkJoinImageFilter" in typehint:
        hint = "TypedJoinImageFilterPROXY"
    elif "itkJoinFunctor" in typehint:
        hint = "TypedJoinFunctorPROXY"
    elif "itkJetColormapFunction" in typehint:
        hint = "TypedJetColormapFunctionPROXY"
    elif "itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4" in typehint:
        hint = "TypedJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4PROXY"
    elif "itkitkTransformBaseTemplate" in typehint:
        hint = "TypeditkTransformBaseTemplatePROXY"
    elif "itkitkFloatingPointExceptions" in typehint:
        hint = "TypeditkFloatingPointExceptionsPROXY"
    elif "itkitkBSplineCenteredL2ResampleImageFilterBase" in typehint:
        hint = "TypeditkBSplineCenteredL2ResampleImageFilterBasePROXY"
    elif "itkIteratorInitializer" in typehint:
        hint = "TypedIteratorInitializerPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterator" in typehint:
        hint = "TypedIteratorPROXY"
    elif "itkIterativeInverseDisplacementFieldImageFilter" in typehint:
        hint = "TypedIterativeInverseDisplacementFieldImageFilterPROXY"
    elif "itkIterativeDeconvolutionImageFilter" in typehint:
        hint = "TypedIterativeDeconvolutionImageFilterPROXY"
    elif "itkIterationReporter" in typehint:
        hint = "TypedIterationReporterPROXY"
    elif "itkIsUnsignedInteger" in typehint:
        hint = "TypedIsUnsignedIntegerPROXY"
    elif "itkIsSmartPointer" in typehint:
        hint = "TypedIsSmartPointerPROXY"
    elif "itkIsotropicFourthOrderLevelSetImageFilter" in typehint:
        hint = "TypedIsotropicFourthOrderLevelSetImageFilterPROXY"
    elif "itkIsolatedWatershedImageFilter" in typehint:
        hint = "TypedIsolatedWatershedImageFilterPROXY"
    elif "itkIsolatedConnectedImageFilter" in typehint:
        hint = "TypedIsolatedConnectedImageFilterPROXY"
    elif "itkIsoDataThresholdImageFilter" in typehint:
        hint = "TypedIsoDataThresholdImageFilterPROXY"
    elif "itkIsoDataThresholdCalculator" in typehint:
        hint = "TypedIsoDataThresholdCalculatorPROXY"
    elif "itkIsoContourDistanceImageFilter" in typehint:
        hint = "TypedIsoContourDistanceImageFilterPROXY"
    elif "itkIsNonInteger" in typehint:
        hint = "TypedIsNonIntegerPROXY"
    elif "itkIsInteger" in typehint:
        hint = "TypedIsIntegerPROXY"
    elif "itkIsFloatingPoint" in typehint:
        hint = "TypedIsFloatingPointPROXY"
    elif "itkIsFixedPoint" in typehint:
        hint = "TypedIsFixedPointPROXY"
    elif "itkIPLCommonImageIO" in typehint:
        hint = "TypedIPLCommonImageIOPROXY"
    elif "itkIOTestHelper" in typehint:
        hint = "TypedIOTestHelperPROXY"
    elif "itkIOPixel" in typehint:
        hint = "TypedIOPixelPROXY"
    elif "itkIOModeEnum" in typehint:
        hint = "TypedIOModeEnumPROXY"
    elif "itkIOFileMode" in typehint:
        hint = "TypedIOFileModePROXY"
    elif "itkIOFile" in typehint:
        hint = "TypedIOFilePROXY"
    elif "itkIOComponent" in typehint:
        hint = "TypedIOComponentPROXY"
    elif "itkIOCommonEnums" in typehint:
        hint = "TypedIOCommonEnumsPROXY"
    elif "itkIOCommon" in typehint:
        hint = "TypedIOCommonPROXY"
    elif "itkIOByteOrder" in typehint:
        hint = "TypedIOByteOrderPROXY"
    elif "itkInvertIntensityTransform" in typehint:
        hint = "TypedInvertIntensityTransformPROXY"
    elif "itkInvertIntensityImageFilter" in typehint:
        hint = "TypedInvertIntensityImageFilterPROXY"
    elif "itkInvertIntensityFunctor" in typehint:
        hint = "TypedInvertIntensityFunctorPROXY"
    elif "itkInvertDisplacementFieldImageFilter" in typehint:
        hint = "TypedInvertDisplacementFieldImageFilterPROXY"
    elif "itkInverseFFTImageFilter" in typehint:
        hint = "TypedInverseFFTImageFilterPROXY"
    elif "itkInverseEuclideanDistanceMatrixCoefficients" in typehint:
        hint = "TypedInverseEuclideanDistanceMatrixCoefficientsPROXY"
    elif "itkInverseDisplacementFieldImageFilter" in typehint:
        hint = "TypedInverseDisplacementFieldImageFilterPROXY"
    elif "itkInverseDeconvolutionImageFilter" in typehint:
        hint = "TypedInverseDeconvolutionImageFilterPROXY"
    elif "itkInverseDeconvolutionFunctor" in typehint:
        hint = "TypedInverseDeconvolutionFunctorPROXY"
    elif "itkInvalidRequestRegionError" in typehint:
        hint = "TypedInvalidRequestRegionErrorPROXY"
    elif "itkInvalidRequestedRegionError" in typehint:
        hint = "TypedInvalidRequestedRegionErrorPROXY"
    elif "itkIntrinsicMatrixCoefficients" in typehint:
        hint = "TypedIntrinsicMatrixCoefficientsPROXY"
    elif "itkInterpolationMode" in typehint:
        hint = "TypedInterpolationModePROXY"
    elif "itkInterpolateImagePointsFilter" in typehint:
        hint = "TypedInterpolateImagePointsFilterPROXY"
    elif "itkInterpolateImageFunction" in typehint:
        hint = "TypedInterpolateImageFunctionPROXY"
    elif "itkInterpolateImageFilter" in typehint:
        hint = "TypedInterpolateImageFilterPROXY"
    elif "itkIntermodesThresholdImageFilter" in typehint:
        hint = "TypedIntermodesThresholdImageFilterPROXY"
    elif "itkIntermodesThresholdCalculator" in typehint:
        hint = "TypedIntermodesThresholdCalculatorPROXY"
    elif "itkInteriorExteriorSpatialFunction" in typehint:
        hint = "TypedInteriorExteriorSpatialFunctionPROXY"
    elif "itkInteriorExteriorMeshFilter" in typehint:
        hint = "TypedInteriorExteriorMeshFilterPROXY"
    elif "itkInterface" in typehint:
        hint = "TypedInterfacePROXY"
    elif "itkIntensityWindowingTransform" in typehint:
        hint = "TypedIntensityWindowingTransformPROXY"
    elif "itkIntensityWindowingImageFilter" in typehint:
        hint = "TypedIntensityWindowingImageFilterPROXY"
    elif "itkIntensityLinearTransform" in typehint:
        hint = "TypedIntensityLinearTransformPROXY"
    elif "itkIntDispatch" in typehint:
        hint = "TypedIntDispatchPROXY"
    elif "itkInsertionPosition" in typehint:
        hint = "TypedInsertionPositionPROXY"
    elif "itkInputDataObjectIterator" in typehint:
        hint = "TypedInputDataObjectIteratorPROXY"
    elif "itkInputDataObjectConstIterator" in typehint:
        hint = "TypedInputDataObjectConstIteratorPROXY"
    elif "itkInPlaceLabelMapFilter" in typehint:
        hint = "TypedInPlaceLabelMapFilterPROXY"
    elif "itkInPlaceImageFilter" in typehint:
        hint = "TypedInPlaceImageFilterPROXY"
    elif "itkInitializationBiasedParticleSwarmOptimizer" in typehint:
        hint = "TypedInitializationBiasedParticleSwarmOptimizerPROXY"
    elif "itkInformationValueType" in typehint:
        hint = "TypedInformationValueTypePROXY"
    elif "itkIndexRange" in typehint:
        hint = "TypedIndexRangePROXY"
    elif "itkIndexedContainerInterface" in typehint:
        hint = "TypedIndexedContainerInterfacePROXY"
    elif "itkIndex" in typehint:
        hint = "TypedIndexPROXY"
    elif "itkIndent" in typehint:
        hint = "TypedIndentPROXY"
    elif "itkIncrementDecrementOperators" in typehint:
        hint = "TypedIncrementDecrementOperatorsPROXY"
    elif "itkImportImageFilter" in typehint:
        hint = "TypedImportImageFilterPROXY"
    elif "itkImportImageContainer" in typehint:
        hint = "TypedImportImageContainerPROXY"
    elif "itkImplicitManifoldNormalVectorFilter" in typehint:
        hint = "TypedImplicitManifoldNormalVectorFilterPROXY"
    elif "itkImageVoxel" in typehint:
        hint = "TypedImageVoxelPROXY"
    elif "itkImageVectorOptimizerParametersHelper" in typehint:
        hint = "TypedImageVectorOptimizerParametersHelperPROXY"
    elif "itkImageToVTKImageFilter" in typehint:
        hint = "TypedImageToVTKImageFilterPROXY"
    elif "itkImageToSpatialObjectRegistrationMethod" in typehint:
        hint = "TypedImageToSpatialObjectRegistrationMethodPROXY"
    elif "itkImageToSpatialObjectMetric" in typehint:
        hint = "TypedImageToSpatialObjectMetricPROXY"
    elif "itkImageToPathFilter" in typehint:
        hint = "TypedImageToPathFilterPROXY"
    elif "itkImageToParametricSpaceFilter" in typehint:
        hint = "TypedImageToParametricSpaceFilterPROXY"
    elif "itkImageToNeighborhoodSampleAdaptor" in typehint:
        hint = "TypedImageToNeighborhoodSampleAdaptorPROXY"
    elif "itkImageToMeshFilter" in typehint:
        hint = "TypedImageToMeshFilterPROXY"
    elif "itkImageToListSampleFilter" in typehint:
        hint = "TypedImageToListSampleFilterPROXY"
    elif "itkImageToListSampleAdaptor" in typehint:
        hint = "TypedImageToListSampleAdaptorPROXY"
    elif "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase" in typehint:
        hint = "TypedImageToImageMetricv4GetValueAndDerivativeThreaderBasePROXY"
    elif "itkImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkImageToImageMetricv4" in typehint:
        hint = "TypedImageToImageMetricv4PROXY"
    elif "itkImageToImageMetric" in typehint:
        hint = "TypedImageToImageMetricPROXY"
    elif "itkImageToImageFilterCommon" in typehint:
        hint = "TypedImageToImageFilterCommonPROXY"
    elif "itkImageToImageFilter" in typehint:
        hint = "TypedImageToImageFilterPROXY"
    elif "itkImageToHistogramFilter" in typehint:
        hint = "TypedImageToHistogramFilterPROXY"
    elif "itkImageSpatialObject" in typehint:
        hint = "TypedImageSpatialObjectPROXY"
    elif "itkImageSourceCommon" in typehint:
        hint = "TypedImageSourceCommonPROXY"
    elif "itkImageSource" in typehint:
        hint = "TypedImageSourcePROXY"
    elif "itkImageSliceIteratorWithIndex" in typehint:
        hint = "TypedImageSliceIteratorWithIndexPROXY"
    elif "itkImageSliceConstIteratorWithIndex" in typehint:
        hint = "TypedImageSliceConstIteratorWithIndexPROXY"
    elif "itkImageSink" in typehint:
        hint = "TypedImageSinkPROXY"
    elif "itkImageShapeModelEstimatorBase" in typehint:
        hint = "TypedImageShapeModelEstimatorBasePROXY"
    elif "itkImageSeriesWriterException" in typehint:
        hint = "TypedImageSeriesWriterExceptionPROXY"
    elif "itkImageSeriesWriter" in typehint:
        hint = "TypedImageSeriesWriterPROXY"
    elif "itkImageSeriesReader" in typehint:
        hint = "TypedImageSeriesReaderPROXY"
    elif "itkImageScanlineIterator" in typehint:
        hint = "TypedImageScanlineIteratorPROXY"
    elif "itkImageScanlineConstIterator" in typehint:
        hint = "TypedImageScanlineConstIteratorPROXY"
    elif "itkImageReverseIterator" in typehint:
        hint = "TypedImageReverseIteratorPROXY"
    elif "itkImageReverseConstIterator" in typehint:
        hint = "TypedImageReverseConstIteratorPROXY"
    elif "itkImageRegistrationMethodv4Enums" in typehint:
        hint = "TypedImageRegistrationMethodv4EnumsPROXY"
    elif "itkImageRegistrationMethodv4" in typehint:
        hint = "TypedImageRegistrationMethodv4PROXY"
    elif "itkImageRegistrationMethodImageSource" in typehint:
        hint = "TypedImageRegistrationMethodImageSourcePROXY"
    elif "itkImageRegistrationMethod" in typehint:
        hint = "TypedImageRegistrationMethodPROXY"
    elif "itkImageRegionSplitterSlowDimension" in typehint:
        hint = "TypedImageRegionSplitterSlowDimensionPROXY"
    elif "itkImageRegionSplitterMultidimensional" in typehint:
        hint = "TypedImageRegionSplitterMultidimensionalPROXY"
    elif "itkImageRegionSplitterDirection" in typehint:
        hint = "TypedImageRegionSplitterDirectionPROXY"
    elif "itkImageRegionSplitterBase" in typehint:
        hint = "TypedImageRegionSplitterBasePROXY"
    elif "itkImageRegionReverseIterator" in typehint:
        hint = "TypedImageRegionReverseIteratorPROXY"
    elif "itkImageRegionReverseConstIterator" in typehint:
        hint = "TypedImageRegionReverseConstIteratorPROXY"
    elif "itkImageRegionRange" in typehint:
        hint = "TypedImageRegionRangePROXY"
    elif "itkImageRegionIteratorWithIndex" in typehint:
        hint = "TypedImageRegionIteratorWithIndexPROXY"
    elif "itkImageRegionIterator" in typehint:
        hint = "TypedImageRegionIteratorPROXY"
    elif "itkImageRegionExclusionIteratorWithIndex" in typehint:
        hint = "TypedImageRegionExclusionIteratorWithIndexPROXY"
    elif "itkImageRegionExclusionConstIteratorWithIndex" in typehint:
        hint = "TypedImageRegionExclusionConstIteratorWithIndexPROXY"
    elif "itkImageRegionCopier" in typehint:
        hint = "TypedImageRegionCopierPROXY"
    elif "itkImageRegionConstIteratorWithOnlyIndex" in typehint:
        hint = "TypedImageRegionConstIteratorWithOnlyIndexPROXY"
    elif "itkImageRegionConstIteratorWithIndex" in typehint:
        hint = "TypedImageRegionConstIteratorWithIndexPROXY"
    elif "itkImageRegionConstIterator" in typehint:
        hint = "TypedImageRegionConstIteratorPROXY"
    elif "itkImageRegion" in typehint:
        hint = "TypedImageRegionPROXY"
    elif "itkImageRandomNonRepeatingIteratorWithIndex" in typehint:
        hint = "TypedImageRandomNonRepeatingIteratorWithIndexPROXY"
    elif "itkImageRandomNonRepeatingConstIteratorWithIndex" in typehint:
        hint = "TypedImageRandomNonRepeatingConstIteratorWithIndexPROXY"
    elif "itkImageRandomIteratorWithIndex" in typehint:
        hint = "TypedImageRandomIteratorWithIndexPROXY"
    elif "itkImageRandomConstIteratorWithOnlyIndex" in typehint:
        hint = "TypedImageRandomConstIteratorWithOnlyIndexPROXY"
    elif "itkImageRandomConstIteratorWithIndex" in typehint:
        hint = "TypedImageRandomConstIteratorWithIndexPROXY"
    elif "itkImagePCAShapeModelEstimator" in typehint:
        hint = "TypedImagePCAShapeModelEstimatorPROXY"
    elif "itkImagePCADecompositionCalculator" in typehint:
        hint = "TypedImagePCADecompositionCalculatorPROXY"
    elif "itkImageMomentsCalculator" in typehint:
        hint = "TypedImageMomentsCalculatorPROXY"
    elif "itkImageModelEstimatorBase" in typehint:
        hint = "TypedImageModelEstimatorBasePROXY"
    elif "itkImageMaskSpatialObject" in typehint:
        hint = "TypedImageMaskSpatialObjectPROXY"
    elif "itkImageLinearIteratorWithIndex" in typehint:
        hint = "TypedImageLinearIteratorWithIndexPROXY"
    elif "itkImageLinearConstIteratorWithIndex" in typehint:
        hint = "TypedImageLinearConstIteratorWithIndexPROXY"
    elif "itkImageKmeansModelEstimator" in typehint:
        hint = "TypedImageKmeansModelEstimatorPROXY"
    elif "itkImageKernelOperator" in typehint:
        hint = "TypedImageKernelOperatorPROXY"
    elif "itkImageJointDomainTraits" in typehint:
        hint = "TypedImageJointDomainTraitsPROXY"
    elif "itkImageIteratorWithIndex" in typehint:
        hint = "TypedImageIteratorWithIndexPROXY"
    elif "itkImageIterator" in typehint:
        hint = "TypedImageIteratorPROXY"
    elif "itkImageIORegionAdaptor" in typehint:
        hint = "TypedImageIORegionAdaptorPROXY"
    elif "itkImageIORegion" in typehint:
        hint = "TypedImageIORegionPROXY"
    elif "itkImageIOFactory" in typehint:
        hint = "TypedImageIOFactoryPROXY"
    elif "itkImageIOBase" in typehint:
        hint = "TypedImageIOBasePROXY"
    elif "itkImageInformationCopier" in typehint:
        hint = "TypedImageInformationCopierPROXY"
    elif "itkImageInfo" in typehint:
        hint = "TypedImageInfoPROXY"
    elif "itkImageHelper" in typehint:
        hint = "TypedImageHelperPROXY"
    elif "itkImageGaussianModelEstimator" in typehint:
        hint = "TypedImageGaussianModelEstimatorPROXY"
    elif "itkImageFunction" in typehint:
        hint = "TypedImageFunctionPROXY"
    elif "itkImageFilterToVideoFilterWrapper" in typehint:
        hint = "TypedImageFilterToVideoFilterWrapperPROXY"
    elif "itkImageFileWriterException" in typehint:
        hint = "TypedImageFileWriterExceptionPROXY"
    elif "itkImageFileWriter" in typehint:
        hint = "TypedImageFileWriterPROXY"
    elif "itkImageFileReader" in typehint:
        hint = "TypedImageFileReaderPROXY"
    elif "itkImageDuplicator" in typehint:
        hint = "TypedImageDuplicatorPROXY"
    elif "itkImageContainerInterface" in typehint:
        hint = "TypedImageContainerInterfacePROXY"
    elif "itkImageConstIteratorWithOnlyIndex" in typehint:
        hint = "TypedImageConstIteratorWithOnlyIndexPROXY"
    elif "itkImageConstIteratorWithIndex" in typehint:
        hint = "TypedImageConstIteratorWithIndexPROXY"
    elif "itkImageConstIterator" in typehint:
        hint = "TypedImageConstIteratorPROXY"
    elif "itkImageClassifierFilter" in typehint:
        hint = "TypedImageClassifierFilterPROXY"
    elif "itkImageClassifierBase" in typehint:
        hint = "TypedImageClassifierBasePROXY"
    elif "itkImageBufferRange" in typehint:
        hint = "TypedImageBufferRangePROXY"
    elif "itkImageBoundaryFacesCalculator" in typehint:
        hint = "TypedImageBoundaryFacesCalculatorPROXY"
    elif "itkImageBoundaryCondition" in typehint:
        hint = "TypedImageBoundaryConditionPROXY"
    elif "itkImageBase" in typehint:
        hint = "TypedImageBasePROXY"
    elif "itkImageAndPathToImageFilter" in typehint:
        hint = "TypedImageAndPathToImageFilterPROXY"
    elif "itkImageAlgorithm" in typehint:
        hint = "TypedImageAlgorithmPROXY"
    elif "itkImageAdaptor" in typehint:
        hint = "TypedImageAdaptorPROXY"
    elif "itkImage" in typehint:
        hint = "TypedImagePROXY"
    elif "itkIdentityTransform" in typehint:
        hint = "TypedIdentityTransformPROXY"
    elif "itkIdentityHelper" in typehint:
        hint = "TypedIdentityHelperPROXY"
    elif "itkIdentifierArrayHashFunction" in typehint:
        hint = "TypedIdentifierArrayHashFunctionPROXY"
    elif "itkIdentifierArrayEqualsFunction" in typehint:
        hint = "TypedIdentifierArrayEqualsFunctionPROXY"
    elif "itkHuangThresholdImageFilter" in typehint:
        hint = "TypedHuangThresholdImageFilterPROXY"
    elif "itkHuangThresholdCalculator" in typehint:
        hint = "TypedHuangThresholdCalculatorPROXY"
    elif "itkHSVColormapFunction" in typehint:
        hint = "TypedHSVColormapFunctionPROXY"
    elif "itkHoughTransform2DLinesImageFilter" in typehint:
        hint = "TypedHoughTransform2DLinesImageFilterPROXY"
    elif "itkHoughTransform2DCirclesImageFilter" in typehint:
        hint = "TypedHoughTransform2DCirclesImageFilterPROXY"
    elif "itkHotColormapFunction" in typehint:
        hint = "TypedHotColormapFunctionPROXY"
    elif "itkHostnameWisdomFilenameGenerator" in typehint:
        hint = "TypedHostnameWisdomFilenameGeneratorPROXY"
    elif "itkHMinimaImageFilter" in typehint:
        hint = "TypedHMinimaImageFilterPROXY"
    elif "itkHMaximaImageFilter" in typehint:
        hint = "TypedHMaximaImageFilterPROXY"
    elif "itkHistogramWrongNumberOfComponents" in typehint:
        hint = "TypedHistogramWrongNumberOfComponentsPROXY"
    elif "itkHistogramToTextureFeaturesFilterEnums" in typehint:
        hint = "TypedHistogramToTextureFeaturesFilterEnumsPROXY"
    elif "itkHistogramToTextureFeaturesFilter" in typehint:
        hint = "TypedHistogramToTextureFeaturesFilterPROXY"
    elif "itkHistogramToRunLengthFeaturesFilterEnums" in typehint:
        hint = "TypedHistogramToRunLengthFeaturesFilterEnumsPROXY"
    elif "itkHistogramToRunLengthFeaturesFilter" in typehint:
        hint = "TypedHistogramToRunLengthFeaturesFilterPROXY"
    elif "itkHistogramToProbabilityImageFilter" in typehint:
        hint = "TypedHistogramToProbabilityImageFilterPROXY"
    elif "itkHistogramToLogProbabilityImageFilter" in typehint:
        hint = "TypedHistogramToLogProbabilityImageFilterPROXY"
    elif "itkHistogramToIntensityImageFilter" in typehint:
        hint = "TypedHistogramToIntensityImageFilterPROXY"
    elif "itkHistogramToImageFilter" in typehint:
        hint = "TypedHistogramToImageFilterPROXY"
    elif "itkHistogramToEntropyImageFilter" in typehint:
        hint = "TypedHistogramToEntropyImageFilterPROXY"
    elif "itkHistogramThresholdImageFilter" in typehint:
        hint = "TypedHistogramThresholdImageFilterPROXY"
    elif "itkHistogramThresholdCalculator" in typehint:
        hint = "TypedHistogramThresholdCalculatorPROXY"
    elif "itkHistogramProbabilityFunction" in typehint:
        hint = "TypedHistogramProbabilityFunctionPROXY"
    elif "itkHistogramMatchingImageFilter" in typehint:
        hint = "TypedHistogramMatchingImageFilterPROXY"
    elif "itkHistogramLogProbabilityFunction" in typehint:
        hint = "TypedHistogramLogProbabilityFunctionPROXY"
    elif "itkHistogramLabelObjectAccessor" in typehint:
        hint = "TypedHistogramLabelObjectAccessorPROXY"
    elif "itkHistogramIntensityFunction" in typehint:
        hint = "TypedHistogramIntensityFunctionPROXY"
    elif "itkHistogramImageToImageMetric" in typehint:
        hint = "TypedHistogramImageToImageMetricPROXY"
    elif "itkHistogramEntropyFunction" in typehint:
        hint = "TypedHistogramEntropyFunctionPROXY"
    elif "itkHistogramAlgorithmBase" in typehint:
        hint = "TypedHistogramAlgorithmBasePROXY"
    elif "itkHistogram" in typehint:
        hint = "TypedHistogramPROXY"
    elif "itkHilbertPath" in typehint:
        hint = "TypedHilbertPathPROXY"
    elif "itkHexahedronCellTopology" in typehint:
        hint = "TypedHexahedronCellTopologyPROXY"
    elif "itkHexahedronCell" in typehint:
        hint = "TypedHexahedronCellPROXY"
    elif "itkHessianToObjectnessMeasureImageFilter" in typehint:
        hint = "TypedHessianToObjectnessMeasureImageFilterPROXY"
    elif "itkHessianRecursiveGaussianImageFilter" in typehint:
        hint = "TypedHessianRecursiveGaussianImageFilterPROXY"
    elif "itkHessian3DToVesselnessMeasureImageFilter" in typehint:
        hint = "TypedHessian3DToVesselnessMeasureImageFilterPROXY"
    elif "itkHeavisideStepFunctionBase" in typehint:
        hint = "TypedHeavisideStepFunctionBasePROXY"
    elif "itkHeavisideStepFunction" in typehint:
        hint = "TypedHeavisideStepFunctionPROXY"
    elif "itkHeader" in typehint:
        hint = "TypedHeaderPROXY"
    elif "itkHDF5TransformIOTemplate" in typehint:
        hint = "TypedHDF5TransformIOTemplatePROXY"
    elif "itkHDF5TransformIOFactory" in typehint:
        hint = "TypedHDF5TransformIOFactoryPROXY"
    elif "itkHDF5ImageIOFactory" in typehint:
        hint = "TypedHDF5ImageIOFactoryPROXY"
    elif "itkHDF5ImageIO" in typehint:
        hint = "TypedHDF5ImageIOPROXY"
    elif "itkHDF5CommonPathNames" in typehint:
        hint = "TypedHDF5CommonPathNamesPROXY"
    elif "itkHConvexImageFilter" in typehint:
        hint = "TypedHConvexImageFilterPROXY"
    elif "itkHConcaveImageFilter" in typehint:
        hint = "TypedHConcaveImageFilterPROXY"
    elif "itkHausdorffDistanceImageFilter" in typehint:
        hint = "TypedHausdorffDistanceImageFilterPROXY"
    elif "itkHasZero" in typehint:
        hint = "TypedHasZeroPROXY"
    elif "itkHasValueType" in typehint:
        hint = "TypedHasValueTypePROXY"
    elif "itkHasPixelTraits" in typehint:
        hint = "TypedHasPixelTraitsPROXY"
    elif "itkHasNumericTraits" in typehint:
        hint = "TypedHasNumericTraitsPROXY"
    elif "itkHasJoinTraits" in typehint:
        hint = "TypedHasJoinTraitsPROXY"
    elif "itkHashOutputQEPrimal" in typehint:
        hint = "TypedHashOutputQEPrimalPROXY"
    elif "itkHashImageFilterEnums" in typehint:
        hint = "TypedHashImageFilterEnumsPROXY"
    elif "itkHashImageFilter" in typehint:
        hint = "TypedHashImageFilterPROXY"
    elif "itkHashFunction" in typehint:
        hint = "TypedHashFunctionPROXY"
    elif "itkHarmonicMatrixCoefficients" in typehint:
        hint = "TypedHarmonicMatrixCoefficientsPROXY"
    elif "itkHardwareWisdomFilenameGenerator" in typehint:
        hint = "TypedHardwareWisdomFilenameGeneratorPROXY"
    elif "itkHardConnectedComponentImageFilter" in typehint:
        hint = "TypedHardConnectedComponentImageFilterPROXY"
    elif "itkHandleRGBPixel" in typehint:
        hint = "TypedHandleRGBPixelPROXY"
    elif "itkHandleRGBPixel" in typehint:
        hint = "TypedHandleRGBPixelPROXY"
    elif "itkHammingWindowFunction" in typehint:
        hint = "TypedHammingWindowFunctionPROXY"
    elif "itkHalfToFullHermitianImageFilter" in typehint:
        hint = "TypedHalfToFullHermitianImageFilterPROXY"
    elif "itkHalfHermitianToRealInverseFFTImageFilter" in typehint:
        hint = "TypedHalfHermitianToRealInverseFFTImageFilterPROXY"
    elif "itkGrowthStrategy" in typehint:
        hint = "TypedGrowthStrategyPROXY"
    elif "itkGroupSpatialObject" in typehint:
        hint = "TypedGroupSpatialObjectPROXY"
    elif "itkGridImageSource" in typehint:
        hint = "TypedGridImageSourcePROXY"
    elif "itkGridForwardWarpImageFilter" in typehint:
        hint = "TypedGridForwardWarpImageFilterPROXY"
    elif "itkGreyColormapFunction" in typehint:
        hint = "TypedGreyColormapFunctionPROXY"
    elif "itkGreyAndPos" in typehint:
        hint = "TypedGreyAndPosPROXY"
    elif "itkGreenPixelAccessor" in typehint:
        hint = "TypedGreenPixelAccessorPROXY"
    elif "itkGreenColormapFunction" in typehint:
        hint = "TypedGreenColormapFunctionPROXY"
    elif "itkGreaterThanComparable" in typehint:
        hint = "TypedGreaterThanComparablePROXY"
    elif "itkGreaterEqual" in typehint:
        hint = "TypedGreaterEqualPROXY"
    elif "itkGreater" in typehint:
        hint = "TypedGreaterPROXY"
    elif "itkGrayscaleMorphologicalOpeningImageFilter" in typehint:
        hint = "TypedGrayscaleMorphologicalOpeningImageFilterPROXY"
    elif "itkGrayscaleMorphologicalClosingImageFilter" in typehint:
        hint = "TypedGrayscaleMorphologicalClosingImageFilterPROXY"
    elif "itkGrayscaleGrindPeakImageFilter" in typehint:
        hint = "TypedGrayscaleGrindPeakImageFilterPROXY"
    elif "itkGrayscaleGeodesicErodeImageFilter" in typehint:
        hint = "TypedGrayscaleGeodesicErodeImageFilterPROXY"
    elif "itkGrayscaleGeodesicDilateImageFilter" in typehint:
        hint = "TypedGrayscaleGeodesicDilateImageFilterPROXY"
    elif "itkGrayscaleFunctionErodeImageFilter" in typehint:
        hint = "TypedGrayscaleFunctionErodeImageFilterPROXY"
    elif "itkGrayscaleFunctionDilateImageFilter" in typehint:
        hint = "TypedGrayscaleFunctionDilateImageFilterPROXY"
    elif "itkGrayscaleFillholeImageFilter" in typehint:
        hint = "TypedGrayscaleFillholeImageFilterPROXY"
    elif "itkGrayscaleErodeImageFilter" in typehint:
        hint = "TypedGrayscaleErodeImageFilterPROXY"
    elif "itkGrayscaleDilateImageFilter" in typehint:
        hint = "TypedGrayscaleDilateImageFilterPROXY"
    elif "itkGrayscaleConnectedOpeningImageFilter" in typehint:
        hint = "TypedGrayscaleConnectedOpeningImageFilterPROXY"
    elif "itkGrayscaleConnectedClosingImageFilter" in typehint:
        hint = "TypedGrayscaleConnectedClosingImageFilterPROXY"
    elif "itkGradientVectorFlowImageFilter" in typehint:
        hint = "TypedGradientVectorFlowImageFilterPROXY"
    elif "itkGradientSource" in typehint:
        hint = "TypedGradientSourcePROXY"
    elif "itkGradientRecursiveGaussianImageFilter" in typehint:
        hint = "TypedGradientRecursiveGaussianImageFilterPROXY"
    elif "itkGradientNDAnisotropicDiffusionFunction" in typehint:
        hint = "TypedGradientNDAnisotropicDiffusionFunctionPROXY"
    elif "itkGradientMagnitudeRecursiveGaussianImageFilter" in typehint:
        hint = "TypedGradientMagnitudeRecursiveGaussianImageFilterPROXY"
    elif "itkGradientMagnitudeImageFilter" in typehint:
        hint = "TypedGradientMagnitudeImageFilterPROXY"
    elif "itkGradientImageFormat" in typehint:
        hint = "TypedGradientImageFormatPROXY"
    elif "itkGradientImageFilter" in typehint:
        hint = "TypedGradientImageFilterPROXY"
    elif "itkGradientEnum" in typehint:
        hint = "TypedGradientEnumPROXY"
    elif "itkGradientDifferenceImageToImageMetric" in typehint:
        hint = "TypedGradientDifferenceImageToImageMetricPROXY"
    elif "itkGradientDescentOptimizerv4Template" in typehint:
        hint = "TypedGradientDescentOptimizerv4TemplatePROXY"
    elif "itkGradientDescentOptimizerEnums" in typehint:
        hint = "TypedGradientDescentOptimizerEnumsPROXY"
    elif "itkGradientDescentOptimizerBasev4Template" in typehint:
        hint = "TypedGradientDescentOptimizerBasev4TemplatePROXY"
    elif "itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate" in typehint:
        hint = "TypedGradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplatePROXY"
    elif "itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate" in typehint:
        hint = "TypedGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplatePROXY"
    elif "itkGradientDescentOptimizerBasev4" in typehint:
        hint = "TypedGradientDescentOptimizerBasev4PROXY"
    elif "itkGradientDescentOptimizer" in typehint:
        hint = "TypedGradientDescentOptimizerPROXY"
    elif "itkGradientDescentLineSearchOptimizerv4Template" in typehint:
        hint = "TypedGradientDescentLineSearchOptimizerv4TemplatePROXY"
    elif "itkGradientAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedGradientAnisotropicDiffusionImageFilterPROXY"
    elif "itkGPUUnaryFunctorImageFilter" in typehint:
        hint = "TypedGPUUnaryFunctorImageFilterPROXY"
    elif "itkGPUTraits" in typehint:
        hint = "TypedGPUTraitsPROXY"
    elif "itkGPUTraits" in typehint:
        hint = "TypedGPUTraitsPROXY"
    elif "itkGPUScalarAnisotropicDiffusionFunction" in typehint:
        hint = "TypedGPUScalarAnisotropicDiffusionFunctionPROXY"
    elif "itkGPUReduction" in typehint:
        hint = "TypedGPUReductionPROXY"
    elif "itkGPUPDEDeformableRegistrationFunction" in typehint:
        hint = "TypedGPUPDEDeformableRegistrationFunctionPROXY"
    elif "itkGPUPDEDeformableRegistrationFilter" in typehint:
        hint = "TypedGPUPDEDeformableRegistrationFilterPROXY"
    elif "itkGPUNeighborhoodOperatorImageFilter" in typehint:
        hint = "TypedGPUNeighborhoodOperatorImageFilterPROXY"
    elif "itkGPUMeanImageFilterFactory" in typehint:
        hint = "TypedGPUMeanImageFilterFactoryPROXY"
    elif "itkGPUMeanImageFilter" in typehint:
        hint = "TypedGPUMeanImageFilterPROXY"
    elif "itkGPUKernelManager" in typehint:
        hint = "TypedGPUKernelManagerPROXY"
    elif "itkGPUInPlaceImageFilter" in typehint:
        hint = "TypedGPUInPlaceImageFilterPROXY"
    elif "itkGPUImageToImageFilter" in typehint:
        hint = "TypedGPUImageToImageFilterPROXY"
    elif "itkGPUImageOps" in typehint:
        hint = "TypedGPUImageOpsPROXY"
    elif "itkGPUImageFactory" in typehint:
        hint = "TypedGPUImageFactoryPROXY"
    elif "itkGPUImageDataManager" in typehint:
        hint = "TypedGPUImageDataManagerPROXY"
    elif "itkGPUImage" in typehint:
        hint = "TypedGPUImagePROXY"
    elif "itkGPUGradientNDAnisotropicDiffusionFunction" in typehint:
        hint = "TypedGPUGradientNDAnisotropicDiffusionFunctionPROXY"
    elif "itkGPUGradientAnisotropicDiffusionImageFilterFactory" in typehint:
        hint = "TypedGPUGradientAnisotropicDiffusionImageFilterFactoryPROXY"
    elif "itkGPUGradientAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedGPUGradientAnisotropicDiffusionImageFilterPROXY"
    elif "itkGPUFunctorBase" in typehint:
        hint = "TypedGPUFunctorBasePROXY"
    elif "itkGPUFiniteDifferenceImageFilter" in typehint:
        hint = "TypedGPUFiniteDifferenceImageFilterPROXY"
    elif "itkGPUFiniteDifferenceFunction" in typehint:
        hint = "TypedGPUFiniteDifferenceFunctionPROXY"
    elif "itkGPUFiniteDifferenceFilterEnum" in typehint:
        hint = "TypedGPUFiniteDifferenceFilterEnumPROXY"
    elif "itkGPUDiscreteGaussianImageFilter" in typehint:
        hint = "TypedGPUDiscreteGaussianImageFilterPROXY"
    elif "itkGPUDenseFiniteDifferenceImageFilter" in typehint:
        hint = "TypedGPUDenseFiniteDifferenceImageFilterPROXY"
    elif "itkGPUDemonsRegistrationFunction" in typehint:
        hint = "TypedGPUDemonsRegistrationFunctionPROXY"
    elif "itkGPUDemonsRegistrationFilterFactory" in typehint:
        hint = "TypedGPUDemonsRegistrationFilterFactoryPROXY"
    elif "itkGPUDemonsRegistrationFilter" in typehint:
        hint = "TypedGPUDemonsRegistrationFilterPROXY"
    elif "itkGPUDataManager" in typehint:
        hint = "TypedGPUDataManagerPROXY"
    elif "itkGPUContextManager" in typehint:
        hint = "TypedGPUContextManagerPROXY"
    elif "itkGPUCastImageFilter" in typehint:
        hint = "TypedGPUCastImageFilterPROXY"
    elif "itkGPUCast" in typehint:
        hint = "TypedGPUCastPROXY"
    elif "itkGPUBoxImageFilter" in typehint:
        hint = "TypedGPUBoxImageFilterPROXY"
    elif "itkGPUBinaryThresholdImageFilterFactory" in typehint:
        hint = "TypedGPUBinaryThresholdImageFilterFactoryPROXY"
    elif "itkGPUBinaryThresholdImageFilter" in typehint:
        hint = "TypedGPUBinaryThresholdImageFilterPROXY"
    elif "itkGPUBinaryThreshold" in typehint:
        hint = "TypedGPUBinaryThresholdPROXY"
    elif "itkGPUAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedGPUAnisotropicDiffusionImageFilterPROXY"
    elif "itkGPUAnisotropicDiffusionFunction" in typehint:
        hint = "TypedGPUAnisotropicDiffusionFunctionPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGlobalDataStruct" in typehint:
        hint = "TypedGlobalDataStructPROXY"
    elif "itkGiplImageIOFactory" in typehint:
        hint = "TypedGiplImageIOFactoryPROXY"
    elif "itkGiplImageIO" in typehint:
        hint = "TypedGiplImageIOPROXY"
    elif "itkGiftiMeshIOFactory" in typehint:
        hint = "TypedGiftiMeshIOFactoryPROXY"
    elif "itkGiftiMeshIO" in typehint:
        hint = "TypedGiftiMeshIOPROXY"
    elif "itkGetValueAndDerivativePerThreadStruct" in typehint:
        hint = "TypedGetValueAndDerivativePerThreadStructPROXY"
    elif "itkGetAverageSliceImageFilter" in typehint:
        hint = "TypedGetAverageSliceImageFilterPROXY"
    elif "itkGeometryUtilities" in typehint:
        hint = "TypedGeometryUtilitiesPROXY"
    elif "itkGeometricalQuadEdge" in typehint:
        hint = "TypedGeometricalQuadEdgePROXY"
    elif "itkGeodesicActiveContourShapePriorLevelSetImageFilter" in typehint:
        hint = "TypedGeodesicActiveContourShapePriorLevelSetImageFilterPROXY"
    elif "itkGeodesicActiveContourShapePriorLevelSetFunction" in typehint:
        hint = "TypedGeodesicActiveContourShapePriorLevelSetFunctionPROXY"
    elif "itkGeodesicActiveContourLevelSetImageFilter" in typehint:
        hint = "TypedGeodesicActiveContourLevelSetImageFilterPROXY"
    elif "itkGeodesicActiveContourLevelSetFunction" in typehint:
        hint = "TypedGeodesicActiveContourLevelSetFunctionPROXY"
    elif "itkGenerateImageSource" in typehint:
        hint = "TypedGenerateImageSourcePROXY"
    elif "itkGEImageHeader" in typehint:
        hint = "TypedGEImageHeaderPROXY"
    elif "itkGEAdwImageIOFactory" in typehint:
        hint = "TypedGEAdwImageIOFactoryPROXY"
    elif "itkGEAdwImageIO" in typehint:
        hint = "TypedGEAdwImageIOPROXY"
    elif "itkGE5ImageIOFactory" in typehint:
        hint = "TypedGE5ImageIOFactoryPROXY"
    elif "itkGE5ImageIO" in typehint:
        hint = "TypedGE5ImageIOPROXY"
    elif "itkGE4ImageIOFactory" in typehint:
        hint = "TypedGE4ImageIOFactoryPROXY"
    elif "itkGE4ImageIO" in typehint:
        hint = "TypedGE4ImageIOPROXY"
    elif "itkGDCMSeriesFileNames" in typehint:
        hint = "TypedGDCMSeriesFileNamesPROXY"
    elif "itkGDCMImageIOFactory" in typehint:
        hint = "TypedGDCMImageIOFactoryPROXY"
    elif "itkGDCMImageIOEnums" in typehint:
        hint = "TypedGDCMImageIOEnumsPROXY"
    elif "itkGDCMImageIO" in typehint:
        hint = "TypedGDCMImageIOPROXY"
    elif "itkGaussianSpatialObject" in typehint:
        hint = "TypedGaussianSpatialObjectPROXY"
    elif "itkGaussianSpatialFunction" in typehint:
        hint = "TypedGaussianSpatialFunctionPROXY"
    elif "itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform" in typehint:
        hint = "TypedGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransformPROXY"
    elif "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor" in typehint:
        hint = "TypedGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptorPROXY"
    elif "itkGaussianSmoothingOnUpdateDisplacementFieldTransform" in typehint:
        hint = "TypedGaussianSmoothingOnUpdateDisplacementFieldTransformPROXY"
    elif "itkGaussianRandomSpatialNeighborSubsampler" in typehint:
        hint = "TypedGaussianRandomSpatialNeighborSubsamplerPROXY"
    elif "itkGaussianOrder" in typehint:
        hint = "TypedGaussianOrderPROXY"
    elif "itkGaussianOperator" in typehint:
        hint = "TypedGaussianOperatorPROXY"
    elif "itkGaussianMixtureModelComponent" in typehint:
        hint = "TypedGaussianMixtureModelComponentPROXY"
    elif "itkGaussianMembershipFunction" in typehint:
        hint = "TypedGaussianMembershipFunctionPROXY"
    elif "itkGaussianKernelFunction" in typehint:
        hint = "TypedGaussianKernelFunctionPROXY"
    elif "itkGaussianInterpolateImageFunction" in typehint:
        hint = "TypedGaussianInterpolateImageFunctionPROXY"
    elif "itkGaussianImageSource" in typehint:
        hint = "TypedGaussianImageSourcePROXY"
    elif "itkGaussianExponentialDiffeomorphicTransformParametersAdaptor" in typehint:
        hint = "TypedGaussianExponentialDiffeomorphicTransformParametersAdaptorPROXY"
    elif "itkGaussianExponentialDiffeomorphicTransform" in typehint:
        hint = "TypedGaussianExponentialDiffeomorphicTransformPROXY"
    elif "itkGaussianDistribution" in typehint:
        hint = "TypedGaussianDistributionPROXY"
    elif "itkGaussianDerivativeSpatialFunction" in typehint:
        hint = "TypedGaussianDerivativeSpatialFunctionPROXY"
    elif "itkGaussianDerivativeOperatorEnums" in typehint:
        hint = "TypedGaussianDerivativeOperatorEnumsPROXY"
    elif "itkGaussianDerivativeOperator" in typehint:
        hint = "TypedGaussianDerivativeOperatorPROXY"
    elif "itkGaussianDerivativeImageFunction" in typehint:
        hint = "TypedGaussianDerivativeImageFunctionPROXY"
    elif "itkGaussianBlurImageFunction" in typehint:
        hint = "TypedGaussianBlurImageFunctionPROXY"
    elif "itkGaborKernelFunction" in typehint:
        hint = "TypedGaborKernelFunctionPROXY"
    elif "itkGaborImageSource" in typehint:
        hint = "TypedGaborImageSourcePROXY"
    elif "itkFunctionCommand" in typehint:
        hint = "TypedFunctionCommandPROXY"
    elif "itkFunctionBase" in typehint:
        hint = "TypedFunctionBasePROXY"
    elif "itkFullToHalfHermitianImageFilter" in typehint:
        hint = "TypedFullToHalfHermitianImageFilterPROXY"
    elif "itkFrustumSpatialFunctionEnums" in typehint:
        hint = "TypedFrustumSpatialFunctionEnumsPROXY"
    elif "itkFrustumSpatialFunction" in typehint:
        hint = "TypedFrustumSpatialFunctionPROXY"
    elif "itkFRPROptimizerEnums" in typehint:
        hint = "TypedFRPROptimizerEnumsPROXY"
    elif "itkFRPROptimizer" in typehint:
        hint = "TypedFRPROptimizerPROXY"
    elif "itkFrontAtom" in typehint:
        hint = "TypedFrontAtomPROXY"
    elif "itkFrequencyShiftedFFTLayoutImageRegionIteratorWithIndex" in typehint:
        hint = "TypedFrequencyShiftedFFTLayoutImageRegionIteratorWithIndexPROXY"
    elif "itkFrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndex" in typehint:
        hint = "TypedFrequencyShiftedFFTLayoutImageRegionConstIteratorWithIndexPROXY"
    elif "itkFrequencyImageRegionIteratorWithIndex" in typehint:
        hint = "TypedFrequencyImageRegionIteratorWithIndexPROXY"
    elif "itkFrequencyImageRegionConstIteratorWithIndex" in typehint:
        hint = "TypedFrequencyImageRegionConstIteratorWithIndexPROXY"
    elif "itkFrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex" in typehint:
        hint = "TypedFrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndexPROXY"
    elif "itkFrequencyHalfHermitianFFTLayoutImageRegionConstIteratorWithIndex" in typehint:
        hint = "TypedFrequencyHalfHermitianFFTLayoutImageRegionConstIteratorWithIndexPROXY"
    elif "itkFrequencyFFTLayoutImageRegionIteratorWithIndex" in typehint:
        hint = "TypedFrequencyFFTLayoutImageRegionIteratorWithIndexPROXY"
    elif "itkFrequencyFFTLayoutImageRegionConstIteratorWithIndex" in typehint:
        hint = "TypedFrequencyFFTLayoutImageRegionConstIteratorWithIndexPROXY"
    elif "itkFrequencyBandImageFilter" in typehint:
        hint = "TypedFrequencyBandImageFilterPROXY"
    elif "itkFreeSurferBinaryMeshIOFactory" in typehint:
        hint = "TypedFreeSurferBinaryMeshIOFactoryPROXY"
    elif "itkFreeSurferBinaryMeshIO" in typehint:
        hint = "TypedFreeSurferBinaryMeshIOPROXY"
    elif "itkFreeSurferAsciiMeshIOFactory" in typehint:
        hint = "TypedFreeSurferAsciiMeshIOFactoryPROXY"
    elif "itkFreeSurferAsciiMeshIO" in typehint:
        hint = "TypedFreeSurferAsciiMeshIOPROXY"
    elif "itkFrameDifferenceVideoFilter" in typehint:
        hint = "TypedFrameDifferenceVideoFilterPROXY"
    elif "itkFrameAverageVideoFilter" in typehint:
        hint = "TypedFrameAverageVideoFilterPROXY"
    elif "itkFourierSeriesPath" in typehint:
        hint = "TypedFourierSeriesPathPROXY"
    elif "itkForwardFFTImageFilter" in typehint:
        hint = "TypedForwardFFTImageFilterPROXY"
    elif "itkForwardDifferenceOperator" in typehint:
        hint = "TypedForwardDifferenceOperatorPROXY"
    elif "itkFortuneSite" in typehint:
        hint = "TypedFortuneSitePROXY"
    elif "itkFortuneHalfEdge" in typehint:
        hint = "TypedFortuneHalfEdgePROXY"
    elif "itkFortuneEdge" in typehint:
        hint = "TypedFortuneEdgePROXY"
    elif "itkFloodFilledSpatialFunctionConditionalIterator" in typehint:
        hint = "TypedFloodFilledSpatialFunctionConditionalIteratorPROXY"
    elif "itkFloodFilledSpatialFunctionConditionalConstIterator" in typehint:
        hint = "TypedFloodFilledSpatialFunctionConditionalConstIteratorPROXY"
    elif "itkFloodFilledImageFunctionConditionalIterator" in typehint:
        hint = "TypedFloodFilledImageFunctionConditionalIteratorPROXY"
    elif "itkFloodFilledImageFunctionConditionalConstIterator" in typehint:
        hint = "TypedFloodFilledImageFunctionConditionalConstIteratorPROXY"
    elif "itkFloodFilledFunctionConditionalConstIterator" in typehint:
        hint = "TypedFloodFilledFunctionConditionalConstIteratorPROXY"
    elif "itkFloatOrDouble" in typehint:
        hint = "TypedFloatOrDoublePROXY"
    elif "itkFloatOrDouble" in typehint:
        hint = "TypedFloatOrDoublePROXY"
    elif "itkFloatOrDouble" in typehint:
        hint = "TypedFloatOrDoublePROXY"
    elif "itkFloatingPointExceptionsEnums" in typehint:
        hint = "TypedFloatingPointExceptionsEnumsPROXY"
    elif "itkFloatingPointExceptions" in typehint:
        hint = "TypedFloatingPointExceptionsPROXY"
    elif "itkFloatIEEETraits" in typehint:
        hint = "TypedFloatIEEETraitsPROXY"
    elif "itkFloatIEEETraits" in typehint:
        hint = "TypedFloatIEEETraitsPROXY"
    elif "itkFloatIEEETraits" in typehint:
        hint = "TypedFloatIEEETraitsPROXY"
    elif "itkFloatIEEE" in typehint:
        hint = "TypedFloatIEEEPROXY"
    elif "itkFlipImageFilter" in typehint:
        hint = "TypedFlipImageFilterPROXY"
    elif "itkFlatStructuringElement" in typehint:
        hint = "TypedFlatStructuringElementPROXY"
    elif "itkFlatnessLabelObjectAccessor" in typehint:
        hint = "TypedFlatnessLabelObjectAccessorPROXY"
    elif "itkflat_region_t" in typehint:
        hint = "Typedflat_region_tPROXY"
    elif "itkflat_region_t" in typehint:
        hint = "Typedflat_region_tPROXY"
    elif "itkFixedImageSamplePoint" in typehint:
        hint = "TypedFixedImageSamplePointPROXY"
    elif "itkFixedCenterOfRotationAffineTransform" in typehint:
        hint = "TypedFixedCenterOfRotationAffineTransformPROXY"
    elif "itkFixedArray" in typehint:
        hint = "TypedFixedArrayPROXY"
    elif "itkFiniteDifferenceSparseImageFunction" in typehint:
        hint = "TypedFiniteDifferenceSparseImageFunctionPROXY"
    elif "itkFiniteDifferenceSparseImageFilter" in typehint:
        hint = "TypedFiniteDifferenceSparseImageFilterPROXY"
    elif "itkFiniteDifferenceImageFilter" in typehint:
        hint = "TypedFiniteDifferenceImageFilterPROXY"
    elif "itkFiniteDifferenceFunction" in typehint:
        hint = "TypedFiniteDifferenceFunctionPROXY"
    elif "itkFiniteCylinderSpatialFunction" in typehint:
        hint = "TypedFiniteCylinderSpatialFunctionPROXY"
    elif "itkFilterState" in typehint:
        hint = "TypedFilterStatePROXY"
    elif "itkFilterRecord" in typehint:
        hint = "TypedFilterRecordPROXY"
    elif "itkFileTools" in typehint:
        hint = "TypedFileToolsPROXY"
    elif "itkFileOutputWindow" in typehint:
        hint = "TypedFileOutputWindowPROXY"
    elif "itkFileListVideoIOFactory" in typehint:
        hint = "TypedFileListVideoIOFactoryPROXY"
    elif "itkFileListVideoIO" in typehint:
        hint = "TypedFileListVideoIOPROXY"
    elif "itkFFTWRealToHalfHermitianForwardFFTImageFilter" in typehint:
        hint = "TypedFFTWRealToHalfHermitianForwardFFTImageFilterPROXY"
    elif "itkFFTWInverseFFTImageFilter" in typehint:
        hint = "TypedFFTWInverseFFTImageFilterPROXY"
    elif "itkFFTWHalfHermitianToRealInverseFFTImageFilter" in typehint:
        hint = "TypedFFTWHalfHermitianToRealInverseFFTImageFilterPROXY"
    elif "itkFFTWGlobalConfiguration" in typehint:
        hint = "TypedFFTWGlobalConfigurationPROXY"
    elif "itkFFTWForwardFFTImageFilter" in typehint:
        hint = "TypedFFTWForwardFFTImageFilterPROXY"
    elif "itkFFTWComplexToComplexFFTImageFilter" in typehint:
        hint = "TypedFFTWComplexToComplexFFTImageFilterPROXY"
    elif "itkFFTShiftImageFilter" in typehint:
        hint = "TypedFFTShiftImageFilterPROXY"
    elif "itkFFTPadImageFilter" in typehint:
        hint = "TypedFFTPadImageFilterPROXY"
    elif "itkFFTNormalizedCorrelationImageFilter" in typehint:
        hint = "TypedFFTNormalizedCorrelationImageFilterPROXY"
    elif "itkFFTConvolutionImageFilter" in typehint:
        hint = "TypedFFTConvolutionImageFilterPROXY"
    elif "itkFeretDiameterLabelObjectAccessor" in typehint:
        hint = "TypedFeretDiameterLabelObjectAccessorPROXY"
    elif "itkFeiExtendedHeader" in typehint:
        hint = "TypedFeiExtendedHeaderPROXY"
    elif "itkFDThreadStruct" in typehint:
        hint = "TypedFDThreadStructPROXY"
    elif "itkFastSymmetricForcesDemonsRegistrationFunction" in typehint:
        hint = "TypedFastSymmetricForcesDemonsRegistrationFunctionPROXY"
    elif "itkFastSymmetricForcesDemonsRegistrationFilter" in typehint:
        hint = "TypedFastSymmetricForcesDemonsRegistrationFilterPROXY"
    elif "itkFastMarchingUpwindGradientImageFilterBase" in typehint:
        hint = "TypedFastMarchingUpwindGradientImageFilterBasePROXY"
    elif "itkFastMarchingUpwindGradientImageFilter" in typehint:
        hint = "TypedFastMarchingUpwindGradientImageFilterPROXY"
    elif "itkFastMarchingTraitsEnums" in typehint:
        hint = "TypedFastMarchingTraitsEnumsPROXY"
    elif "itkFastMarchingTraitsBase" in typehint:
        hint = "TypedFastMarchingTraitsBasePROXY"
    elif "itkFastMarchingTraits" in typehint:
        hint = "TypedFastMarchingTraitsPROXY"
    elif "itkFastMarchingTraits" in typehint:
        hint = "TypedFastMarchingTraitsPROXY"
    elif "itkFastMarchingTraits" in typehint:
        hint = "TypedFastMarchingTraitsPROXY"
    elif "itkFastMarchingThresholdStoppingCriterion" in typehint:
        hint = "TypedFastMarchingThresholdStoppingCriterionPROXY"
    elif "itkFastMarchingReachedTargetNodesStoppingCriterionEnums" in typehint:
        hint = "TypedFastMarchingReachedTargetNodesStoppingCriterionEnumsPROXY"
    elif "itkFastMarchingReachedTargetNodesStoppingCriterion" in typehint:
        hint = "TypedFastMarchingReachedTargetNodesStoppingCriterionPROXY"
    elif "itkFastMarchingQuadEdgeMeshFilterBase" in typehint:
        hint = "TypedFastMarchingQuadEdgeMeshFilterBasePROXY"
    elif "itkFastMarchingNumberOfElementsStoppingCriterion" in typehint:
        hint = "TypedFastMarchingNumberOfElementsStoppingCriterionPROXY"
    elif "itkFastMarchingImageToNodePairContainerAdaptor" in typehint:
        hint = "TypedFastMarchingImageToNodePairContainerAdaptorPROXY"
    elif "itkFastMarchingImageFilterEnums" in typehint:
        hint = "TypedFastMarchingImageFilterEnumsPROXY"
    elif "itkFastMarchingImageFilterBase" in typehint:
        hint = "TypedFastMarchingImageFilterBasePROXY"
    elif "itkFastMarchingImageFilter" in typehint:
        hint = "TypedFastMarchingImageFilterPROXY"
    elif "itkFastMarchingExtensionImageFilterBase" in typehint:
        hint = "TypedFastMarchingExtensionImageFilterBasePROXY"
    elif "itkFastMarchingExtensionImageFilter" in typehint:
        hint = "TypedFastMarchingExtensionImageFilterPROXY"
    elif "itkFastMarchingBase" in typehint:
        hint = "TypedFastMarchingBasePROXY"
    elif "itkFastIncrementalBinaryDilateImageFilter" in typehint:
        hint = "TypedFastIncrementalBinaryDilateImageFilterPROXY"
    elif "itkFastChamferDistanceImageFilter" in typehint:
        hint = "TypedFastChamferDistanceImageFilterPROXY"
    elif "itkFastApproximateRankImageFilter" in typehint:
        hint = "TypedFastApproximateRankImageFilterPROXY"
    elif "itkFancyString" in typehint:
        hint = "TypedFancyStringPROXY"
    elif "itkface_pixel_t" in typehint:
        hint = "Typedface_pixel_tPROXY"
    elif "itkExtrapolateImageFunction" in typehint:
        hint = "TypedExtrapolateImageFunctionPROXY"
    elif "itkExtractSliceImageFilterEnums" in typehint:
        hint = "TypedExtractSliceImageFilterEnumsPROXY"
    elif "itkExtractSliceImageFilter" in typehint:
        hint = "TypedExtractSliceImageFilterPROXY"
    elif "itkExtractOrthogonalSwath2DImageFilter" in typehint:
        hint = "TypedExtractOrthogonalSwath2DImageFilterPROXY"
    elif "itkExtractionImageFilterEnums" in typehint:
        hint = "TypedExtractionImageFilterEnumsPROXY"
    elif "itkExtractImageFilterRegionCopier" in typehint:
        hint = "TypedExtractImageFilterRegionCopierPROXY"
    elif "itkExtractImageFilterEnums" in typehint:
        hint = "TypedExtractImageFilterEnumsPROXY"
    elif "itkExtractImageFilter" in typehint:
        hint = "TypedExtractImageFilterPROXY"
    elif "itkExtensionVelocitiesImageFilter" in typehint:
        hint = "TypedExtensionVelocitiesImageFilterPROXY"
    elif "itkExpPixelAccessor" in typehint:
        hint = "TypedExpPixelAccessorPROXY"
    elif "itkExponentialDisplacementFieldImageFilter" in typehint:
        hint = "TypedExponentialDisplacementFieldImageFilterPROXY"
    elif "itkExpNegativePixelAccessor" in typehint:
        hint = "TypedExpNegativePixelAccessorPROXY"
    elif "itkExpNegativeImageFilter" in typehint:
        hint = "TypedExpNegativeImageFilterPROXY"
    elif "itkExpNegativeImageAdaptor" in typehint:
        hint = "TypedExpNegativeImageAdaptorPROXY"
    elif "itkExpNegative" in typehint:
        hint = "TypedExpNegativePROXY"
    elif "itkExpImageFilter" in typehint:
        hint = "TypedExpImageFilterPROXY"
    elif "itkExpImageAdaptor" in typehint:
        hint = "TypedExpImageAdaptorPROXY"
    elif "itkExpectationMaximizationMixtureModelEstimatorEnums" in typehint:
        hint = "TypedExpectationMaximizationMixtureModelEstimatorEnumsPROXY"
    elif "itkExpectationMaximizationMixtureModelEstimator" in typehint:
        hint = "TypedExpectationMaximizationMixtureModelEstimatorPROXY"
    elif "itkExpectationBasedPointSetToPointSetMetricv4" in typehint:
        hint = "TypedExpectationBasedPointSetToPointSetMetricv4PROXY"
    elif "itkExpandImageFilter" in typehint:
        hint = "TypedExpandImageFilterPROXY"
    elif "itkExp" in typehint:
        hint = "TypedExpPROXY"
    elif "itkExhaustiveOptimizerv4" in typehint:
        hint = "TypedExhaustiveOptimizerv4PROXY"
    elif "itkExhaustiveOptimizer" in typehint:
        hint = "TypedExhaustiveOptimizerPROXY"
    elif "itkExceptionAction" in typehint:
        hint = "TypedExceptionActionPROXY"
    elif "itkEventObject" in typehint:
        hint = "TypedEventObjectPROXY"
    elif "itkEuler3DTransform" in typehint:
        hint = "TypedEuler3DTransformPROXY"
    elif "itkEuler2DTransform" in typehint:
        hint = "TypedEuler2DTransformPROXY"
    elif "itkEuclideanSquareDistanceMetric" in typehint:
        hint = "TypedEuclideanSquareDistanceMetricPROXY"
    elif "itkEuclideanDistancePointSetToPointSetMetricv4" in typehint:
        hint = "TypedEuclideanDistancePointSetToPointSetMetricv4PROXY"
    elif "itkEuclideanDistancePointMetric" in typehint:
        hint = "TypedEuclideanDistancePointMetricPROXY"
    elif "itkEuclideanDistanceMetric" in typehint:
        hint = "TypedEuclideanDistanceMetricPROXY"
    elif "itkESMDemonsRegistrationFunctionEnums" in typehint:
        hint = "TypedESMDemonsRegistrationFunctionEnumsPROXY"
    elif "itkESMDemonsRegistrationFunction" in typehint:
        hint = "TypedESMDemonsRegistrationFunctionPROXY"
    elif "itkErodeObjectMorphologyImageFilter" in typehint:
        hint = "TypedErodeObjectMorphologyImageFilterPROXY"
    elif "itkEquivalentSphericalRadiusLabelObjectAccessor" in typehint:
        hint = "TypedEquivalentSphericalRadiusLabelObjectAccessorPROXY"
    elif "itkEquivalentSphericalPerimeterLabelObjectAccessor" in typehint:
        hint = "TypedEquivalentSphericalPerimeterLabelObjectAccessorPROXY"
    elif "itkEquivalentEllipsoidDiameterLabelObjectAccessor" in typehint:
        hint = "TypedEquivalentEllipsoidDiameterLabelObjectAccessorPROXY"
    elif "itkEquivalencyTable" in typehint:
        hint = "TypedEquivalencyTablePROXY"
    elif "itkEquivalenceRelabeler" in typehint:
        hint = "TypedEquivalenceRelabelerPROXY"
    elif "itkEqualityComparable" in typehint:
        hint = "TypedEqualityComparablePROXY"
    elif "itkEqual" in typehint:
        hint = "TypedEqualPROXY"
    elif "itkEnableIf" in typehint:
        hint = "TypedEnableIfPROXY"
    elif "itkEmptyPixelAccessParameter" in typehint:
        hint = "TypedEmptyPixelAccessParameterPROXY"
    elif "itkEmptyAccessorFunctor" in typehint:
        hint = "TypedEmptyAccessorFunctorPROXY"
    elif "itkElongationLabelObjectAccessor" in typehint:
        hint = "TypedElongationLabelObjectAccessorPROXY"
    elif "itkEllipsoidInteriorExteriorSpatialFunction" in typehint:
        hint = "TypedEllipsoidInteriorExteriorSpatialFunctionPROXY"
    elif "itkEllipseSpatialObject" in typehint:
        hint = "TypedEllipseSpatialObjectPROXY"
    elif "itkElementWrapperPointerInterface" in typehint:
        hint = "TypedElementWrapperPointerInterfacePROXY"
    elif "itkElementWrapperInterface" in typehint:
        hint = "TypedElementWrapperInterfacePROXY"
    elif "itkElasticBodySplineKernelTransform" in typehint:
        hint = "TypedElasticBodySplineKernelTransformPROXY"
    elif "itkElasticBodyReciprocalSplineKernelTransform" in typehint:
        hint = "TypedElasticBodyReciprocalSplineKernelTransformPROXY"
    elif "itkEigenValueOrder" in typehint:
        hint = "TypedEigenValueOrderPROXY"
    elif "itkEigenAnalysis2DImageFilter" in typehint:
        hint = "TypedEigenAnalysis2DImageFilterPROXY"
    elif "itkEdgePotentialImageFilter" in typehint:
        hint = "TypedEdgePotentialImageFilterPROXY"
    elif "itkEdgePotential" in typehint:
        hint = "TypedEdgePotentialPROXY"
    elif "itkEdgeDecimationQuadEdgeMeshFilter" in typehint:
        hint = "TypedEdgeDecimationQuadEdgeMeshFilterPROXY"
    elif "itkedge_pair_t" in typehint:
        hint = "Typededge_pair_tPROXY"
    elif "itkDynamicLoader" in typehint:
        hint = "TypedDynamicLoaderPROXY"
    elif "itkDumpOldValues" in typehint:
        hint = "TypedDumpOldValuesPROXY"
    elif "itkDTITubeSpatialObjectPointField" in typehint:
        hint = "TypedDTITubeSpatialObjectPointFieldPROXY"
    elif "itkDTITubeSpatialObjectPointEnums" in typehint:
        hint = "TypedDTITubeSpatialObjectPointEnumsPROXY"
    elif "itkDTITubeSpatialObjectPoint" in typehint:
        hint = "TypedDTITubeSpatialObjectPointPROXY"
    elif "itkDTITubeSpatialObject" in typehint:
        hint = "TypedDTITubeSpatialObjectPROXY"
    elif "itkDoubleThresholdImageFilter" in typehint:
        hint = "TypedDoubleThresholdImageFilterPROXY"
    elif "itkDontShrinkToFit" in typehint:
        hint = "TypedDontShrinkToFitPROXY"
    elif "itkDOMWriter" in typehint:
        hint = "TypedDOMWriterPROXY"
    elif "itkDOMTextNode" in typehint:
        hint = "TypedDOMTextNodePROXY"
    elif "itkDOMReader" in typehint:
        hint = "TypedDOMReaderPROXY"
    elif "itkDOMNodeXMLWriter" in typehint:
        hint = "TypedDOMNodeXMLWriterPROXY"
    elif "itkDOMNodeXMLReader" in typehint:
        hint = "TypedDOMNodeXMLReaderPROXY"
    elif "itkDOMNode" in typehint:
        hint = "TypedDOMNodePROXY"
    elif "itkDomainThreader" in typehint:
        hint = "TypedDomainThreaderPROXY"
    elif "itkDivReal" in typehint:
        hint = "TypedDivRealPROXY"
    elif "itkDivisionOperators" in typehint:
        hint = "TypedDivisionOperatorsPROXY"
    elif "itkDivisionAndAssignOperators" in typehint:
        hint = "TypedDivisionAndAssignOperatorsPROXY"
    elif "itkDivideOrZeroOutImageFilter" in typehint:
        hint = "TypedDivideOrZeroOutImageFilterPROXY"
    elif "itkDivideOrZeroOut" in typehint:
        hint = "TypedDivideOrZeroOutPROXY"
    elif "itkDivideImageFilter" in typehint:
        hint = "TypedDivideImageFilterPROXY"
    elif "itkDivFloor" in typehint:
        hint = "TypedDivFloorPROXY"
    elif "itkDiv" in typehint:
        hint = "TypedDivPROXY"
    elif "itkDistanceToCentroidMembershipFunction" in typehint:
        hint = "TypedDistanceToCentroidMembershipFunctionPROXY"
    elif "itkDistanceMetric" in typehint:
        hint = "TypedDistanceMetricPROXY"
    elif "itkDisplacementFieldTransformParametersAdaptor" in typehint:
        hint = "TypedDisplacementFieldTransformParametersAdaptorPROXY"
    elif "itkDisplacementFieldTransform" in typehint:
        hint = "TypedDisplacementFieldTransformPROXY"
    elif "itkDisplacementFieldToBSplineImageFilter" in typehint:
        hint = "TypedDisplacementFieldToBSplineImageFilterPROXY"
    elif "itkDisplacementFieldJacobianDeterminantFilter" in typehint:
        hint = "TypedDisplacementFieldJacobianDeterminantFilterPROXY"
    elif "itkDispatchBase" in typehint:
        hint = "TypedDispatchBasePROXY"
    elif "itkDispatchBase" in typehint:
        hint = "TypedDispatchBasePROXY"
    elif "itkDispatchBase" in typehint:
        hint = "TypedDispatchBasePROXY"
    elif "itkDispatchBase" in typehint:
        hint = "TypedDispatchBasePROXY"
    elif "itkDispatchBase" in typehint:
        hint = "TypedDispatchBasePROXY"
    elif "itkDispatch" in typehint:
        hint = "TypedDispatchPROXY"
    elif "itkDispatch" in typehint:
        hint = "TypedDispatchPROXY"
    elif "itkDispatch" in typehint:
        hint = "TypedDispatchPROXY"
    elif "itkDispatch" in typehint:
        hint = "TypedDispatchPROXY"
    elif "itkDiscretePrincipalCurvaturesQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscretePrincipalCurvaturesQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteMinimumCurvatureQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteMinimumCurvatureQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteMeanCurvatureQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteMeanCurvatureQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteMaximumCurvatureQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteMaximumCurvatureQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteLevelSetImage" in typehint:
        hint = "TypedDiscreteLevelSetImagePROXY"
    elif "itkDiscreteHessianGaussianImageFunction" in typehint:
        hint = "TypedDiscreteHessianGaussianImageFunctionPROXY"
    elif "itkDiscreteGradientMagnitudeGaussianImageFunction" in typehint:
        hint = "TypedDiscreteGradientMagnitudeGaussianImageFunctionPROXY"
    elif "itkDiscreteGaussianImageFilter" in typehint:
        hint = "TypedDiscreteGaussianImageFilterPROXY"
    elif "itkDiscreteGaussianDerivativeImageFunction" in typehint:
        hint = "TypedDiscreteGaussianDerivativeImageFunctionPROXY"
    elif "itkDiscreteGaussianDerivativeImageFilter" in typehint:
        hint = "TypedDiscreteGaussianDerivativeImageFilterPROXY"
    elif "itkDiscreteGaussianCurvatureQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteGaussianCurvatureQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteCurvatureTensorQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteCurvatureTensorQuadEdgeMeshFilterPROXY"
    elif "itkDiscreteCurvatureQuadEdgeMeshFilter" in typehint:
        hint = "TypedDiscreteCurvatureQuadEdgeMeshFilterPROXY"
    elif "itkDisableIfC" in typehint:
        hint = "TypedDisableIfCPROXY"
    elif "itkDisableIf" in typehint:
        hint = "TypedDisableIfPROXY"
    elif "itkDirectory" in typehint:
        hint = "TypedDirectoryPROXY"
    elif "itkDirectionCost" in typehint:
        hint = "TypedDirectionCostPROXY"
    elif "itkDirectionCollapseStrategy" in typehint:
        hint = "TypedDirectionCollapseStrategyPROXY"
    elif "itkDirectFourierReconstructionImageToImageFilter" in typehint:
        hint = "TypedDirectFourierReconstructionImageToImageFilterPROXY"
    elif "itkDirectedHausdorffDistanceImageFilter" in typehint:
        hint = "TypedDirectedHausdorffDistanceImageFilterPROXY"
    elif "itkDilateObjectMorphologyImageFilter" in typehint:
        hint = "TypedDilateObjectMorphologyImageFilterPROXY"
    elif "itkDiffusionTensor3DReconstructionImageFilterEnums" in typehint:
        hint = "TypedDiffusionTensor3DReconstructionImageFilterEnumsPROXY"
    elif "itkDiffusionTensor3DReconstructionImageFilter" in typehint:
        hint = "TypedDiffusionTensor3DReconstructionImageFilterPROXY"
    elif "itkDiffusionTensor3D" in typehint:
        hint = "TypedDiffusionTensor3DPROXY"
    elif "itkDifferenceOfGaussiansGradientImageFilter" in typehint:
        hint = "TypedDifferenceOfGaussiansGradientImageFilterPROXY"
    elif "itkDiffeomorphicDemonsRegistrationFilter" in typehint:
        hint = "TypedDiffeomorphicDemonsRegistrationFilterPROXY"
    elif "itkDFMFormat" in typehint:
        hint = "TypedDFMFormatPROXY"
    elif "itkDerivativeOperator" in typehint:
        hint = "TypedDerivativeOperatorPROXY"
    elif "itkDerivativeImageFilter" in typehint:
        hint = "TypedDerivativeImageFilterPROXY"
    elif "itkDerivativeBufferManager" in typehint:
        hint = "TypedDerivativeBufferManagerPROXY"
    elif "itkDenseFrequencyContainer2" in typehint:
        hint = "TypedDenseFrequencyContainer2PROXY"
    elif "itkDenseFiniteDifferenceImageFilter" in typehint:
        hint = "TypedDenseFiniteDifferenceImageFilterPROXY"
    elif "itkDenseFDThreadStruct" in typehint:
        hint = "TypedDenseFDThreadStructPROXY"
    elif "itkDemonsRegistrationFunction" in typehint:
        hint = "TypedDemonsRegistrationFunctionPROXY"
    elif "itkDemonsRegistrationFilter" in typehint:
        hint = "TypedDemonsRegistrationFilterPROXY"
    elif "itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedDemonsImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkDemonsImageToImageMetricv4" in typehint:
        hint = "TypedDemonsImageToImageMetricv4PROXY"
    elif "itkDelaunayConformingQuadEdgeMeshFilter" in typehint:
        hint = "TypedDelaunayConformingQuadEdgeMeshFilterPROXY"
    elif "itkDeformableSimplexMesh3DGradientConstraintForceFilterEnums" in typehint:
        hint = "TypedDeformableSimplexMesh3DGradientConstraintForceFilterEnumsPROXY"
    elif "itkDeformableSimplexMesh3DGradientConstraintForceFilter" in typehint:
        hint = "TypedDeformableSimplexMesh3DGradientConstraintForceFilterPROXY"
    elif "itkDeformableSimplexMesh3DFilter" in typehint:
        hint = "TypedDeformableSimplexMesh3DFilterPROXY"
    elif "itkDeformableSimplexMesh3DBalloonForceFilter" in typehint:
        hint = "TypedDeformableSimplexMesh3DBalloonForceFilterPROXY"
    elif "itkDefaultVectorPixelAccessorFunctor" in typehint:
        hint = "TypedDefaultVectorPixelAccessorFunctorPROXY"
    elif "itkDefaultVectorPixelAccessor" in typehint:
        hint = "TypedDefaultVectorPixelAccessorPROXY"
    elif "itkDefaultStaticMeshTraits" in typehint:
        hint = "TypedDefaultStaticMeshTraitsPROXY"
    elif "itkDefaultPixelAccessorFunctor" in typehint:
        hint = "TypedDefaultPixelAccessorFunctorPROXY"
    elif "itkDefaultPixelAccessor" in typehint:
        hint = "TypedDefaultPixelAccessorPROXY"
    elif "itkDefaultImageToImageMetricTraitsv4" in typehint:
        hint = "TypedDefaultImageToImageMetricTraitsv4PROXY"
    elif "itkDefaultDynamicMeshTraits" in typehint:
        hint = "TypedDefaultDynamicMeshTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConvertPixelTraits" in typehint:
        hint = "TypedDefaultConvertPixelTraitsPROXY"
    elif "itkDefaultConstructible" in typehint:
        hint = "TypedDefaultConstructiblePROXY"
    elif "itkDecodingFormat" in typehint:
        hint = "TypedDecodingFormatPROXY"
    elif "itkDecodingFormat" in typehint:
        hint = "TypedDecodingFormatPROXY"
    elif "itkDecisionRule" in typehint:
        hint = "TypedDecisionRulePROXY"
    elif "itkDecimationQuadEdgeMeshFilter" in typehint:
        hint = "TypedDecimationQuadEdgeMeshFilterPROXY"
    elif "itkDecimateFramesVideoFilter" in typehint:
        hint = "TypedDecimateFramesVideoFilterPROXY"
    elif "itkDCMTKSeriesFileNames" in typehint:
        hint = "TypedDCMTKSeriesFileNamesPROXY"
    elif "itkDCMTKSequence" in typehint:
        hint = "TypedDCMTKSequencePROXY"
    elif "itkDCMTKItem" in typehint:
        hint = "TypedDCMTKItemPROXY"
    elif "itkDCMTKImageIOFactory" in typehint:
        hint = "TypedDCMTKImageIOFactoryPROXY"
    elif "itkDCMTKImageIOEnums" in typehint:
        hint = "TypedDCMTKImageIOEnumsPROXY"
    elif "itkDCMTKImageIO" in typehint:
        hint = "TypedDCMTKImageIOPROXY"
    elif "itkDCMTKFileReader" in typehint:
        hint = "TypedDCMTKFileReaderPROXY"
    elif "itkDataType" in typehint:
        hint = "TypedDataTypePROXY"
    elif "itkDataObjectIterator" in typehint:
        hint = "TypedDataObjectIteratorPROXY"
    elif "itkDataObjectError" in typehint:
        hint = "TypedDataObjectErrorPROXY"
    elif "itkDataObjectDecorator" in typehint:
        hint = "TypedDataObjectDecoratorPROXY"
    elif "itkDataObjectConstIterator" in typehint:
        hint = "TypedDataObjectConstIteratorPROXY"
    elif "itkDataObject" in typehint:
        hint = "TypedDataObjectPROXY"
    elif "itkDanielssonDistanceMapImageFilter" in typehint:
        hint = "TypedDanielssonDistanceMapImageFilterPROXY"
    elif "itkCyclicShiftImageFilter" in typehint:
        hint = "TypedCyclicShiftImageFilterPROXY"
    elif "itkCustomColormapFunction" in typehint:
        hint = "TypedCustomColormapFunctionPROXY"
    elif "itkCurvesLevelSetImageFilter" in typehint:
        hint = "TypedCurvesLevelSetImageFilterPROXY"
    elif "itkCurvesLevelSetFunction" in typehint:
        hint = "TypedCurvesLevelSetFunctionPROXY"
    elif "itkCurvatureRegistrationFilter" in typehint:
        hint = "TypedCurvatureRegistrationFilterPROXY"
    elif "itkCurvatureNDAnisotropicDiffusionFunction" in typehint:
        hint = "TypedCurvatureNDAnisotropicDiffusionFunctionPROXY"
    elif "itkCurvatureFlowImageFilter" in typehint:
        hint = "TypedCurvatureFlowImageFilterPROXY"
    elif "itkCurvatureFlowFunction" in typehint:
        hint = "TypedCurvatureFlowFunctionPROXY"
    elif "itkCurvatureAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedCurvatureAnisotropicDiffusionImageFilterPROXY"
    elif "itkCumulativeGaussianOptimizer" in typehint:
        hint = "TypedCumulativeGaussianOptimizerPROXY"
    elif "itkCumulativeGaussianCostFunction" in typehint:
        hint = "TypedCumulativeGaussianCostFunctionPROXY"
    elif "itkCSVNumericObjectFileWriter" in typehint:
        hint = "TypedCSVNumericObjectFileWriterPROXY"
    elif "itkCSVFileReaderBase" in typehint:
        hint = "TypedCSVFileReaderBasePROXY"
    elif "itkCSVArray2DFileReader" in typehint:
        hint = "TypedCSVArray2DFileReaderPROXY"
    elif "itkCSVArray2DDataObject" in typehint:
        hint = "TypedCSVArray2DDataObjectPROXY"
    elif "itkCStyleCommand" in typehint:
        hint = "TypedCStyleCommandPROXY"
    elif "itkCrossHelper" in typehint:
        hint = "TypedCrossHelperPROXY"
    elif "itkCross" in typehint:
        hint = "TypedCrossPROXY"
    elif "itkCropLabelMapFilter" in typehint:
        hint = "TypedCropLabelMapFilterPROXY"
    elif "itkCropImageFilter" in typehint:
        hint = "TypedCropImageFilterPROXY"
    elif "itkCreateObjectFunctionBase" in typehint:
        hint = "TypedCreateObjectFunctionBasePROXY"
    elif "itkCreateObjectFunction" in typehint:
        hint = "TypedCreateObjectFunctionPROXY"
    elif "itkCoxDeBoorBSplineKernelFunction" in typehint:
        hint = "TypedCoxDeBoorBSplineKernelFunctionPROXY"
    elif "itkCovariantVector" in typehint:
        hint = "TypedCovariantVectorPROXY"
    elif "itkCovarianceSampleFilter" in typehint:
        hint = "TypedCovarianceSampleFilterPROXY"
    elif "itkCovarianceImageFunction" in typehint:
        hint = "TypedCovarianceImageFunctionPROXY"
    elif "itkCostFunctionTemplate" in typehint:
        hint = "TypedCostFunctionTemplatePROXY"
    elif "itkCostFunction" in typehint:
        hint = "TypedCostFunctionPROXY"
    elif "itkCosPixelAccessor" in typehint:
        hint = "TypedCosPixelAccessorPROXY"
    elif "itkCosineWindowFunction" in typehint:
        hint = "TypedCosineWindowFunctionPROXY"
    elif "itkCosImageFilter" in typehint:
        hint = "TypedCosImageFilterPROXY"
    elif "itkCosImageAdaptor" in typehint:
        hint = "TypedCosImageAdaptorPROXY"
    elif "itkCos" in typehint:
        hint = "TypedCosPROXY"
    elif "itkCorrespondenceDataStructureIterator" in typehint:
        hint = "TypedCorrespondenceDataStructureIteratorPROXY"
    elif "itkCorrelationMetricValueDerivativePerThreadStruct" in typehint:
        hint = "TypedCorrelationMetricValueDerivativePerThreadStructPROXY"
    elif "itkCorrelationMetricPerThreadStruct" in typehint:
        hint = "TypedCorrelationMetricPerThreadStructPROXY"
    elif "itkCorrelationImageToImageMetricv4HelperThreader" in typehint:
        hint = "TypedCorrelationImageToImageMetricv4HelperThreaderPROXY"
    elif "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedCorrelationImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkCorrelationImageToImageMetricv4" in typehint:
        hint = "TypedCorrelationImageToImageMetricv4PROXY"
    elif "itkCorrelationCoefficientHistogramImageToImageMetric" in typehint:
        hint = "TypedCorrelationCoefficientHistogramImageToImageMetricPROXY"
    elif "itkCopyConstructible" in typehint:
        hint = "TypedCopyConstructiblePROXY"
    elif "itkCopperColormapFunction" in typehint:
        hint = "TypedCopperColormapFunctionPROXY"
    elif "itkCopierDispatchBase" in typehint:
        hint = "TypedCopierDispatchBasePROXY"
    elif "itkCopierDispatch" in typehint:
        hint = "TypedCopierDispatchPROXY"
    elif "itkCoolColormapFunction" in typehint:
        hint = "TypedCoolColormapFunctionPROXY"
    elif "itkConvolutionImageFilterOutputRegion" in typehint:
        hint = "TypedConvolutionImageFilterOutputRegionPROXY"
    elif "itkConvolutionImageFilterBaseEnums" in typehint:
        hint = "TypedConvolutionImageFilterBaseEnumsPROXY"
    elif "itkConvolutionImageFilterBase" in typehint:
        hint = "TypedConvolutionImageFilterBasePROXY"
    elif "itkConvolutionImageFilter" in typehint:
        hint = "TypedConvolutionImageFilterPROXY"
    elif "itkConvertPixelBuffer" in typehint:
        hint = "TypedConvertPixelBufferPROXY"
    elif "itkConvertPixelBuffer" in typehint:
        hint = "TypedConvertPixelBufferPROXY"
    elif "itkConvertPixelBuffer" in typehint:
        hint = "TypedConvertPixelBufferPROXY"
    elif "itkConvertLabelMapFilter" in typehint:
        hint = "TypedConvertLabelMapFilterPROXY"
    elif "itkConvertible" in typehint:
        hint = "TypedConvertiblePROXY"
    elif "itkConvergenceMonitoringFunction" in typehint:
        hint = "TypedConvergenceMonitoringFunctionPROXY"
    elif "itkContourType" in typehint:
        hint = "TypedContourTypePROXY"
    elif "itkContourSpatialObjectPoint" in typehint:
        hint = "TypedContourSpatialObjectPointPROXY"
    elif "itkContourSpatialObjectEnums" in typehint:
        hint = "TypedContourSpatialObjectEnumsPROXY"
    elif "itkContourSpatialObject" in typehint:
        hint = "TypedContourSpatialObjectPROXY"
    elif "itkContourMeanDistanceImageFilter" in typehint:
        hint = "TypedContourMeanDistanceImageFilterPROXY"
    elif "itkContourExtractor2DImageFilter" in typehint:
        hint = "TypedContourExtractor2DImageFilterPROXY"
    elif "itkContourDirectedMeanDistanceImageFilter" in typehint:
        hint = "TypedContourDirectedMeanDistanceImageFilterPROXY"
    elif "itkContourData" in typehint:
        hint = "TypedContourDataPROXY"
    elif "itkContinuousIndex" in typehint:
        hint = "TypedContinuousIndexPROXY"
    elif "itkConstSparseFieldLayerIterator" in typehint:
        hint = "TypedConstSparseFieldLayerIteratorPROXY"
    elif "itkConstSliceIterator" in typehint:
        hint = "TypedConstSliceIteratorPROXY"
    elif "itkConstShapedNeighborhoodIterator" in typehint:
        hint = "TypedConstShapedNeighborhoodIteratorPROXY"
    elif "itkConstReverseIterator" in typehint:
        hint = "TypedConstReverseIteratorPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstraints" in typehint:
        hint = "TypedConstraintsPROXY"
    elif "itkConstrainedValueDifferenceImageFilter" in typehint:
        hint = "TypedConstrainedValueDifferenceImageFilterPROXY"
    elif "itkConstrainedValueDifference" in typehint:
        hint = "TypedConstrainedValueDifferencePROXY"
    elif "itkConstrainedValueAdditionImageFilter" in typehint:
        hint = "TypedConstrainedValueAdditionImageFilterPROXY"
    elif "itkConstrainedValueAddition" in typehint:
        hint = "TypedConstrainedValueAdditionPROXY"
    elif "itkConstrainedRegionBasedLevelSetFunctionSharedData" in typehint:
        hint = "TypedConstrainedRegionBasedLevelSetFunctionSharedDataPROXY"
    elif "itkConstNeighborhoodIteratorWithOnlyIndex" in typehint:
        hint = "TypedConstNeighborhoodIteratorWithOnlyIndexPROXY"
    elif "itkConstNeighborhoodIterator" in typehint:
        hint = "TypedConstNeighborhoodIteratorPROXY"
    elif "itkConstLineIterator" in typehint:
        hint = "TypedConstLineIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIterator" in typehint:
        hint = "TypedConstIteratorPROXY"
    elif "itkConstIndexIterator" in typehint:
        hint = "TypedConstIndexIteratorPROXY"
    elif "itkConstantVelocityFieldTransformParametersAdaptor" in typehint:
        hint = "TypedConstantVelocityFieldTransformParametersAdaptorPROXY"
    elif "itkConstantVelocityFieldTransform" in typehint:
        hint = "TypedConstantVelocityFieldTransformPROXY"
    elif "itkConstantPointerWrapper" in typehint:
        hint = "TypedConstantPointerWrapperPROXY"
    elif "itkConstantPadImageFilter" in typehint:
        hint = "TypedConstantPadImageFilterPROXY"
    elif "itkConstantBoundaryImageNeighborhoodPixelAccessPolicy" in typehint:
        hint = "TypedConstantBoundaryImageNeighborhoodPixelAccessPolicyPROXY"
    elif "itkConstantBoundaryCondition" in typehint:
        hint = "TypedConstantBoundaryConditionPROXY"
    elif "itkconst_iterator" in typehint:
        hint = "Typedconst_iteratorPROXY"
    elif "itkconnectivity_t" in typehint:
        hint = "Typedconnectivity_tPROXY"
    elif "itkConnectivity" in typehint:
        hint = "TypedConnectivityPROXY"
    elif "itkConnectedThresholdImageFilterEnums" in typehint:
        hint = "TypedConnectedThresholdImageFilterEnumsPROXY"
    elif "itkConnectedThresholdImageFilter" in typehint:
        hint = "TypedConnectedThresholdImageFilterPROXY"
    elif "itkConnectedRegionsMeshFilter" in typehint:
        hint = "TypedConnectedRegionsMeshFilterPROXY"
    elif "itkConnectedImageNeighborhoodShape" in typehint:
        hint = "TypedConnectedImageNeighborhoodShapePROXY"
    elif "itkConnectedComponentImageFilter" in typehint:
        hint = "TypedConnectedComponentImageFilterPROXY"
    elif "itkConnectedComponentFunctorImageFilter" in typehint:
        hint = "TypedConnectedComponentFunctorImageFilterPROXY"
    elif "itkConjugateGradientOptimizer" in typehint:
        hint = "TypedConjugateGradientOptimizerPROXY"
    elif "itkConjugateGradientLineSearchOptimizerv4Template" in typehint:
        hint = "TypedConjugateGradientLineSearchOptimizerv4TemplatePROXY"
    elif "itkConicShellInteriorExteriorSpatialFunction" in typehint:
        hint = "TypedConicShellInteriorExteriorSpatialFunctionPROXY"
    elif "itkConformalMatrixCoefficients" in typehint:
        hint = "TypedConformalMatrixCoefficientsPROXY"
    elif "itkConformalFlatteningMeshFilter" in typehint:
        hint = "TypedConformalFlatteningMeshFilterPROXY"
    elif "itkConfidenceConnectedImageFilter" in typehint:
        hint = "TypedConfidenceConnectedImageFilterPROXY"
    elif "itkConditionalConstIterator" in typehint:
        hint = "TypedConditionalConstIteratorPROXY"
    elif "itkCompression" in typehint:
        hint = "TypedCompressionPROXY"
    elif "itkCompositeTransformIOHelperTemplate" in typehint:
        hint = "TypedCompositeTransformIOHelperTemplatePROXY"
    elif "itkCompositeTransform" in typehint:
        hint = "TypedCompositeTransformPROXY"
    elif "itkComposeScaleSkewVersor3DTransform" in typehint:
        hint = "TypedComposeScaleSkewVersor3DTransformPROXY"
    elif "itkComposeImageFilter" in typehint:
        hint = "TypedComposeImageFilterPROXY"
    elif "itkComposeDisplacementFieldsImageFilter" in typehint:
        hint = "TypedComposeDisplacementFieldsImageFilterPROXY"
    elif "itkComponentState" in typehint:
        hint = "TypedComponentStatePROXY"
    elif "itkComplexToRealPixelAccessor" in typehint:
        hint = "TypedComplexToRealPixelAccessorPROXY"
    elif "itkComplexToRealImageFilter" in typehint:
        hint = "TypedComplexToRealImageFilterPROXY"
    elif "itkComplexToRealImageAdaptor" in typehint:
        hint = "TypedComplexToRealImageAdaptorPROXY"
    elif "itkComplexToReal" in typehint:
        hint = "TypedComplexToRealPROXY"
    elif "itkComplexToPhasePixelAccessor" in typehint:
        hint = "TypedComplexToPhasePixelAccessorPROXY"
    elif "itkComplexToPhaseImageFilter" in typehint:
        hint = "TypedComplexToPhaseImageFilterPROXY"
    elif "itkComplexToPhaseImageAdaptor" in typehint:
        hint = "TypedComplexToPhaseImageAdaptorPROXY"
    elif "itkComplexToPhase" in typehint:
        hint = "TypedComplexToPhasePROXY"
    elif "itkComplexToModulusPixelAccessor" in typehint:
        hint = "TypedComplexToModulusPixelAccessorPROXY"
    elif "itkComplexToModulusImageFilter" in typehint:
        hint = "TypedComplexToModulusImageFilterPROXY"
    elif "itkComplexToModulusImageAdaptor" in typehint:
        hint = "TypedComplexToModulusImageAdaptorPROXY"
    elif "itkComplexToModulus" in typehint:
        hint = "TypedComplexToModulusPROXY"
    elif "itkComplexToImaginaryPixelAccessor" in typehint:
        hint = "TypedComplexToImaginaryPixelAccessorPROXY"
    elif "itkComplexToImaginaryImageFilter" in typehint:
        hint = "TypedComplexToImaginaryImageFilterPROXY"
    elif "itkComplexToImaginaryImageAdaptor" in typehint:
        hint = "TypedComplexToImaginaryImageAdaptorPROXY"
    elif "itkComplexToImaginary" in typehint:
        hint = "TypedComplexToImaginaryPROXY"
    elif "itkComplexToComplexFFTImageFilterEnums" in typehint:
        hint = "TypedComplexToComplexFFTImageFilterEnumsPROXY"
    elif "itkComplexToComplexFFTImageFilter" in typehint:
        hint = "TypedComplexToComplexFFTImageFilterPROXY"
    elif "itkComplexConjugatePixelAccessor" in typehint:
        hint = "TypedComplexConjugatePixelAccessorPROXY"
    elif "itkComplexConjugateImageAdaptor" in typehint:
        hint = "TypedComplexConjugateImageAdaptorPROXY"
    elif "itkComplexBSplineInterpolateImageFunction" in typehint:
        hint = "TypedComplexBSplineInterpolateImageFunctionPROXY"
    elif "itkCompensatedSummation" in typehint:
        hint = "TypedCompensatedSummationPROXY"
    elif "itkComparisonImageFilter" in typehint:
        hint = "TypedComparisonImageFilterPROXY"
    elif "itkComparePixStruct" in typehint:
        hint = "TypedComparePixStructPROXY"
    elif "itkCompareHistogramImageToImageMetric" in typehint:
        hint = "TypedCompareHistogramImageToImageMetricPROXY"
    elif "itkComparator" in typehint:
        hint = "TypedComparatorPROXY"
    elif "itkComparator" in typehint:
        hint = "TypedComparatorPROXY"
    elif "itkComparable" in typehint:
        hint = "TypedComparablePROXY"
    elif "itkCommonEnums" in typehint:
        hint = "TypedCommonEnumsPROXY"
    elif "itkCommandVnlIterationUpdate" in typehint:
        hint = "TypedCommandVnlIterationUpdatePROXY"
    elif "itkCommandIterationUpdatev4" in typehint:
        hint = "TypedCommandIterationUpdatev4PROXY"
    elif "itkCommandIterationUpdate" in typehint:
        hint = "TypedCommandIterationUpdatePROXY"
    elif "itkCommand" in typehint:
        hint = "TypedCommandPROXY"
    elif "itkColorTable" in typehint:
        hint = "TypedColorTablePROXY"
    elif "itkColormapFunction" in typehint:
        hint = "TypedColormapFunctionPROXY"
    elif "itkCollidingFrontsImageFilter" in typehint:
        hint = "TypedCollidingFrontsImageFilterPROXY"
    elif "itkCoLexicographicCompare" in typehint:
        hint = "TypedCoLexicographicComparePROXY"
    elif "itkCoefficientVectorSizeMismatch" in typehint:
        hint = "TypedCoefficientVectorSizeMismatchPROXY"
    elif "itkClosingByReconstructionImageFilter" in typehint:
        hint = "TypedClosingByReconstructionImageFilterPROXY"
    elif "itkCleanQuadEdgeMeshFilter" in typehint:
        hint = "TypedCleanQuadEdgeMeshFilterPROXY"
    elif "itkClassifierBase" in typehint:
        hint = "TypedClassifierBasePROXY"
    elif "itkClampImageFilter" in typehint:
        hint = "TypedClampImageFilterPROXY"
    elif "itkClamp" in typehint:
        hint = "TypedClampPROXY"
    elif "itkChoiceMethod" in typehint:
        hint = "TypedChoiceMethodPROXY"
    elif "itkChiSquareDistribution" in typehint:
        hint = "TypedChiSquareDistributionPROXY"
    elif "itkCheckPolicy" in typehint:
        hint = "TypedCheckPolicyPROXY"
    elif "itkCheckerBoardImageFilter" in typehint:
        hint = "TypedCheckerBoardImageFilterPROXY"
    elif "itkChangeRegionLabelMapFilter" in typehint:
        hint = "TypedChangeRegionLabelMapFilterPROXY"
    elif "itkChangeLabelLabelMapFilter" in typehint:
        hint = "TypedChangeLabelLabelMapFilterPROXY"
    elif "itkChangeLabelImageFilter" in typehint:
        hint = "TypedChangeLabelImageFilterPROXY"
    elif "itkChangeLabel" in typehint:
        hint = "TypedChangeLabelPROXY"
    elif "itkChangeInformationImageFilter" in typehint:
        hint = "TypedChangeInformationImageFilterPROXY"
    elif "itkChainCodeToFourierSeriesPathFilter" in typehint:
        hint = "TypedChainCodeToFourierSeriesPathFilterPROXY"
    elif "itkChainCodePath2D" in typehint:
        hint = "TypedChainCodePath2DPROXY"
    elif "itkChainCodePath" in typehint:
        hint = "TypedChainCodePathPROXY"
    elif "itkCentroidLabelObjectAccessor" in typehint:
        hint = "TypedCentroidLabelObjectAccessorPROXY"
    elif "itkCentralDifferenceImageFunction" in typehint:
        hint = "TypedCentralDifferenceImageFunctionPROXY"
    elif "itkCenterOfGravityLabelObjectAccessor" in typehint:
        hint = "TypedCenterOfGravityLabelObjectAccessorPROXY"
    elif "itkCenteredVersorTransformInitializer" in typehint:
        hint = "TypedCenteredVersorTransformInitializerPROXY"
    elif "itkCenteredTransformInitializer" in typehint:
        hint = "TypedCenteredTransformInitializerPROXY"
    elif "itkCenteredSimilarity2DTransform" in typehint:
        hint = "TypedCenteredSimilarity2DTransformPROXY"
    elif "itkCenteredRigid2DTransform" in typehint:
        hint = "TypedCenteredRigid2DTransformPROXY"
    elif "itkCenteredEuler3DTransform" in typehint:
        hint = "TypedCenteredEuler3DTransformPROXY"
    elif "itkCenteredAffineTransform" in typehint:
        hint = "TypedCenteredAffineTransformPROXY"
    elif "itkCellTraitsInfo" in typehint:
        hint = "TypedCellTraitsInfoPROXY"
    elif "itkCellInterfaceVisitorImplementation" in typehint:
        hint = "TypedCellInterfaceVisitorImplementationPROXY"
    elif "itkCellInterfaceVisitor" in typehint:
        hint = "TypedCellInterfaceVisitorPROXY"
    elif "itkCellInterface" in typehint:
        hint = "TypedCellInterfacePROXY"
    elif "itkCellGeometry" in typehint:
        hint = "TypedCellGeometryPROXY"
    elif "itkCastSpatialObjectFilter" in typehint:
        hint = "TypedCastSpatialObjectFilterPROXY"
    elif "itkCastImageFilter" in typehint:
        hint = "TypedCastImageFilterPROXY"
    elif "itkCannySegmentationLevelSetImageFilter" in typehint:
        hint = "TypedCannySegmentationLevelSetImageFilterPROXY"
    elif "itkCannySegmentationLevelSetFunction" in typehint:
        hint = "TypedCannySegmentationLevelSetFunctionPROXY"
    elif "itkCannyEdgeDetectionImageFilter" in typehint:
        hint = "TypedCannyEdgeDetectionImageFilterPROXY"
    elif "itkCandidateVector" in typehint:
        hint = "TypedCandidateVectorPROXY"
    elif "itkCandidate" in typehint:
        hint = "TypedCandidatePROXY"
    elif "itkop" in typehint:
        hint = "TypedopPROXY"
    elif "itkCanBeMultiplied" in typehint:
        hint = "TypedCanBeMultipliedPROXY"
    elif "itkop" in typehint:
        hint = "TypedopPROXY"
    elif "itkCanBeDivided" in typehint:
        hint = "TypedCanBeDividedPROXY"
    elif "itkop" in typehint:
        hint = "TypedopPROXY"
    elif "itkCanBeAddedOrSubtracted" in typehint:
        hint = "TypedCanBeAddedOrSubtractedPROXY"
    elif "itkCallbackTypeProxy" in typehint:
        hint = "TypedCallbackTypeProxyPROXY"
    elif "itkCalculateOutputWrapOffsetModifiers" in typehint:
        hint = "TypedCalculateOutputWrapOffsetModifiersPROXY"
    elif "itkBYUMeshIOFactory" in typehint:
        hint = "TypedBYUMeshIOFactoryPROXY"
    elif "itkBYUMeshIO" in typehint:
        hint = "TypedBYUMeshIOPROXY"
    elif "itkByteSwapper" in typehint:
        hint = "TypedByteSwapperPROXY"
    elif "itkBuildInformation" in typehint:
        hint = "TypedBuildInformationPROXY"
    elif "itkBufferedImageNeighborhoodPixelAccessPolicy" in typehint:
        hint = "TypedBufferedImageNeighborhoodPixelAccessPolicyPROXY"
    elif "itkBSplineUpsampleImageFilter" in typehint:
        hint = "TypedBSplineUpsampleImageFilterPROXY"
    elif "itkBSplineTransformParametersAdaptor" in typehint:
        hint = "TypedBSplineTransformParametersAdaptorPROXY"
    elif "itkBSplineTransformInitializer" in typehint:
        hint = "TypedBSplineTransformInitializerPROXY"
    elif "itkBSplineTransform" in typehint:
        hint = "TypedBSplineTransformPROXY"
    elif "itkBSplineSyNImageRegistrationMethod" in typehint:
        hint = "TypedBSplineSyNImageRegistrationMethodPROXY"
    elif "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor" in typehint:
        hint = "TypedBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptorPROXY"
    elif "itkBSplineSmoothingOnUpdateDisplacementFieldTransform" in typehint:
        hint = "TypedBSplineSmoothingOnUpdateDisplacementFieldTransformPROXY"
    elif "itkBSplineScatteredDataPointSetToImageFilter" in typehint:
        hint = "TypedBSplineScatteredDataPointSetToImageFilterPROXY"
    elif "itkBSplineResampleImageFunction" in typehint:
        hint = "TypedBSplineResampleImageFunctionPROXY"
    elif "itkBSplineResampleImageFilterBase" in typehint:
        hint = "TypedBSplineResampleImageFilterBasePROXY"
    elif "itkBSplineL2ResampleImageFilterBase" in typehint:
        hint = "TypedBSplineL2ResampleImageFilterBasePROXY"
    elif "itkBSplineKernelFunction" in typehint:
        hint = "TypedBSplineKernelFunctionPROXY"
    elif "itkBSplineInterpolationWeightFunction" in typehint:
        hint = "TypedBSplineInterpolationWeightFunctionPROXY"
    elif "itkBSplineInterpolateImageFunction" in typehint:
        hint = "TypedBSplineInterpolateImageFunctionPROXY"
    elif "itkBSplineExponentialDiffeomorphicTransformParametersAdaptor" in typehint:
        hint = "TypedBSplineExponentialDiffeomorphicTransformParametersAdaptorPROXY"
    elif "itkBSplineExponentialDiffeomorphicTransform" in typehint:
        hint = "TypedBSplineExponentialDiffeomorphicTransformPROXY"
    elif "itkBSplineDownsampleImageFilter" in typehint:
        hint = "TypedBSplineDownsampleImageFilterPROXY"
    elif "itkBSplineDerivativeKernelFunction" in typehint:
        hint = "TypedBSplineDerivativeKernelFunctionPROXY"
    elif "itkBSplineDeformableTransform" in typehint:
        hint = "TypedBSplineDeformableTransformPROXY"
    elif "itkBSplineDecompositionImageFilter" in typehint:
        hint = "TypedBSplineDecompositionImageFilterPROXY"
    elif "itkBSplineControlPointImageFunction" in typehint:
        hint = "TypedBSplineControlPointImageFunctionPROXY"
    elif "itkBSplineControlPointImageFilter" in typehint:
        hint = "TypedBSplineControlPointImageFilterPROXY"
    elif "itkBSplineCenteredResampleImageFilterBase" in typehint:
        hint = "TypedBSplineCenteredResampleImageFilterBasePROXY"
    elif "itkBSplineCenteredL2ResampleImageFilterBase" in typehint:
        hint = "TypedBSplineCenteredL2ResampleImageFilterBasePROXY"
    elif "itkBSplineBaseTransform" in typehint:
        hint = "TypedBSplineBaseTransformPROXY"
    elif "itkBruker2dseqImageIOFactory" in typehint:
        hint = "TypedBruker2dseqImageIOFactoryPROXY"
    elif "itkBruker2dseqImageIO" in typehint:
        hint = "TypedBruker2dseqImageIOPROXY"
    elif "itkBresenhamLine" in typehint:
        hint = "TypedBresenhamLinePROXY"
    elif "itkBracketOperator" in typehint:
        hint = "TypedBracketOperatorPROXY"
    elif "itkBoxSpatialObject" in typehint:
        hint = "TypedBoxSpatialObjectPROXY"
    elif "itkBoxSigmaImageFilter" in typehint:
        hint = "TypedBoxSigmaImageFilterPROXY"
    elif "itkBoxMeanImageFilter" in typehint:
        hint = "TypedBoxMeanImageFilterPROXY"
    elif "itkBoxImageFilter" in typehint:
        hint = "TypedBoxImageFilterPROXY"
    elif "itkBoundingBoxLabelObjectAccessor" in typehint:
        hint = "TypedBoundingBoxLabelObjectAccessorPROXY"
    elif "itkBoundingBox" in typehint:
        hint = "TypedBoundingBoxPROXY"
    elif "itkBoundedReciprocalImageFilter" in typehint:
        hint = "TypedBoundedReciprocalImageFilterPROXY"
    elif "itkBoundedReciprocal" in typehint:
        hint = "TypedBoundedReciprocalPROXY"
    elif "itkBoundaryResolver" in typehint:
        hint = "TypedBoundaryResolverPROXY"
    elif "itkBoundaryAssignmentIdentifier" in typehint:
        hint = "TypedBoundaryAssignmentIdentifierPROXY"
    elif "itkBoundary" in typehint:
        hint = "TypedBoundaryPROXY"
    elif "itkBorderTransform" in typehint:
        hint = "TypedBorderTransformPROXY"
    elif "itkBorderQuadEdgeMeshFilterEnums" in typehint:
        hint = "TypedBorderQuadEdgeMeshFilterEnumsPROXY"
    elif "itkBorderQuadEdgeMeshFilter" in typehint:
        hint = "TypedBorderQuadEdgeMeshFilterPROXY"
    elif "itkBorderPick" in typehint:
        hint = "TypedBorderPickPROXY"
    elif "itkBooleanDispatch" in typehint:
        hint = "TypedBooleanDispatchPROXY"
    elif "itkBMPImageIOFactory" in typehint:
        hint = "TypedBMPImageIOFactoryPROXY"
    elif "itkBMPImageIO" in typehint:
        hint = "TypedBMPImageIOPROXY"
    elif "itkBluePixelAccessor" in typehint:
        hint = "TypedBluePixelAccessorPROXY"
    elif "itkBlueColormapFunction" in typehint:
        hint = "TypedBlueColormapFunctionPROXY"
    elif "itkBlockMatchingImageFilter" in typehint:
        hint = "TypedBlockMatchingImageFilterPROXY"
    elif "itkBlobSpatialObject" in typehint:
        hint = "TypedBlobSpatialObjectPROXY"
    elif "itkBlackTopHatImageFilter" in typehint:
        hint = "TypedBlackTopHatImageFilterPROXY"
    elif "itkBlackmanWindowFunction" in typehint:
        hint = "TypedBlackmanWindowFunctionPROXY"
    elif "itkBitwiseOperators" in typehint:
        hint = "TypedBitwiseOperatorsPROXY"
    elif "itkBitwiseNot" in typehint:
        hint = "TypedBitwiseNotPROXY"
    elif "itkBioRadImageIOFactory" in typehint:
        hint = "TypedBioRadImageIOFactoryPROXY"
    elif "itkBioRadImageIO" in typehint:
        hint = "TypedBioRadImageIOPROXY"
    elif "itkBinShrinkImageFilter" in typehint:
        hint = "TypedBinShrinkImageFilterPROXY"
    elif "itkBinomialBlurImageFilter" in typehint:
        hint = "TypedBinomialBlurImageFilterPROXY"
    elif "itkBinaryUnsignedIntDispatch" in typehint:
        hint = "TypedBinaryUnsignedIntDispatchPROXY"
    elif "itkBinaryThresholdSpatialFunction" in typehint:
        hint = "TypedBinaryThresholdSpatialFunctionPROXY"
    elif "itkBinaryThresholdProjectionImageFilter" in typehint:
        hint = "TypedBinaryThresholdProjectionImageFilterPROXY"
    elif "itkBinaryThresholdImageFunction" in typehint:
        hint = "TypedBinaryThresholdImageFunctionPROXY"
    elif "itkBinaryThresholdImageFilter" in typehint:
        hint = "TypedBinaryThresholdImageFilterPROXY"
    elif "itkBinaryThresholdAccumulator" in typehint:
        hint = "TypedBinaryThresholdAccumulatorPROXY"
    elif "itkBinaryThreshold" in typehint:
        hint = "TypedBinaryThresholdPROXY"
    elif "itkBinaryThinningImageFilter" in typehint:
        hint = "TypedBinaryThinningImageFilterPROXY"
    elif "itkBinaryStatisticsOpeningImageFilter" in typehint:
        hint = "TypedBinaryStatisticsOpeningImageFilterPROXY"
    elif "itkBinaryStatisticsKeepNObjectsImageFilter" in typehint:
        hint = "TypedBinaryStatisticsKeepNObjectsImageFilterPROXY"
    elif "itkBinaryShapeOpeningImageFilter" in typehint:
        hint = "TypedBinaryShapeOpeningImageFilterPROXY"
    elif "itkBinaryShapeKeepNObjectsImageFilter" in typehint:
        hint = "TypedBinaryShapeKeepNObjectsImageFilterPROXY"
    elif "itkBinaryReconstructionLabelMapFilter" in typehint:
        hint = "TypedBinaryReconstructionLabelMapFilterPROXY"
    elif "itkBinaryReconstructionByErosionImageFilter" in typehint:
        hint = "TypedBinaryReconstructionByErosionImageFilterPROXY"
    elif "itkBinaryReconstructionByDilationImageFilter" in typehint:
        hint = "TypedBinaryReconstructionByDilationImageFilterPROXY"
    elif "itkBinaryPruningImageFilter" in typehint:
        hint = "TypedBinaryPruningImageFilterPROXY"
    elif "itkBinaryProjectionImageFilter" in typehint:
        hint = "TypedBinaryProjectionImageFilterPROXY"
    elif "itkBinaryOpeningByReconstructionImageFilter" in typehint:
        hint = "TypedBinaryOpeningByReconstructionImageFilterPROXY"
    elif "itkBinaryNotImageFilter" in typehint:
        hint = "TypedBinaryNotImageFilterPROXY"
    elif "itkBinaryNot" in typehint:
        hint = "TypedBinaryNotPROXY"
    elif "itkBinaryMorphologyImageFilter" in typehint:
        hint = "TypedBinaryMorphologyImageFilterPROXY"
    elif "itkBinaryMorphologicalOpeningImageFilter" in typehint:
        hint = "TypedBinaryMorphologicalOpeningImageFilterPROXY"
    elif "itkBinaryMorphologicalClosingImageFilter" in typehint:
        hint = "TypedBinaryMorphologicalClosingImageFilterPROXY"
    elif "itkBinaryMinMaxCurvatureFlowImageFilter" in typehint:
        hint = "TypedBinaryMinMaxCurvatureFlowImageFilterPROXY"
    elif "itkBinaryMinMaxCurvatureFlowFunction" in typehint:
        hint = "TypedBinaryMinMaxCurvatureFlowFunctionPROXY"
    elif "itkBinaryMedianImageFilter" in typehint:
        hint = "TypedBinaryMedianImageFilterPROXY"
    elif "itkBinaryMaskToNarrowBandPointSetFilter" in typehint:
        hint = "TypedBinaryMaskToNarrowBandPointSetFilterPROXY"
    elif "itkBinaryMask3DMeshSource" in typehint:
        hint = "TypedBinaryMask3DMeshSourcePROXY"
    elif "itkBinaryMagnitudeImageFilter" in typehint:
        hint = "TypedBinaryMagnitudeImageFilterPROXY"
    elif "itkBinaryIntDispatch" in typehint:
        hint = "TypedBinaryIntDispatchPROXY"
    elif "itkBinaryImageToStatisticsLabelMapFilter" in typehint:
        hint = "TypedBinaryImageToStatisticsLabelMapFilterPROXY"
    elif "itkBinaryImageToSparseLevelSetImageAdaptorBase" in typehint:
        hint = "TypedBinaryImageToSparseLevelSetImageAdaptorBasePROXY"
    elif "itkBinaryImageToShapeLabelMapFilter" in typehint:
        hint = "TypedBinaryImageToShapeLabelMapFilterPROXY"
    elif "itkBinaryImageToLevelSetImageAdator" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdatorPROXY"
    elif "itkBinaryImageToLevelSetImageAdaptorBase" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorBasePROXY"
    elif "itkBinaryImageToLevelSetImageAdaptor" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorPROXY"
    elif "itkBinaryImageToLevelSetImageAdaptor" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorPROXY"
    elif "itkBinaryImageToLevelSetImageAdaptor" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorPROXY"
    elif "itkBinaryImageToLevelSetImageAdaptor" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorPROXY"
    elif "itkBinaryImageToLevelSetImageAdaptor" in typehint:
        hint = "TypedBinaryImageToLevelSetImageAdaptorPROXY"
    elif "itkBinaryImageToLabelMapFilter" in typehint:
        hint = "TypedBinaryImageToLabelMapFilterPROXY"
    elif "itkBinaryGrindPeakImageFilter" in typehint:
        hint = "TypedBinaryGrindPeakImageFilterPROXY"
    elif "itkBinaryGeneratorImageFilter" in typehint:
        hint = "TypedBinaryGeneratorImageFilterPROXY"
    elif "itkBinaryFunctorImageFilter" in typehint:
        hint = "TypedBinaryFunctorImageFilterPROXY"
    elif "itkBinaryFillholeImageFilter" in typehint:
        hint = "TypedBinaryFillholeImageFilterPROXY"
    elif "itkBinaryErodeImageFilter" in typehint:
        hint = "TypedBinaryErodeImageFilterPROXY"
    elif "itkBinaryDilateImageFilter" in typehint:
        hint = "TypedBinaryDilateImageFilterPROXY"
    elif "itkBinaryCrossStructuringElement" in typehint:
        hint = "TypedBinaryCrossStructuringElementPROXY"
    elif "itkBinaryContourImageFilter" in typehint:
        hint = "TypedBinaryContourImageFilterPROXY"
    elif "itkBinaryClosingByReconstructionImageFilter" in typehint:
        hint = "TypedBinaryClosingByReconstructionImageFilterPROXY"
    elif "itkBinaryBooleanDispatch" in typehint:
        hint = "TypedBinaryBooleanDispatchPROXY"
    elif "itkBinaryBallStructuringElement" in typehint:
        hint = "TypedBinaryBallStructuringElementPROXY"
    elif "itkBinaryAccumulator" in typehint:
        hint = "TypedBinaryAccumulatorPROXY"
    elif "itkBilateralImageFilter" in typehint:
        hint = "TypedBilateralImageFilterPROXY"
    elif "itkBayesianClassifierInitializationImageFilter" in typehint:
        hint = "TypedBayesianClassifierInitializationImageFilterPROXY"
    elif "itkBayesianClassifierImageFilter" in typehint:
        hint = "TypedBayesianClassifierImageFilterPROXY"
    elif "itkBasicErodeImageFilter" in typehint:
        hint = "TypedBasicErodeImageFilterPROXY"
    elif "itkBasicDilateImageFilter" in typehint:
        hint = "TypedBasicDilateImageFilterPROXY"
    elif "itkBarycentricCombination" in typehint:
        hint = "TypedBarycentricCombinationPROXY"
    elif "itkBandNode" in typehint:
        hint = "TypedBandNodePROXY"
    elif "itkBackwardDifferenceOperator" in typehint:
        hint = "TypedBackwardDifferenceOperatorPROXY"
    elif "itkAzimuthElevationToCartesianTransform" in typehint:
        hint = "TypedAzimuthElevationToCartesianTransformPROXY"
    elif "itkAxisNodeType" in typehint:
        hint = "TypedAxisNodeTypePROXY"
    elif "itkAuxVarTypeDefault" in typehint:
        hint = "TypedAuxVarTypeDefaultPROXY"
    elif "itkAutumnColormapFunction" in typehint:
        hint = "TypedAutumnColormapFunctionPROXY"
    elif "itkAutoPointerDataObjectDecorator" in typehint:
        hint = "TypedAutoPointerDataObjectDecoratorPROXY"
    elif "itkAutoPointer" in typehint:
        hint = "TypedAutoPointerPROXY"
    elif "itkAutomaticTopologyMeshSource" in typehint:
        hint = "TypedAutomaticTopologyMeshSourcePROXY"
    elif "itkAutoCropLabelMapFilter" in typehint:
        hint = "TypedAutoCropLabelMapFilterPROXY"
    elif "itkAuthalicMatrixCoefficients" in typehint:
        hint = "TypedAuthalicMatrixCoefficientsPROXY"
    elif "itkAttributeUniqueLabelMapFilter" in typehint:
        hint = "TypedAttributeUniqueLabelMapFilterPROXY"
    elif "itkAttributeSelectionLabelMapFilter" in typehint:
        hint = "TypedAttributeSelectionLabelMapFilterPROXY"
    elif "itkAttributeRelabelLabelMapFilter" in typehint:
        hint = "TypedAttributeRelabelLabelMapFilterPROXY"
    elif "itkAttributePositionLabelMapFilter" in typehint:
        hint = "TypedAttributePositionLabelMapFilterPROXY"
    elif "itkAttributeOpeningLabelMapFilter" in typehint:
        hint = "TypedAttributeOpeningLabelMapFilterPROXY"
    elif "itkAttributeMorphologyBaseImageFilter" in typehint:
        hint = "TypedAttributeMorphologyBaseImageFilterPROXY"
    elif "itkAttributeLabelObjectAccessor" in typehint:
        hint = "TypedAttributeLabelObjectAccessorPROXY"
    elif "itkAttributeLabelObject" in typehint:
        hint = "TypedAttributeLabelObjectPROXY"
    elif "itkAttributeKeepNObjectsLabelMapFilter" in typehint:
        hint = "TypedAttributeKeepNObjectsLabelMapFilterPROXY"
    elif "itkAtomicPixel" in typehint:
        hint = "TypedAtomicPixelPROXY"
    elif "itkAtanRegularizedHeavisideStepFunction" in typehint:
        hint = "TypedAtanRegularizedHeavisideStepFunctionPROXY"
    elif "itkAtanPixelAccessor" in typehint:
        hint = "TypedAtanPixelAccessorPROXY"
    elif "itkAtanImageFilter" in typehint:
        hint = "TypedAtanImageFilterPROXY"
    elif "itkAtanImageAdaptor" in typehint:
        hint = "TypedAtanImageAdaptorPROXY"
    elif "itkAtan2ImageFilter" in typehint:
        hint = "TypedAtan2ImageFilterPROXY"
    elif "itkAtan2" in typehint:
        hint = "TypedAtan2PROXY"
    elif "itkAtan" in typehint:
        hint = "TypedAtanPROXY"
    elif "itkAssignable" in typehint:
        hint = "TypedAssignablePROXY"
    elif "itkAsinPixelAccessor" in typehint:
        hint = "TypedAsinPixelAccessorPROXY"
    elif "itkAsinImageFilter" in typehint:
        hint = "TypedAsinImageFilterPROXY"
    elif "itkAsinImageAdaptor" in typehint:
        hint = "TypedAsinImageAdaptorPROXY"
    elif "itkAsin" in typehint:
        hint = "TypedAsinPROXY"
    elif "itkArrowSpatialObject" in typehint:
        hint = "TypedArrowSpatialObjectPROXY"
    elif "itkArrayCallback" in typehint:
        hint = "TypedArrayCallbackPROXY"
    elif "itkArray2D" in typehint:
        hint = "TypedArray2DPROXY"
    elif "itkArray" in typehint:
        hint = "TypedArrayPROXY"
    elif "itkAreaOpeningImageFilter" in typehint:
        hint = "TypedAreaOpeningImageFilterPROXY"
    elif "itkAreaClosingImageFilter" in typehint:
        hint = "TypedAreaClosingImageFilterPROXY"
    elif "itkArea" in typehint:
        hint = "TypedAreaPROXY"
    elif "itkArchetypeSeriesFileNames" in typehint:
        hint = "TypedArchetypeSeriesFileNamesPROXY"
    elif "itkApproximateSignedDistanceMapImageFilter" in typehint:
        hint = "TypedApproximateSignedDistanceMapImageFilterPROXY"
    elif "itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader" in typehint:
        hint = "TypedANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreaderPROXY"
    elif "itkANTSNeighborhoodCorrelationImageToImageMetricv4" in typehint:
        hint = "TypedANTSNeighborhoodCorrelationImageToImageMetricv4PROXY"
    elif "itkAntiAliasBinaryImageFilter" in typehint:
        hint = "TypedAntiAliasBinaryImageFilterPROXY"
    elif "itkAnnulusOperator" in typehint:
        hint = "TypedAnnulusOperatorPROXY"
    elif "itkAnisotropicFourthOrderLevelSetImageFilter" in typehint:
        hint = "TypedAnisotropicFourthOrderLevelSetImageFilterPROXY"
    elif "itkAnisotropicDiffusionImageFilter" in typehint:
        hint = "TypedAnisotropicDiffusionImageFilterPROXY"
    elif "itkAnisotropicDiffusionFunction" in typehint:
        hint = "TypedAnisotropicDiffusionFunctionPROXY"
    elif "itkAndImageFilter" in typehint:
        hint = "TypedAndImageFilterPROXY"
    elif "itkAndC" in typehint:
        hint = "TypedAndCPROXY"
    elif "itkAnd" in typehint:
        hint = "TypedAndPROXY"
    elif "itkAND" in typehint:
        hint = "TypedANDPROXY"
    elif "itkAnchorUtilities" in typehint:
        hint = "TypedAnchorUtilitiesPROXY"
    elif "itkAnchorOpenImageFilter" in typehint:
        hint = "TypedAnchorOpenImageFilterPROXY"
    elif "itkAnchorOpenCloseLine" in typehint:
        hint = "TypedAnchorOpenCloseLinePROXY"
    elif "itkAnchorOpenCloseImageFilter" in typehint:
        hint = "TypedAnchorOpenCloseImageFilterPROXY"
    elif "itkAnchorErodeImageFilter" in typehint:
        hint = "TypedAnchorErodeImageFilterPROXY"
    elif "itkAnchorErodeDilateLine" in typehint:
        hint = "TypedAnchorErodeDilateLinePROXY"
    elif "itkAnchorErodeDilateImageFilter" in typehint:
        hint = "TypedAnchorErodeDilateImageFilterPROXY"
    elif "itkAnchorDilateImageFilter" in typehint:
        hint = "TypedAnchorDilateImageFilterPROXY"
    elif "itkAnchorCloseImageFilter" in typehint:
        hint = "TypedAnchorCloseImageFilterPROXY"
    elif "itkAnalyze75Flavor" in typehint:
        hint = "TypedAnalyze75FlavorPROXY"
    elif "itkAmoebaOptimizerv4" in typehint:
        hint = "TypedAmoebaOptimizerv4PROXY"
    elif "itkAmoebaOptimizer" in typehint:
        hint = "TypedAmoebaOptimizerPROXY"
    elif "itkAlwaysReallocate" in typehint:
        hint = "TypedAlwaysReallocatePROXY"
    elif "itkAlmostEqualsUnsignedVsSigned" in typehint:
        hint = "TypedAlmostEqualsUnsignedVsSignedPROXY"
    elif "itkAlmostEqualsSignedVsUnsigned" in typehint:
        hint = "TypedAlmostEqualsSignedVsUnsignedPROXY"
    elif "itkAlmostEqualsPlainOldEquals" in typehint:
        hint = "TypedAlmostEqualsPlainOldEqualsPROXY"
    elif "itkAlmostEqualsIntegerVsFloat" in typehint:
        hint = "TypedAlmostEqualsIntegerVsFloatPROXY"
    elif "itkAlmostEqualsFunctionSelector" in typehint:
        hint = "TypedAlmostEqualsFunctionSelectorPROXY"
    elif "itkAlmostEqualsFloatVsInteger" in typehint:
        hint = "TypedAlmostEqualsFloatVsIntegerPROXY"
    elif "itkAlmostEqualsFloatVsFloat" in typehint:
        hint = "TypedAlmostEqualsFloatVsFloatPROXY"
    elif "itkAllocateRootPolicy" in typehint:
        hint = "TypedAllocateRootPolicyPROXY"
    elif "itkAlgorithmType" in typehint:
        hint = "TypedAlgorithmTypePROXY"
    elif "itkAggregateLabelMapFilter" in typehint:
        hint = "TypedAggregateLabelMapFilterPROXY"
    elif "itkAffineTransform" in typehint:
        hint = "TypedAffineTransformPROXY"
    elif "itkAddPixelAccessor" in typehint:
        hint = "TypedAddPixelAccessorPROXY"
    elif "itkAdditiveOperators" in typehint:
        hint = "TypedAdditiveOperatorsPROXY"
    elif "itkAdditiveGaussianNoiseImageFilter" in typehint:
        hint = "TypedAdditiveGaussianNoiseImageFilterPROXY"
    elif "itkAdditiveAndAssignOperators" in typehint:
        hint = "TypedAdditiveAndAssignOperatorsPROXY"
    elif "itkAddImageFilter" in typehint:
        hint = "TypedAddImageFilterPROXY"
    elif "itkAddImageAdaptor" in typehint:
        hint = "TypedAddImageAdaptorPROXY"
    elif "itkAdd3" in typehint:
        hint = "TypedAdd3PROXY"
    elif "itkAdd2" in typehint:
        hint = "TypedAdd2PROXY"
    elif "itkAdd1" in typehint:
        hint = "TypedAdd1PROXY"
    elif "itkAdaptiveHistogramEqualizationImageFilter" in typehint:
        hint = "TypedAdaptiveHistogramEqualizationImageFilterPROXY"
    elif "itkAdaptiveEqualizationHistogram" in typehint:
        hint = "TypedAdaptiveEqualizationHistogramPROXY"
    elif "itkAdaptImageFilter" in typehint:
        hint = "TypedAdaptImageFilterPROXY"
    elif "itkAcosPixelAccessor" in typehint:
        hint = "TypedAcosPixelAccessorPROXY"
    elif "itkAcosImageFilter" in typehint:
        hint = "TypedAcosImageFilterPROXY"
    elif "itkAcosImageAdaptor" in typehint:
        hint = "TypedAcosImageAdaptorPROXY"
    elif "itkAcos" in typehint:
        hint = "TypedAcosPROXY"
    elif "itkAccumulateImageFilter" in typehint:
        hint = "TypedAccumulateImageFilterPROXY"
    elif "itkAccessorFunctorInitializer" in typehint:
        hint = "TypedAccessorFunctorInitializerPROXY"
    elif "itkAccessorFunctor" in typehint:
        hint = "TypedAccessorFunctorPROXY"
    elif "itkAbsPixelAccessor" in typehint:
        hint = "TypedAbsPixelAccessorPROXY"
    elif "itkAbsoluteValueDifferenceImageFilter" in typehint:
        hint = "TypedAbsoluteValueDifferenceImageFilterPROXY"
    elif "itkAbsoluteValueDifference2" in typehint:
        hint = "TypedAbsoluteValueDifference2PROXY"
    elif "itkAbsLessCompare" in typehint:
        hint = "TypedAbsLessComparePROXY"
    elif "itkAbsImageFilter" in typehint:
        hint = "TypedAbsImageFilterPROXY"
    elif "itkAbsImageAdaptor" in typehint:
        hint = "TypedAbsImageAdaptorPROXY"
    elif "itkAbs" in typehint:
        hint = "TypedAbsPROXY"
    # END AUTOGENERATED CODE

    elif typed:
        # In this case we do not have a conversion defined
        hint = CYAN + typehint + END  # color used to warn against blind copy

    else:
        # dont color hint if not typed (meaning it is used for non-code creation purposes)
        hint = typehint

    if not typed:
        hint = hint.replace("Typed", "", 1).replace("PROXY", "")

    return hint
