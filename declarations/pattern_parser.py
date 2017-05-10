# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Implementation details

"""

import re


class parser_t(object):

    """implementation details"""

    def __init__(
            self,
            pattern_char_begin,
            pattern_char_end,
            pattern_char_separator):
        self.__begin = pattern_char_begin
        self.__end = pattern_char_end
        self.__separator = pattern_char_separator
        # right now parser does not take into account next qualifiers, but it
        # will
        self.__text_qualifier = '"'
        self.__char_qualifier = "'"
        self.__escape = '\\'

    def has_pattern(self, decl_string):
        """
        Implementation detail

        """
        if self.__begin == "<":
            # Cleanup parentheses blocks before checking for the pattern
            # See also the args() method (in this file) for more explanations.
            decl_string = re.sub("\\s\\(.*?\\)", "", decl_string).strip()

        last_part = decl_string.split('::')[-1]
        return (
            decl_string.find(self.__begin) != -1 and
            last_part.find(self.__end) != -1
        )

    def name(self, decl_string):
        """implementation details"""
        if not self.has_pattern(decl_string):
            return decl_string
        args_begin = decl_string.find(self.__begin)
        return decl_string[0: args_begin].strip()

    def __find_args_separator(self, decl_string, start_pos):
        """implementation details"""
        bracket_depth = 0
        for index, ch in enumerate(decl_string[start_pos:]):
            if ch not in (self.__begin, self.__end, self.__separator):
                continue  # I am interested only in < and >
            elif self.__separator == ch:
                if not bracket_depth:
                    return index + start_pos
            elif self.__begin == ch:
                bracket_depth += 1
            elif not bracket_depth:
                return index + start_pos
            else:
                bracket_depth -= 1
        return -1

    def args(self, decl_string):
        """
        Extracts a list of arguments from the provided declaration string.

        Implementation detail. Example usages:
        Input: myClass<std::vector<int>, std::vector<double>>
        Output: [std::vector<int>, std::vector<double>]

        Args:
           decl_string (str): the full declaration string

        Returns:
            list: list of arguments as strings

        """
        args_begin = decl_string.find(self.__begin)
        args_end = decl_string.rfind(self.__end)
        if -1 in (args_begin, args_end) or args_begin == args_end:
            raise RuntimeError(
                "%s doesn't validate template instantiation string" %
                decl_string)

        args_only = decl_string[args_begin + 1: args_end].strip()

        # The list of arguments to be returned
        args = []

        parentheses_blocks = []
        prev_span = 0
        if self.__begin == "<":
            # In case where we are splitting template names, there
            # can be parentheses blocks (for arguments) that need to be taken
            # care of.

            # Build a regex matching a space (\s)
            # + something inside parentheses
            regex = re.compile("\\s\\(.*?\\)")
            for m in regex.finditer(args_only):
                # Store the position and the content
                parentheses_blocks.append([m.start() - prev_span, m.group()])
                prev_span = m.end() - m.start()
                # Cleanup the args_only string by removing the parentheses and
                # their content.
                args_only = args_only.replace(m.group(), "")

        # Now we are trying to split the args_only string in multiple arguments
        previous_found, found = 0, 0
        while True:
            found = self.__find_args_separator(args_only, previous_found)
            if found == -1:
                args.append(args_only[previous_found:].strip())
                # This is the last argument. Break out of the loop.
                break
            else:
                args.append(args_only[previous_found: found].strip())
            previous_found = found + 1  # skip found separator

        # Get the size and position for each argument
        absolute_pos_list = []
        absolute_pos = 0
        for arg in args:
            absolute_pos += len(arg)
            absolute_pos_list.append(absolute_pos)

        for item in parentheses_blocks:
            # In case where there are parentheses blocks we add them back
            # to the right argument
            parentheses_block_absolute_pos = item[0]
            parentheses_block_string = item[1]

            current_arg_absolute_pos = 0
            for arg_index, arg_absolute_pos in enumerate(absolute_pos_list):
                current_arg_absolute_pos += arg_absolute_pos
                if current_arg_absolute_pos >= parentheses_block_absolute_pos:
                    # Add the parentheses block back and break out of the loop.
                    args[arg_index] += parentheses_block_string
                    break

        return args

    NOT_FOUND = (-1, -1)
    """implementation details"""

    def find_args(self, text, start=None):
        """implementation details"""
        if start is None:
            start = 0
        first_occurance = text.find(self.__begin, start)
        if first_occurance == -1:
            return self.NOT_FOUND
        previous_found, found = first_occurance + 1, 0
        while True:
            found = self.__find_args_separator(text, previous_found)
            if found == -1:
                return self.NOT_FOUND
            elif text[found] == self.__end:
                return first_occurance, found
            else:
                previous_found = found + 1  # skip found sep

    def split(self, decl_string):
        """implementation details"""
        assert self.has_pattern(decl_string)
        return self.name(decl_string), self.args(decl_string)

    def split_recursive(self, decl_string):
        """implementation details"""
        assert self.has_pattern(decl_string)
        to_go = [decl_string]
        while to_go:
            name, args = self.split(to_go.pop())
            for arg in args:
                if self.has_pattern(arg):
                    to_go.append(arg)
            yield name, args

    def join(self, name, args, arg_separator=None):
        """implementation details"""
        if None is arg_separator:
            arg_separator = ', '
        args = [_f for _f in args if _f]

        if not args:
            args_str = ' '
        elif len(args) == 1:
            args_str = ' ' + args[0] + ' '
        else:
            args_str = ' ' + arg_separator.join(args) + ' '

        return ''.join([name, self.__begin, args_str, self.__end])

    def normalize(self, decl_string, arg_separator=None):
        """implementation details"""
        if not self.has_pattern(decl_string):
            return decl_string
        name, args = self.split(decl_string)
        for i, arg in enumerate(args):
            args[i] = self.normalize(arg)
        return self.join(name, args, arg_separator)
