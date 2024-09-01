import sys
import ast
from _ast import AST  # Python internals I guess?
import argparse
import pathlib
import codecs
import json

arg_parser = argparse.ArgumentParser(description="Parse a Python program to AST.")
arg_parser.add_argument("--input-file", help="Read and parse input file.")
arg_parser.add_argument(
    "--stdin", action="store_true", help="Read and parse input from stdin."
)
arg_parser.add_argument(
    "--type-comments", action="store_true", help="Produce an AST with type comments."
)
args = arg_parser.parse_args()

if args.input_file is not None:
    source = pathlib.Path(args.input_file).read_text()
elif args.stdin:
    source = sys.stdin.read()
else:
    print(
        "Missing input parameter. Please specify one of --input-file or --stdin.",
        file=sys.stderr,
    )
    sys.exit(1)

# ----- Begin inline dependency -------------------------------------------------------------------
# https://github.com/YoloSwagTeam/ast2json

# Copyright (c) 2013, Laurent Peuch <cortex@worlddomination.be>
#
# All rights reserved.
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of the University of California, Berkeley nor the
#   names of its contributors may be used to endorse or promote products
#   derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

BUILTIN_PURE = (int, float, bool)
BUILTIN_BYTES = (bytearray, bytes)
BUILTIN_STR = str


def decode_str(value):
    return value


def decode_bytes(value):
    try:
        return value.decode("utf-8")
    except:
        return codecs.getencoder("hex_codec")(value)[0].decode("utf-8")


def ast2json(node):
    assert isinstance(node, AST)
    to_return = dict()
    to_return["_type"] = node.__class__.__name__
    for attr in dir(node):
        if attr.startswith("_") or attr == "n" or attr == "s":
            continue
        to_return[attr] = get_value(getattr(node, attr))
        to_return.pop("lineno", None)
        to_return.pop("end_lineno", None)
        to_return.pop("col_offset", None)
        to_return.pop("end_col_offset", None)
    return to_return


def get_value(attr_value):
    if attr_value is None:
        return attr_value
    if isinstance(attr_value, BUILTIN_PURE):
        return attr_value
    if isinstance(attr_value, BUILTIN_BYTES):
        return decode_bytes(attr_value)
    if isinstance(attr_value, BUILTIN_STR):
        return decode_str(attr_value)
    if isinstance(attr_value, complex):
        return str(attr_value)
    if isinstance(attr_value, list):
        return [get_value(x) for x in attr_value]
    if isinstance(attr_value, AST):
        return ast2json(attr_value)
    if isinstance(attr_value, type(Ellipsis)):
        return "..."
    else:
        raise Exception(
            "Unknown case for '%s' of type '%s'" % (attr_value, type(attr_value))
        )


# -------------------------------------------------------------------- End inline dependency ------


tree = ast.parse(
    source,
    filename=args.input_file or "stdin",
    mode="exec",
    type_comments=args.type_comments,
)
tree_json = ast2json(tree)
print(json.dumps(tree_json, indent=4))
