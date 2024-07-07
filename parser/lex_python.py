import io
import json
import sys
import tokenize
import argparse
from typing import TypedDict

class Token(TypedDict):
    kind: str
    value: str
    start: tuple[int, int]
    end: tuple[int, int]


arg_parser = argparse.ArgumentParser(
    description="Tokenize a Python program."
)
arg_parser.add_argument("--input-file", help="Read and tokenize input file.")
arg_parser.add_argument("--stdin", action="store_true", help="Read and tokenize input from stdin.")
arg_parser.add_argument("--output-format", choices=["compact", "json"], default="compact", help="default = compact")
args = arg_parser.parse_args()

if args.input_file is not None:
    with tokenize.open(args.input_file) as file:
        tokens = list(tokenize.generate_tokens(file.readline))
elif args.stdin:
    tokens = list(tokenize.generate_tokens(sys.stdin.readline))
else:
    print("Missing input parameter. Please specify one of --input-file or --stdin.", file=sys.stderr)
    sys.exit(1)

def unicode_escape(input: str) -> str:
    encoded = input.encode("unicode_escape")
    escaped = encoded.decode("utf-8")
    return escaped

if args.output_format == "compact":
    output = "\n".join([
        f"{token.start[0]},{token.start[1]}-{token.end[0]},{token.end[1]} {tokenize.tok_name[token.type]} {unicode_escape(token.string)}"
        for token in tokens
    ])
    print(output)
elif args.output_format == "json":
    normalized_tokens: list[Token] = [
        {
            "kind": tokenize.tok_name[token.type],
            "value": token.string,
            # Python's tokenize appears to reserve the first line a special file encoding token.
            "start": token.start,
            "end": token.end,
        }
        for token in tokens
    ]
    print(json.dumps(normalized_tokens, indent=4))
else:
    print(f"Unrecognized output format: {args.output_format}.", file=sys.stderr)
    sys.exit(1)
