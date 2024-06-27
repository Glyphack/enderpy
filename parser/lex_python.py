import json
import sys
import tokenize
from typing import TypedDict

class Token(TypedDict):
    kind: str
    value: str
    start: tuple[int, int]
    end: tuple[int, int]


if len(sys.argv) < 2:
    raise Exception("Missing file path. Example: `python lex_python.py file.py`")

def unicode_escape(input: str) -> str:
    encoded = input.encode("unicode_escape")
    escaped = encoded.decode("utf-8")
    return escaped

with tokenize.open(sys.argv[1]) as file:
    tokens = list(tokenize.generate_tokens(file.readline))
    if "--compact" in sys.argv:
        output = "\n".join([
            f"{token.start[0]},{token.start[1]}-{token.end[0]},{token.end[1]} {tokenize.tok_name[token.type]} {unicode_escape(token.string)}"
            for token in tokens
        ])
        print(output)
    else:
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
