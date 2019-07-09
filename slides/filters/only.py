#!/usr/bin/env python

import pandocfilters as pf

def latex(s):
    return pf.RawBlock('latex', s)

def mk_only(k, v, f, m):
    if k == "Para":
        value = pf.stringify(v)
        if value.startswith('[') and value.endswith(']'):
            content = value[1:-1]
            if content.startswith("only="):
                return latex(r'\only<%s>{' % content[5:])
            elif content == "/only":
                return latex(r'}')

if __name__ == "__main__":
    pf.toJSONFilter(mk_only)
