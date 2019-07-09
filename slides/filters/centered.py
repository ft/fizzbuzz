#!/usr/bin/env python

import pandocfilters as pf

def latex(s):
    return pf.RawBlock('latex', s)

def mk_centered(k, v, f, m):
    if k == "Para":
        value = pf.stringify(v)
        if value.startswith('[') and value.endswith(']'):
            content = value[1:-1]
            if content == "center":
                return latex(r'\begin{center}')
            elif content == "/center":
                return latex(r'\end{center}')

if __name__ == "__main__":
    pf.toJSONFilter(mk_centered)
