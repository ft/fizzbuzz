#!/usr/bin/env python

# Slide-Images filter for LaTeX Beamer output with pandoc

# When using images in LaTeX-beamer slides, I usually want one of these:
#
# 1) Centered plain image:
#
#     [simg:h=5cm|graph/some-pic.jpg]
#
# 2) Freely movable image using tikz:
#
#     [simg:h=5cm:(1cm,4cm)|graph/some-pic.jpg]
#
# 3) Freely movable image using tikz, and only on some slides:
#
#     [simg:h=5cm:(1cm,4cm):<2-4>|graph/some-pic.jpg]
#
# 4) Multiple freely movable image using tikz, and only on some slides:
#
#     [simg:h=5cm:(1cm,4cm):<1>|graph/some-pic.jpg +++
#           h=5cm:(1cm,4cm):<2>|graph/some-other-pic.jpg +++
#           h=5cm:(1cm,4cm):<3>|graph/final-pic.jpg]
#
# Previously, I was tempted to use yasnippet to deal with that. But pandoc's
# new filter feature seems to be the much cleaner approach.
#
#   Minimal: [simg|graph/structure.pdf]
#      Full: [simg:h=5cm:w=3cm:(4cm,3cm):<2-3>|graph/structure.pdf]
# Multiline: [simg:h=5cm:w=3cm:(4cm,3cm):<2-3>|graph/structure.pdf +++
#                  h=5cm:w=3cm:(4cm,3cm):<2-3>|graph/structure2.pdf]
#
# - <...> is a slide-range for \only<...>{...}
# - (...,...) are coordinates for the tikz overlay
# - h=... is a height parameter
# - w=... is a width parameter
#
# This needs the following snippet in the document's preamble:
#
#    \usepackage{tikz}
#    \usetikzlibrary{calc}


from __future__ import print_function
import sys
import re
import pandocfilters as pf

debugging = False

def debug(*objs):
    if debugging:
        print("DEBUG: ", *objs, file=sys.stderr)
    return debugging

def latex(s):
    debug(s)
    return pf.RawBlock('latex', s)

def got_something(lst, s):
    for item in lst[:]:
        if item.startswith(s):
            return item
    return False

def got_slide_range(lst):
    return got_something(lst, '<');

def got_coordinates(lst):
    return got_something(lst, '(');

def got_height(lst):
    h = got_something(lst, 'h=');
    if h: return 'height=' + h[2:]
    return False

def got_width(lst):
    w = got_something(lst, 'w=');
    if w: return 'width=' + w[2:]
    return False

def gen_includegraphics(fname, width, height):
    rv = '\includegraphics'
    if height or width:
        rv += '['
        if height: rv += height
        if height and width: rv += ','
        if width: rv += width
        rv += ']'
    rv += '{'
    rv += fname
    rv += '}'
    return rv

def gen_tikz_node(fname, width, height, coords, srange):
    rv = ''
    if srange:
        rv += '  \\onslide'
        rv += srange
        rv += '{%\n'
    rv += '  \\node at ($(current page.south west)+'
    rv += coords
    rv += '$)%\n  {'
    rv += gen_includegraphics(fname, width, height)
    rv += '};'
    if srange:
        rv += '}'
    rv += '%\n'
    return rv

def gen_tikz_head():
    rv = ''
    rv += '\\begin{tikzpicture}[overlay,remember picture,anchor=south west]%\n'
    rv += '  every node/.style={anchor=south west}%\n'
    return rv

def gen_tikz_foot():
    return '\\end{tikzpicture}%'

def gen_tikz_block(fname, width, height, coords, srange):
    rv = gen_tikz_head()
    rv += gen_tikz_node(fname, width, height, coords, srange)
    rv += gen_tikz_foot()
    return rv

def gen_centerimage(fname, width, height, srange):
    if srange:
        rv = '\\only'
        rv += srange
        rv += '{'
    else:
        rv = ''
    rv += r'\centerline{'
    rv += gen_includegraphics(fname, width, height)
    rv += r'}'
    if srange:
        rv += '}'
    return rv

def generate_image_code(images):
    if len(images) == 1:
        image = images[0]
        toks = image.split('|')
        fname = toks[1]
        opts = toks[0].split(':')
        height = got_height(opts)
        width = got_width(opts)
        coords = got_coordinates(opts)
        srange = got_slide_range(opts)
        if coords:
            return gen_tikz_block(fname, width, height, coords, srange)
        else:
            return gen_centerimage(fname, width, height, srange)
    else:
        rv = gen_tikz_head()
        for image in images:
            toks = image.split('|')
            fname = toks[1]
            opts = toks[0].split(':')
            height = got_height(opts)
            width = got_width(opts)
            coords = got_coordinates(opts)
            srange = got_slide_range(opts)
            rv += gen_tikz_node(fname, width, height, coords, srange)
        rv += gen_tikz_foot()
        return rv

def process_simg(k, v, f, m):
    if k == "Para":
        value = pf.stringify(v)
        if value.startswith('[simg') and value.endswith(']'):
            debug(value)
            images = re.split(r'\s+\+\+\+\s*', value[1:-1])
            return latex(generate_image_code(images))

if __name__ == "__main__":
    pf.toJSONFilter(process_simg)
